#!/bin/bash

# Check if a log file is provided as an argument
if [ -z "$1" ]; then
    echo "Usage: $0 <input_log_file>"
    exit 1
fi

INPUT_FILE="$1"

# --- Dynamic Output File Naming ---
# 1. Get the base file name (remove directory path)
BASE_NAME=$(basename "$INPUT_FILE")
# 2. Remove any existing extension
BASE_NAME_NO_EXT="${BASE_NAME%.*}"
# 3. Append the desired new extension
OUTPUT_FILE="${BASE_NAME_NO_EXT}.report.md"
# ----------------------------------

# Temporary files to store the summary and the table body
SUMMARY_TMP=$(mktemp)
TABLE_TMP=$(mktemp)

# Process the log file using AWK and redirect the table content to TABLE_TMP
# We use AWK to generate both the table and calculate the counts
awk '
BEGIN {
    FS=": "
    row_num = 0
    pvs_full_path = ""
    cert_file = ""
    num_cert_file = ""
    error_found = 0
    error_desc = ""
    tc_status = 1
    # Counters for the final summary
    total_error = 0
    total_success = 0
}

# --- Function to split the path and print the processed record ---
function print_record() {
    # Increment row number
    row_num++
    
    # Path Splitting Logic (Awk standard-compliant)
    path_copy = pvs_full_path
    pvs_file = path_copy
    if (sub(/^.*\/+/, "", pvs_file)) {
        pvs_location = pvs_full_path
        sub("/" pvs_file "$", "", pvs_location)
    } else {
        pvs_location = ""
    }

    # Determine the final result
    if (error_found == 1 || tc_status == 0) {
        RESULT = "**Error**"
        total_error++  # Increment error count
        if (error_desc == "") {
            error_desc = "Typechecking failed or process interrupted"
        }
    } else {
        RESULT = "Typechecked"
        total_success++ # Increment success count
        error_desc = ""
    }
    
    # Print the record as a Markdown table row to standard output (captured by TABLE_TMP)
    print "| " row_num " | " pvs_location " | " pvs_file " | " cert_file " | " num_cert_file " | " RESULT " | " error_desc " |"
}

# --- Start of a new record block ---
/^\.pvsFile: / {
    if (pvs_full_path != "") {
        print_record()
    }

    pvs_full_path = $2
    cert_file = ""
    num_cert_file = ""
    error_found = 0
    error_desc = ""
    tc_status = 1
    
    FS=": "
    next
}

# --- All other logic is unchanged (path extraction, error capturing) ---
/^pvsCertFile: / { cert_file = $2; next }
/^pvsNumCertFile: / { num_cert_file = $2; next }
/^File .* typechecked/ { next }

/^Typechecking failed for / {
    tc_status = 0
    if (error_found == 0) {
        FS=""
        error_desc = "Typechecking failed for " substr($0, index($0, "Typechecking failed for ") + length("Typechecking failed for "))
        gsub(/\n|\r|\s*$/, "", error_desc)
        FS=": "
    }
    next
}

/^\*\*\* Error: / {
    if (error_found == 0) {
        error_found = 1
        tc_status = 0
        FS=""
        error_desc = substr($0, index($0, "*** Error: ") + length("*** Error: "))
        sub(/(\s*Found:.*Expected:.*|\s*In file.*|\s*Typechecking failed for.*)/, "", error_desc)
        gsub(/^[ \t]+|[ \t]+$/, "", error_desc)
        
        if (length(error_desc) > 80) {
             error_desc = substr(error_desc, 1, 77) "..."
        }
        
        FS=": "
    }
    next
}

/BINDING-STACK-EXHAUSTED-ERROR/ || /\(\*.*ERROR\*\)/ {
    if (error_found == 0) {
        error_found = 1
        tc_status = 0
        error_desc = "Binding stack exhausted error (or similar critical process failure)"
    }
    next
}

# --- END block to handle the last record and print the summary counts ---
END {
    if (pvs_full_path != "") {
        print_record()
    }
    
    # Print the summary counts to standard error (which we redirect to SUMMARY_TMP)
    # Using specific tags so we can easily parse them in bash
    print "TOTAL_SUCCESS:" total_success > "/dev/stderr"
    print "TOTAL_ERROR:" total_error > "/dev/stderr"
}
' "$INPUT_FILE" 2> "$SUMMARY_TMP" > "$TABLE_TMP"

# --- Bash logic to extract counts and build the final file ---
TOTAL_SUCCESS=$(grep "TOTAL_SUCCESS:" "$SUMMARY_TMP" | cut -d: -f2)
TOTAL_ERROR=$(grep "TOTAL_ERROR:" "$SUMMARY_TMP" | cut -d: -f2)
TOTAL_COUNT=$((TOTAL_SUCCESS + TOTAL_ERROR))

# Build the final output file
{
    echo "# 📄 $INPUT_FILE"
    echo ""
    echo "---"
    echo "## 📊 Summary"
    echo ""
    echo "* **Total Files Processed:** **$TOTAL_COUNT**"
    echo "* **Total Errors:** **$TOTAL_ERROR** ❌"
    echo "* **Total Successful:** **$TOTAL_SUCCESS** ✅"
    echo ""
    echo "---"
    echo "## 📋 Detailed Report"
    echo ""
    echo "| # | Location | PVS File | PVS Cert File | PVS Num Cert File | Result | Error Description |"
    echo "| :---: | :--- | :--- | :--- | :--- | :--- | :--- |"
    cat "$TABLE_TMP"
} > "$OUTPUT_FILE"

# Clean up temporary files
rm "$SUMMARY_TMP" "$TABLE_TMP"

echo "✅ Report generated successfully: $OUTPUT_FILE"

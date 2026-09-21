#!/usr/bin/env bash

ABSPATH_SOURCE="$(realpath ${BASH_SOURCE[0]})"
CHECKER="$(dirname $ABSPATH_SOURCE)/checkErrorDifference.py"

typecheck=
check_certificates=no

while [ $# -gt 1 ]
do
    case $1 in
	--typecheck)
        typecheck=yes
	    ;;
	--check-certificates)
        check_certificates=yes
	    ;;
    esac
    shift
done

test=$1
test_folder=$(dirname $test)

name=$(jq -r '.name' $test)
pvsFile=$(jq -r '.pvsFile' $test)
inputFile=$(jq -r '.inputFile' $test)
stableError=$(jq '.stableError' $test)
unstableError=$(jq '.unstableError' $test)
extraFlags=$(jq -r '.flags // ""' $test)

# Extract baseline paths for PVS files (optional fields)
baselineRealFile=$(jq -r '.baselineRealFile // "null"' $test)
baselineCertFile=$(jq -r '.baselineCertFile // "null"' $test)
baselineNumCertFile=$(jq -r '.baselineNumCertFile // "null"' $test)

# Certificate checking requires --check-certificates flag AND at least one baseline path
if [[ "$check_certificates" == "yes" ]]; then
    if [[ "$baselineRealFile" == "null" && "$baselineCertFile" == "null" &&
          "$baselineNumCertFile" == "null" ]]; then
        # Silently skip - test has no baseline files
        check_certificates=no
    fi
fi

results="temp.$name.json"

# Determine certificate directory (where PRECiSA writes certificate files)
certDir=$(dirname $pvsFile)
basename=$(basename ${pvsFile%.*})

# Save current directory before cd
current_dir=$(pwd)

# Convert relative baseline paths to absolute (relative to test descriptor location)
if [[ "$check_certificates" == "yes" ]]; then
    # Make paths absolute from current directory
    [[ "$baselineRealFile" != "null" ]] && baselineRealFile="$current_dir/$test_folder/$baselineRealFile"
    [[ "$baselineCertFile" != "null" ]] && baselineCertFile="$current_dir/$test_folder/$baselineCertFile"
    [[ "$baselineNumCertFile" != "null" ]] && baselineNumCertFile="$current_dir/$test_folder/$baselineNumCertFile"

    # Make certDir absolute too
    certDir="$current_dir/$test_folder/$certDir"
fi

cd $test_folder

precisa --json $extraFlags $pvsFile $inputFile > $results

sutStableError=$(jq '.results.[0].stableError' $results)
sutUnstableError=$(jq '.results.[0].unstableError' $results)

if ! [ $stableError = "null" ]; then
    $CHECKER $stableError $sutStableError 2>/dev/null
    status1=$?
else
    echo "Test $name stableError is null. Skipping."
fi

if ! [ $unstableError = "null" ]; then
    $CHECKER $unstableError $sutUnstableError 2>/dev/null
    status2=$?
else
    status2=0
fi

if ! [[ $status1 -eq 0 && $status2 -eq 0 ]]; then
    echo "$name failed:"
    echo "  reference:"
    echo "$(jq . $test | sed 's/^/    /')"
    echo "  results:"
    echo "$(jq . $results | sed 's/^/    /')"
    exit 1;
fi

# Check generated PVS files against baseline if requested
if [[ "$check_certificates" == "yes" ]]; then
    # Check real-valued certificate
    if [[ "$baselineRealFile" != "null" ]]; then
        actualRealFile="$certDir/${basename}_real.pvs"
        if [ -f "$actualRealFile" ]; then
            if diff -q "$actualRealFile" "$baselineRealFile" > /dev/null 2>&1; then
                echo "✓ Real certificate matches baseline"
            else
                echo "✗ REAL CERTIFICATE DIFFERS FROM BASELINE"
                echo "  Expected: $baselineRealFile"
                echo "  Actual: $actualRealFile"
                diff "$actualRealFile" "$baselineRealFile" | head -20
                exit 1
            fi
        else
            echo "✗ Real certificate not generated: $actualRealFile"
            exit 1
        fi
    fi

    # Check symbolic certificate
    if [[ "$baselineCertFile" != "null" ]]; then
        actualCertFile="$certDir/${basename}_cert.pvs"
        if [ -f "$actualCertFile" ]; then
            if diff -q "$actualCertFile" "$baselineCertFile" > /dev/null 2>&1; then
                echo "✓ Symbolic certificate matches baseline"
            else
                echo "✗ SYMBOLIC CERTIFICATE DIFFERS FROM BASELINE"
                echo "  Expected: $baselineCertFile"
                echo "  Actual: $actualCertFile"
                diff "$actualCertFile" "$baselineCertFile" | head -20
                exit 1
            fi
        else
            echo "✗ Symbolic certificate not generated: $actualCertFile"
            exit 1
        fi
    fi

    # Check numerical certificate (with floating-point tolerance for cross-platform compatibility)
    if [[ "$baselineNumCertFile" != "null" ]]; then
        actualNumCertFile="$certDir/${basename}_num_cert.pvs"
        if [ -f "$actualNumCertFile" ]; then
            # Use tolerant comparison for numerical certificates (platform-dependent Kodiak output)
            CERT_COMPARATOR="$(dirname $ABSPATH_SOURCE)/compareCertificates.sh"
            if "$CERT_COMPARATOR" "$baselineNumCertFile" "$actualNumCertFile"; then
                echo "✓ Numerical certificate matches baseline (within tolerance)"
            else
                echo "✗ NUMERICAL CERTIFICATE DIFFERS FROM BASELINE"
                echo "  Expected: $baselineNumCertFile"
                echo "  Actual: $actualNumCertFile"
                echo "  (Numerical bounds differ beyond adaptive tolerance)"
                exit 1
            fi
        else
            echo "✗ Numerical certificate not generated: $actualNumCertFile"
            exit 1
        fi
    fi
fi

if [[ "$typecheck" == "yes" ]]; then
    cd $(dirname $pvsFile)

    pvsRealFile="$(basename ${pvsFile%.*})_real.pvs"
    pvsCertFile="$(basename ${pvsFile%.*})_cert.pvs"
    pvsNumCertFile="$(basename ${pvsFile%.*})_num_cert.pvs"
    echo "pvsFile: $pvsFile"
    echo "pvsRealFile: $pvsRealFile"
    echo "pvsCertFile: $pvsCertFile"
    echo "pvsNumCertFile: $pvsNumCertFile"

    for file in $(basename $pvsFile) $pvsRealFile $pvsCertFile $pvsNumCertFile; do
        if ! proveit -q -T $file; then
            echo "Typechecking failed for $file"
            exit 1
        fi
    done
fi
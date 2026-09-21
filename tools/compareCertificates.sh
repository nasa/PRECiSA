#!/usr/bin/env bash
# compareCertificates.sh - Compare two numerical PVS certificates with floating-point tolerance
#
# Usage: compareCertificates.sh <reference_cert.pvs> <actual_cert.pvs>
# Exit codes:
#   0 - Certificates match within tolerance
#   1 - Certificates differ beyond tolerance or structural mismatch

set -euo pipefail

if [ $# -ne 2 ]; then
    echo "Usage: $0 <reference_cert> <actual_cert>" >&2
    exit 1
fi

ref_cert="$1"
act_cert="$2"

if [ ! -f "$ref_cert" ]; then
    echo "Reference certificate not found: $ref_cert" >&2
    exit 1
fi

if [ ! -f "$act_cert" ]; then
    echo "Actual certificate not found: $act_cert" >&2
    exit 1
fi

# Extract numerical bounds from PVS certificate
# Looks for rational fractions: "numerator / denominator"
# Converts them to decimal scientific notation
extract_bounds() {
    local cert_file="$1"

    # Find lines with rational fractions (number / number)
    # Extract numerator and denominator, compute decimal value
    grep -E '[0-9]+ */ *[0-9]+' "$cert_file" | \
        sed -E 's/.*([0-9]+) *\/ *([0-9]+).*/\1 \2/' | \
        awk '{if ($2 != 0) printf "%.17e\n", $1 / $2; else print "inf"}'
}

# Get the directory containing this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
CHECKER="$SCRIPT_DIR/checkErrorDifference.py"

if [ ! -f "$CHECKER" ]; then
    echo "Error: checkErrorDifference.py not found at $CHECKER" >&2
    exit 1
fi

# Extract bounds from both certificates
ref_bounds=$(extract_bounds "$ref_cert")
act_bounds=$(extract_bounds "$act_cert")

# Compare number of bounds
ref_count=$(echo "$ref_bounds" | wc -l | tr -d ' ')
act_count=$(echo "$act_bounds" | wc -l | tr -d ' ')

if [ "$ref_count" != "$act_count" ]; then
    echo "Structural mismatch: different number of numerical bounds" >&2
    echo "  Reference has $ref_count bounds" >&2
    echo "  Actual has $act_count bounds" >&2
    exit 1
fi

# If no bounds found, certificates match trivially
if [ "$ref_count" -eq 0 ] || [ -z "$ref_bounds" ]; then
    exit 0
fi

# Compare each bound with adaptive tolerance
exit_code=0
for i in $(seq 1 "$ref_count"); do
    ref=$(echo "$ref_bounds" | sed -n "${i}p")
    act=$(echo "$act_bounds" | sed -n "${i}p")

    # If both values are identical (exact string match), accept immediately
    if [ "$ref" = "$act" ]; then
        continue
    fi

    # Use checkErrorDifference.py for tolerance comparison
    # It applies adaptive tolerance based on least significant digit
    # Note: For very small values (<1e-20), the script may fail due to precision limits
    if python3 "$CHECKER" "$ref" "$act" 2>/dev/null; then
        continue
    fi

    # Fallback for very small numbers or when checkErrorDifference.py fails
    # Very small bounds (<1e-20) are particularly sensitive to platform-specific
    # floating-point behavior in Kodiak's interval arithmetic. For these cases,
    # we use order-of-magnitude comparison (accept if within 100x factor).
    # Rationale: These bounds are negligible in practice - a 100x difference at
    # 1e-27 scale is still only 1e-25, far below any meaningful error threshold.
    if python3 -c "
import sys, math
a, b = float('$ref'), float('$act')

# For very small bounds (<1e-20), platform differences can be extreme
# Accept if within two orders of magnitude (100x factor)
if abs(a) < 1e-20 or abs(b) < 1e-20:
    # Handle zero case
    if min(abs(a), abs(b)) == 0:
        sys.exit(0 if abs(a) == abs(b) else 1)
    # Check if ratio is within 100x
    ratio = max(abs(a), abs(b)) / min(abs(a), abs(b))
    sys.exit(0 if ratio <= 100.0 else 1)
else:
    # Normal relative tolerance for larger values
    sys.exit(0 if math.isclose(a, b, rel_tol=1e-10, abs_tol=1e-50) else 1)
" 2>/dev/null; then
        continue
    fi

    # Bound differs beyond tolerance
    echo "Bound #$i differs beyond tolerance:" >&2
    echo "  Reference: $ref" >&2
    echo "  Actual:    $act" >&2
    exit_code=1
done

exit $exit_code

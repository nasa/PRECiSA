#!/usr/bin/env bash

ABSPATH_SOURCE="$(realpath ${BASH_SOURCE[0]})"
CHECKER="$(dirname $ABSPATH_SOURCE)/checkErrorDifference.py"

typecheck=

while [ $# -gt 1 ]
do
    case $1 in
	--typecheck)
        typecheck=yes
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

results="temp.$name.json"

cd $test_folder

precisa --json $pvsFile $inputFile > $results

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
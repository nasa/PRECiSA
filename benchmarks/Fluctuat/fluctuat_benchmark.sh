#!/bin/bash

FLUCTUATHOME=$1
PATH=$FLUCTUATHOME/bin:$PATH

FILE=$(realpath $2)
IIFILE="${2%.*}.ii"

rm -rf output

daed_project $2
daed_analyze $IIFILE |& tee ianlog

echo ""
echo "BENCHMARK: ${1%.*}"
echo -n "ABS_ERROR: "
head -n 3 output/res.var | tail -n 1 | sed "s|e-|EXP|g" | sed "s|-||g" | sed "s|EXP|e-|g" | sed "s| |,|g" | sed "s|\(.*\)|print(max(\1))|g" | python -

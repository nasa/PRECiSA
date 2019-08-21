#!/bin/bash

RESULTS_DIR=$(pwd)'/results'
INPUT="experiment-inputs"
OVERWRITE=
VERBOSE=

usage() {
    echo "run-experiment -- runs Fluctuat experiment in each file with '.g' extension.

Usage: run-experiment [-o <dir>] [-f]

  -o --output DIR        Directory where the output of each analysis will be 
                         stored. Default value: './results'.
  
  -f --force             Overwrite previous results (if any).
                         Defatul value: "$(if [[ $OVERWRITE ]]; then echo "true"; else echo "false"; fi)".

  -v --verbose           Verbose mode.
                         Default value: "$(if [[ $VERBOSE ]]; then echo "activated"; else echo "no"; fi)".

"  
}

while [ $# -gt 0 ]
do
  case $1 in
      -h|-help|--help)     
	  usage
	  exit 0;;
      -o| --output)
	  RESULTS_DIR=$2;;
      -f| --force)
	  OVERWRITE=T;;
      -v| --verbose)
	  VERBOSE=T;;
  esac
  if [ "$#" -gt 0 ]; then shift; fi
done

run_test() {
    output_file="$results_directory/$1.fluctuat-output"
    input_basename=$(basename $1 '.g')
    echo "Date: $(date)" > $output_file
    echo "Config: $(uname -a)" >> $output_file
    echo "Command: time $shell_command $input_basename" >> $output_file
    if [[ $VERBOSE ]]; then echo -n "Running Fluctuat on $input_basename..."; fi
    $shell_command $1
    rc=$?;
    if [[ $rc != 0 ]];
    then
        echo "Error"
        exit $rc
    else
        if [[ $VERBOSE ]]; then echo " done."; fi
    fi
    return 0;
}

check_directory() {
    if [[ ! -d $1 ]]
    then
	mkdir $1
    else
	if [[ ! $OVERWRITE ]]
	then
	    echo "ERROR: the output directory '$1' already exists."
	    echo "       Use the -f option if you would like to overwrite the old files. "
	    echo
	    exit 1
	fi
    fi
    return 0;
}

run_fluctuat() {
    FILE=$(realpath $1)
    IIFILE="${1%.*}.ii"

    rm -rf output

#    daed_project $1
#    daed_analyze $IIFILE

    (TIMEFORMAT='real  %R'; time (daed_project $1; daed_analyze $IIFILE)) &>> $output_file

    echo "ABS_ERROR: "$(head -n 3 output/res.var | tail -n 1 | sed "s|e-|EXP|g" | sed "s|-||g" | sed "s|EXP|e-|g" | sed "s| |,|g" | sed "s|\(.*\)|print(max(\1))|g" | python -) >> $output_file
    
    return 0
}


EXPERIMENT_INPUTS=$(find . \( ! -name . -prune \) -type f -name '*.experiment-inputs')

for exp_inputs in $EXPERIMENT_INPUTS; do
    exp_name=$(basename $exp_inputs '.experiment-inputs')
    if [[ $VERBOSE ]]; then echo "** Running series '$exp_name' **"; fi
    results_directory=$RESULTS_DIR'-'$exp_name
    #    shell_command="./fluctuat_benchmark.sh"
    shell_command=run_fluctuat
    check_directory $results_directory
    while IFS= read -r expname; do
	if [[ ! ${expname:0:1} == "#" ]];
	then
	    run_test ${expname}
	fi
    done < "$exp_inputs"
done

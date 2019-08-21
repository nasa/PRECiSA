#!/bin/bash

RESULTS_DIR=$(pwd)'/results'
INPUT="experiment-inputs"
OVERWRITE=
VERBOSE=

usage() {
    echo "run-experiment -- runs Rosa experiment in each file with '.g' extension.

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
    output_file="$results_directory/$1.rosa-output"
    input_basename=$(basename $1 '.g')
    echo "Date: $(date)" > $output_file
    echo "Config: $(uname -a)" >> $output_file
    echo "Command: time $shell_command $input_basename" >> $output_file
    if [[ $VERBOSE ]]; then echo -n "Running Rosa on $input_basename..."; fi
    #    (/usr/bin/time -f "real\t%e"  gappa $input_basename'.g') &>> $output_file
    # (TIMEFORMAT='real  %R'; time gappa $input_basename'.g') &>> $output_file
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

run_rosa() {
    CURR_DIR=$(pwd)
    cd ~/rosa/
    (TIMEFORMAT='real  %R'; time ./leon --functions=$1 $CURR_DIR/rosa.scala) &>> $output_file
    cd $CURR_DIR
    return 0
}

EXPERIMENT_INPUTS=$(find . \( ! -name . -prune \) -type f -name '*.experiment-inputs')

for exp_inputs in $EXPERIMENT_INPUTS; do
    exp_name=$(basename $exp_inputs '.experiment-inputs')
    if [[ $VERBOSE ]]; then echo "** Running series '$exp_name' **"; fi
    results_directory=$RESULTS_DIR'-'$exp_name
    shell_command=run_rosa
    check_directory $results_directory
    while IFS= read -r expname; do
	if [[ ! ${expname:0:1} == "#" ]];
	then
	    run_test ${expname}
	fi
    done < "$exp_inputs"
done

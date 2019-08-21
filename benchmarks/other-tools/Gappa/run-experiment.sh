#!/bin/bash

RESULTS_DIR=results
INPUT="experiment-inputs"
OVERWRITE=
VERBOSE=

usage() {
    echo "run-experiment -- runs Gappa experiment in each file with '.g' extension.

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
    output_file="$results_directory/$1.gappa-output"
    if [[ ! -f $1 ]]
    then
	echo "Error: input file '$filename' not found." > $output_file		
    else
	input_basename=$(basename $1 '.g')
	echo "Date: $(date)" > $output_file
	echo "Config: $(uname -a)" >> $output_file
	echo "Command: time $shell_command $input_basename" >> $output_file
	if [[ $VERBOSE ]]; then echo -n "Running Gappa on $input_basename..."; fi
	#    (/usr/bin/time -f "real\t%e"  gappa $input_basename'.g') &>> $output_file
	(TIMEFORMAT='real  %R'; time gappa $input_basename'.g') &>> $output_file
	rc=$?;
	if [[ $rc != 0 ]];
	then
            echo "Error"
            exit $rc
	else
            if [[ $VERBOSE ]]; then echo " done."; fi
	fi
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

EXPERIMENT_INPUT_FILES=$(find . \( ! -name . -prune \) -type f -name '*.experiment-inputs')

for exp_inputs in $EXPERIMENT_INPUT_FILES; do
    exp_name=$(basename $exp_inputs '.experiment-inputs')
    if [[ $VERBOSE ]]; then echo "** Running series '$exp_name' **"; fi
    results_directory=$RESULTS_DIR'-'$exp_name
    shell_command=gappa
    check_directory $results_directory
    while IFS= read -r filename; do
	if [[ ! ${filename:0:1} == "#" ]];
	then
	    run_test ${filename}
	fi
    done < "$exp_inputs"
done

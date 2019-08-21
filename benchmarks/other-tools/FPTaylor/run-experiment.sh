#!/bin/bash

RESULTS_DIR=results
OVERWRITE=
VERBOSE=
BASE_COMMAND="~/FPTaylor/repo/fptaylor"
INPUT="experiment-inputs"

usage() {
    echo "run-experiment -- runs FPTaylor experiment in each file mentioned in the file '$INPUT'.

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
    output_file="$results_directory/$1.fptaylor-output"
    if [[ $VERBOSE ]]; then echo -n "Running Fptaylor on $1..."; fi
    echo "Date: $(date)" > $output_file
    echo "Config: $(uname -a)" >> $output_file
    echo "Command: time $shell_command $1" >> $output_file
    eval "(TIMEFORMAT='real  %R'; time $shell_command $1) &>> $output_file"
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


CFG_FILES=$(find . \( ! -name . -prune \) -type f -name '*.cfg')

# date=$(date +%Y%m%d%k%M)

for cfg in $CFG_FILES; do
    cfg=$(basename $cfg)
    cfg_name=$(basename $cfg '.cfg')
    if [[ $VERBOSE ]]; then echo "** Using configuration '$cfg_name' **"; fi
    results_directory=$RESULTS_DIR'-'$cfg_name
    shell_command=$BASE_COMMAND" -c $cfg"
    check_directory $results_directory
    while IFS= read -r filename; do
	if [[ ! ${filename:0:1} == "#" ]];
	then
	    if [[ ! -f $filename ]]
	    then
		echo "Error input file not found: $filename"		
	    else
		run_test ${filename}
	    fi
	fi
    done < "$INPUT"
    cp $cfg $results_directory/
done

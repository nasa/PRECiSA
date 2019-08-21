#!/bin/bash

INPUT_DIR="results"
OUTPUT_FILENAME="fluctuat"
OVERWRITE=
VERBOSE=

usage() {
    echo "process-output.sh -- process the result of the Fluctuat analysis.

It creates a CSV file with all the information in the indicated directory.

Usage: process-output.sh [-i <directory>] [-o <filename>] [-f] [-v]

  -i --inputs DIR        Directory where the files resulting from the Fluctuat
                         analysis are located.
                         Default value: '$INPUT_DIR'.
  
  -o --output FILENAME   File where the information is collected.
                         Default value: '$OUTPUT_FILENAME'.
  
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

if [[ -f $OUTPUT_FILENAME ]]
then
    if [[ $OVERWRITE ]]
    then
	rm $OUTPUT_FILENAME
    else 
	echo "ERROR: the output file '$OUTPUT_FILENAME' already exists."
	echo "       Use the -f option if you would like to overwrite it. "
	echo
	exit 1
    fi
fi

echo "case name , absolute error, self-reported time (sec.), wall time (sec.)" >> $OUTPUT_FILENAME

pattern_bound="/ABS_ERROR: ([0-9]+\.?[0-9]*e*.*[0-9]*).*$/"
gawk_str_bound=$pattern_bound' {print gensub('$pattern_bound',"\\1","g")}'

# pattern_self_reported_time="/Total time: ([0-9]+\.[0-9]+) seconds.*$/"
# gawk_str_self_reported_time=$pattern_self_reported_time' {print gensub('$pattern_self_reported_time',"\\1","g")}'

pattern_wall_time="/real.*([0-9]+\.[0-9]+).*$/"
gawk_str_wall_time=$pattern_wall_time' {print gensub('$pattern_wall_time',"\\1","g")}'

pattern__command="/Command: (.*)$/"
gawk_str__command=$pattern__command' \
              {print gensub('$pattern__command',"\\1","g");exit;}'

pattern__date="/Date: (.*)$/"
gawk_str__date=$pattern__date' \
              {print gensub('$pattern__date',"\\1","g")}'

pattern__config="/Config: (.*)$/"
gawk_str__config=$pattern__config' \
              {print gensub('$pattern__config',"\\1","g")}'


input_dirs=$(find . -type d -name "$INPUT_DIR*")

for input_dir in $input_dirs; do
    FILES=$(find $input_dir -type f -name '*.fluctuat-output')
    output_file=$OUTPUT_FILENAME"-"$(basename $input_dir)".csv"
    echo 'case name, error bound, self-reported time, wall time, date, command, support configuration' > $output_file
    if [[ $VERBOSE ]]; then echo "Generating $output_file ..."; fi
    for file in $FILES; do
	filename=$(basename $file '.fluctuat-output')
	if [[ $VERBOSE ]]; then echo -n $filename '... '; fi
	bound_str=$(gawk "$gawk_str_bound" $file)
	if [[ -z $bound_str ]]
	then
	    bound_str="n/a"
	    sefl_reported_time_str="n/a"
	    wall_time_str="n/a"
	    date_str="n/a"
	    command_str="n/a"
	    config_str="n/a"
	else
	    sefl_reported_time_str="n/a"  #$(gawk "$gawk_str_self_reported_time" $file)
	    wall_time_str=$(gawk "$gawk_str_wall_time" $file)
	    date_str=$(gawk "$gawk_str__date" $file)
	    command_str=$(gawk "$gawk_str__command" $file)
	    config_str=$(gawk "$gawk_str__config" $file)
	fi
	echo $filename', '\
	     $bound_str', '\
	     $sefl_reported_time_str', '\
	     $wall_time_str', '\
	     $date_str', '\
	     $command_str', '\
	     $config_str >> $output_file
	if [[ $VERBOSE ]]; then echo 'done.'; fi
    done
done




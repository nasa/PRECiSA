#!/bin/bash

INPUT_DIR="results"
OUTPUT_FILENAME="fptaylor"
OVERWRITE=
VERBOSE=

usage() {
    echo "process-output.sh -- process the result of the Fptaylor analysis.

It creates a CSV file with all the information in the indicated directory.

Usage: process-output.sh [-i <directory>] [-o <filename>] [-f] [-v]

  -i --inputs DIR        Directory where the files resulting from the Fptaylor
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

pattern__date="/Date: (.*)$/"
gawk_str__date=$pattern__date' \
              {print gensub('$pattern__date',"\\1","g")}'

pattern__absolute_error_exact="/Absolute error .exact.: (.*)$/"
gawk_str__absolute_error_exact=$pattern__absolute_error_exact' \
              {print gensub('$pattern__absolute_error_exact',"\\1","g")}'

pattern__absolute_error_approximate="/Absolute error .approximate.: (.*)$/"
gawk_str__absolute_error_approximate=$pattern__absolute_error_approximate' \
              {print gensub('$pattern__absolute_error_approximate',"\\1","g")}'

pattern__self_reported_time="/Elapsed time: (.*)$/"
gawk_str__self_reported_time=$pattern__self_reported_time' \
              {print gensub('$pattern__self_reported_time',"\\1","g");exit;}'

pattern__command="/Command: (.*)$/"
gawk_str__command=$pattern__command' \
              {print gensub('$pattern__command',"\\1","g");exit;}'

pattern_wall_time="/real.*([0-9]+\.[0-9]+).*$/"
# pattern__wall_time="/real.*([0-9]+m[0-9]+\.[0-9]+s).*$/"
gawk_str__wall_time=$pattern__wall_time' \
              {print gensub('$pattern__wall_time',"\\1","g");exit;}'

input_dirs=$(find . -type d -name "$INPUT_DIR*")

for input_dir in $input_dirs; do
    FILES=$(find $input_dir -type f -name '*.fptaylor-output')
    output_file=$OUTPUT_FILENAME"-"$(basename $input_dir)".csv"
    echo 'case name, approximate absolute error, self-reported time, wall time,  date, command, exact absolute error' > $output_file
    if [[ $VERBOSE ]]; then echo "Generating $output_file ..."; fi
    for file in $FILES; do
	filename=$(basename $file '.txt.fptaylor-output')
	if [[ $VERBOSE ]]; then echo -n $filename '... '; fi
	# case name
	echo -n $filename ', ' >> $output_file
	# bound
	echo -n $(gawk "$gawk_str__absolute_error_approximate" $file) >> $output_file
	echo -n ', ' >> $output_file
	# self reported time
	echo -n $(gawk "$gawk_str__self_reported_time" $file) >> $output_file
	echo -n ', ' >> $output_file
	# wall time
	echo -n $(gawk "$gawk_str__wall_time" $file) >> $output_file
	echo -n ', ' >> $output_file
	# date
	echo -n $(gawk "$gawk_str__date" $file) >> $output_file
	echo -n ', ' >> $output_file
	# command
	echo -n $(gawk "$gawk_str__command" $file) >> $output_file
	echo -n ', ' >> $output_file
	# extras
	echo -n $(gawk "$gawk_str__absolute_error_exact" $file) >> $output_file
	echo ' ' >> $output_file
	if [[ $VERBOSE ]]; then echo 'done.'; fi
    done
done

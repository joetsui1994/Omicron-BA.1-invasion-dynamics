#!/bin/bash

sampling_freq=40000
resampling_freq=40000

while getopts i:o:s:r: flag
do
	case "${flag}" in
		i) infile=${OPTARG};;
		o) outfile=${OPTARG};;
		s) sampling_freq=${OPTARG};;
		r) resampling_freq=${OPTARG};;
	esac
done

curr_i=0
while read line
do
	if [[ "$line" == "tree"* ]]
	then
		IFS=' ' read -r prefix state tree <<< $line
		state_i=${state#"STATE_"}
		if [ $(( $state_i % resampling_freq )) -eq 0 ]; then
			echo "tree STATE_$((curr_i*resampling_freq)) ${tree}" >> $outfile
			echo "STATE_$((curr_i*resampling_freq))"
			((curr_i++))
		fi
	else
		echo $line >> $outfile
	fi
done < $infile

echo "DONE!"

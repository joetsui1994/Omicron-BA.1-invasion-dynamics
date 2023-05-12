#!/bin/bash

prefix="split"
sampling_freq=6000

while getopts i:p:s: flag
do
	case "${flag}" in
		i) infile=${OPTARG};;
		p) prefix=${OPTARG};;
		s) sampling_freq=${OPTARG};;
	esac
done

echo "Finding line cut-off for Nexus header..."
cutoff=$(grep -ni "tree STATE_${sampling_freq} " $infile | cut -f1 -d:)
((cutoff--))
echo "Cut-off found at line ${cutoff}"

i=0
while read line
do
	if [[ "$line" == "tree"* ]]
	then
		outfile="${prefix}_${i}.tree"
		echo "Writing posterior tree #${i} to ${outfile}..."
		head "-${cutoff}" $infile > $outfile
		echo $line | sed 's/\[&lnP=.*\] =/=/g' >> $outfile
		echo "End;" >> $outfile
		((i++))
	fi
done < $infile

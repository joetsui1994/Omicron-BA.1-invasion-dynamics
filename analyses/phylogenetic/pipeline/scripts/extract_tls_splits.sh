#!/bin/bash

prefix="split"
while getopts i:o:g: flag
do
	case "${flag}" in
		i) inDir=${OPTARG};;
		o) origin=${OPTARG};;
		g) ignore=${OPTARG};;
	esac
done

ignore_str=""
if [ ! -z $ignore ];
then
	ignore_str="--ignore-taxa ${ignore}"
fi

files="${inDir}*.tree"
for f in $files
do
	filename=${f##*/}
	base=${filename%.tree}
	echo "Extracting transmission lineages from ${filename}..."
	echo "Writing output to ${base}_tls.tsv..."
	fertree transmission-lineages -k location -t ENG -o ${origin} -i $f --taxa -n ${ignore_str} > "${base}_tls.tsv"
done

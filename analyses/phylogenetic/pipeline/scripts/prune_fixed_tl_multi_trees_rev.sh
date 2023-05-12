#!/bin/bash

while getopts i:o:t:p: flag
do
	case "${flag}" in
		i) inDir=${OPTARG};;
		o) outDir=${OPTARG};;
		t) tips=${OPTARG};;
		p) prefix=${OPTARG};;
	esac
done

trees="${inDir}*"
for tree in $(ls ${inDir}* | sort -r)
do
	filename=${tree##*/}
	base=${filename%.tree}
	echo "Extracting transmission lineage subtree from posterior tree ${base} and writing to ${prefix}_${base}.newick..."
	fertree prune keep -n -i ${tree} -t ${tips} > ${outDir}${prefix}_${base}.newick
	gotree comment clear -i ${outDir}${prefix}_${base}.newick -o ${outDir}${prefix}_${base}.uc.newick
done

#!/usr/bin/env python3

from ete3 import Tree
import argparse
import sys

## parse command-line options
parser = argparse.ArgumentParser(description='Read in a CSV/TSV file containing relevant metadata and split it into multiple batches for downstream analyses')
parser.add_argument('-i', '--infile', metavar='INPUT FILE', action='store', type=str, required=True, help='CSV or TSV file containing relevant metadata')
parser.add_argument('-o', '--outfile', metavar='OUTPUT FILE', action='store', type=argparse.FileType('w+', encoding='UTF-8'), default=sys.stdout, help='output file (print to stdout by default)')
parser.add_argument('-d', metavar='ROOT BRLEN', action='store', type=float, default=0.0, help='custom branch length to root node (0 by default)')
args = parser.parse_args()

try:
    ## read specified newick treefile
    tree = Tree(args.infile, format=1)
    ## reset branch length to root node
    tree.dist = args.d

    ## write to outfile if specified
    if args.outfile.name != '<stdout>':
        tree.write(format=1, outfile=args.outfile.name)
    else: ## write to stdout otherwise
        print(tree.write(format=1))

except Exception as e:
	print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")

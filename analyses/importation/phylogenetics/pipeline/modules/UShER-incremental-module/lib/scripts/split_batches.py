#!/usr/bin/env python3

import pandas as pd
import argparse
import os

## allowed column names for sequence name and sample date
date_choices = ['date', 'sample_date', 'collection_date']
name_choices = ['name', 'strain', 'sequence_name', 'seq_name']

def dir_path(path):
    if os.path.isdir(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"readable_dir:{path} is not a valid path")

## parse command-line options
parser = argparse.ArgumentParser(description='Read in a CSV/TSV file containing relevant metadata and split it into multiple batches for downstream analyses')
parser.add_argument('-i', '--infile', metavar='INPUT FILE', action='store', type=str, required=True, help='CSV or TSV file containing relevant metadata')
parser.add_argument('-o', '--outdir', metavar='OUTPUT DIRECTORY', action='store', type=dir_path, help='path to directory in which output files are to be stored', default='./')
parser.add_argument('-d', '--delimiter', metavar='DELIMITER', action='store', type=str, help='delimiter used in metadata file (tab-separated by default)', default='\t')
parser.add_argument('-s', '--suffix', metavar='OUTPUT SUFFIX', action='store', type=str, help='suffix for output files', default='')
args = parser.parse_args()

try:
    ## read specified metadata file
    meta = pd.read_csv(args.infile, sep=args.delimiter)
    ## convert all column names to lower case
    meta = meta.rename(columns={ k: k.lower() for k in meta.columns })
    ## check that strain/sequence_name/name, date field exist in metadata file and parse
    date_index = meta.columns.tolist().index(next((a for a in meta.columns if a in set(date_choices)), None))
    name_index = meta.columns.tolist().index(next((a for a in meta.columns if a in set(name_choices)), None))
    date_col = meta.columns[date_index]
    name_col = meta.columns[name_index]

    ## sort dataframe by date and collect all unique dates
    meta = meta.sort_values(date_col)
    all_dates = meta[date_col].unique()

    ## loop over all dates
    for date in all_dates:
        date_name = date.replace('-', '')
        meta_tmp = meta.loc[meta[date_col] == date]
        out_name = '%s%s.tsv' % (date_name, ('.' if args.suffix else '') + args.suffix)
        out_path = os.path.join(args.outdir, out_name)
        meta_tmp[[name_col]].to_csv(out_path, sep='\t', header=False, index=False)

except Exception as e:
	print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")

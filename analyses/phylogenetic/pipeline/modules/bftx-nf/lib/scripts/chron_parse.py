#!/usr/bin/env python3.8

from statistics import NormalDist
import datetime as dt
import argparse
import sys
import math

## minimum allowed date variance (unit=years)
min_var = 0.0001
day_seconds = 60*60*24

## parse command-line options
parser = argparse.ArgumentParser(description='Parse predicted tip dates from Chronumental v0.0.44 (Theo Sanderson, 2021)')
parser.add_argument('--infile', metavar='INPUT FILE', action='store', type=argparse.FileType('r', encoding='UTF-8'), required=True, help='unprocessed output file from chronumental containing predicted tip dates (with header)')
parser.add_argument('--dates', metavar='METADATA', action='store', type=argparse.FileType('r', encoding='UTF-8'), required=True, help='metadata file with at least columns strain/name and date')
parser.add_argument('--outfile', metavar='OUTPUT FILE', action='store', type=argparse.FileType('w+', encoding='UTF-8'), default=sys.stdout, help='output file (print to stdout by default)')
parser.add_argument('-var', '--variance', metavar='DATE VARIANCE', action='store', type=float, default=0.3, help='specify variance of normal distribution centred at predicted tip date (default=0.3, min=%f)' % min_var)
parser.add_argument('-p', '--p_value', metavar='ALLOWED TWO-SIDED P-VALUE', action='store', type=float, default=0.95)
parser.add_argument('-d', '--days', metavar='ALLOWED DIFFERENCE', action='store', type=float, default=0, help='allowed difference between observed and predicted tip date (unit=days)')
parser.add_argument('-v', '--verbose', action='store_true', default=False, help='print summary of pass/fail rate (off by default)')
parser.add_argument('--output_meta', action='store_true', default=False, help='output metadata of sequences that passed temporal QC (only names are printed by default)')
parser.add_argument('--header', action='store_true', default='False', help='output sequence information with header')
args = parser.parse_args()

def parse_date(date_str):
	date_format = date_str.count('-')
	if date_format == 2:
		return dt.datetime.strptime(date_str.strip(), '%Y-%m-%d')
	elif date_format == 1:
		return dt.datetime.strptime(date_str.strip() + '-01', '%Y-%m-%d')
	else:
		return dt.datetime.strptime(date_str.strip() + '-01-01', '%Y-%m-%d')

try:
	## read input file containing predicated dates from Chronumental
	input = args.infile.read().strip().splitlines()[1:]
	args.infile.close()
	## read meta file containing date column
	meta = args.dates.read().strip().splitlines()
	args.dates.close()

	## check that strain/name, date field exists in meta file and parse
	columns = [x.lower() for x in meta[0].split('\t')]
	entries = [x.split('\t') for x in meta[1:]]
	date_index = columns.index('date')
	name_index = columns.index('name') if 'name' in columns else columns.index('strain')
	dates_dict = { t[name_index]: parse_date(t[date_index]) for t in entries }

	## get rid of milliseconds and take only %Y-%m-%d %H:%M:%S
	preds = [x.split('\t') for x in input]
	pred_dict = { t[0]: dt.datetime.strptime(t[1].split('.')[0], '%Y-%m-%d %H:%M:%S') for t in preds }
	
	## compute difference in observed and predicted tip dates (unit=days)
	diff_dict = { k: abs(v - pred_dict[k]) for k, v in dates_dict.items() }
	
	## pass/fail condition (using a normal distribution centred on the predicted date by default, otherwise use absolute different in unit of days)
	if not (args.p_value >= 0 and args.p_value <= 1):
		raise ValueError('acceptable two-sided p-value must be between 0 and 1')
	abs_cond = abs(args.days) if args.days else NormalDist(mu=0, sigma=math.sqrt(max(abs(args.variance), min_var))).inv_cdf(0.5*(args.p_value+1))
	## assess pass/fail
	pass_dict = { k: (v.days + v.seconds/day_seconds) <= abs_cond for k, v in diff_dict.items() }

	## print outlier assessment if verbose
	true_count = sum(list(pass_dict.values()))
	if args.verbose:
		print('\n')
		print('Chronumental output analysis COMPLETED: %d/%d (PASS/FAIL)' % (true_count, len(pass_dict) - true_count))
		print('Maximum allowed difference between observed and predicted tip date: %f days' % abs_cond)
		print('\n')

	## output name/meta of sequences that passed, with/out header
	out_str = ''
	if args.output_meta:
		if args.header:
			out_str += ('%s\n' % meta[0])
		out_str += ('\n'.join(['\t'.join(x) for x in list(filter(lambda entry: pass_dict[entry[name_index]], entries))]))
	else:
		out_str += ('\n'.join(list(filter(lambda name: pass_dict[name], list(pass_dict.keys())))))

	## write to outfile if specified
	if args.outfile.name != '<stdout>':
		args.outfile.write(out_str)
	else: ## write to stdout otherwise
		print(out_str)

except Exception as e:
	print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")
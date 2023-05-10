#!/usr/bin/env python3

## Load in modules
import geopandas as gpd
import pandas as pd
import math
from shapely.geometry import Point
from pyproj import Proj, transform
import datetime as dt
import dendropy
import tqdm
import json
import os
import sys

## utility functions
def get_date_str(dec_date):
    date = dt.datetime(int(dec_date), 1, 1) + dt.timedelta(days = (dec_date % 1) * 365)
    return dt.datetime.strftime(date, '%Y-%m-%d')

## prompts
ref_file = ''
while not os.path.exists(ref_file):
    print('Enter path to tsv file containing table of transmission lineages to be processed:')
    ref_file = input()

out_dir = ''
while not os.path.isdir(out_dir):
    print('Enter path to output folder for storing edge times summary:')
    out_dir = input()

in_dir = ''
while not os.path.isdir(in_dir):
    print('Enter path to input directory containing all input mcc trees:')
    in_dir = input()

## load in TL table with most recent sample time for each TL
tls = pd.read_csv(ref_file, sep='\t')

## prepare projections
inProj = Proj(init='epsg:4326')
outProj = Proj(init='epsg:3395')

try:
    for tl in tqdm.tqdm(tls.label.unique()[300:]):
        tree_filename = os.path.join(in_dir, '%s.cut_200m.combined.mcc.tree' % tl)

        ## Do actual processing
        tree = dendropy.Tree.get(path=tree_filename, schema='nexus')

        ## read most recent sample time
        most_recent_dec_date = tls[tls.label == tl].most_recent_dec_date.values[0]

        ## name internal nodes
        count = 0
        for node in tree.preorder_node_iter():
            count += 1
            node.label = count

        ## store information from tree
        edge_info = []
        for edge in tree.edges():
            if edge.tail_node:
                head_node = edge.tail_node ## tail_node in edge indicates older node (confusing defintion)
                head_dec_date = most_recent_dec_date - float(head_node.annotations['height'].value)
                head_date = get_date_str(head_dec_date)
                original_head_lat = head_node.annotations['coordinates1'].value
                original_head_long = head_node.annotations['coordinates2'].value

                tail_node = edge.head_node ## head_node in edge indicates younger node (confusing defintion)
                tail_dec_date = most_recent_dec_date - float(tail_node.annotations['height'].value)
                tail_date = get_date_str(tail_dec_date)
                original_tail_lat = tail_node.annotations['coordinates1'].value
                original_tail_long = tail_node.annotations['coordinates2'].value

                proj_head_long, proj_head_lat = transform(inProj, outProj, original_head_long, original_head_lat)
                proj_tail_long, proj_tail_lat = transform(inProj, outProj, original_tail_long, original_tail_lat)

                head_point = Point(float(proj_head_long), float(proj_head_lat))
                tail_point = Point(float(proj_tail_long), float(proj_tail_lat))

                edge_info.append(
                    {
                        'head_node': head_node.label,
                        'tail_node': tail_node.label,
                        'head_date': head_date,
                        'head_dec_date': head_dec_date,
                        'tail_date': tail_date,
                        'tail_dec_date': tail_dec_date,
                        'head_lat_3395': proj_head_lat,
                        'head_long_3395': proj_head_long,
                        'tail_lat_3395': proj_tail_lat,
                        'tail_long_3395': proj_tail_long,
                        'head_lat_4326': original_head_lat,
                        'head_long_4326': original_head_long,
                        'tail_lat_4326': original_tail_lat,
                        'tail_long_4326': original_tail_long
                    }
                )

        edge_info_df = pd.DataFrame(edge_info)
        edge_info_filename = os.path.join(out_dir, '%s_mcc_edge_metadata.tsv' % tl)
        edge_info_df.to_csv(edge_info_filename, sep='\t', index=False)

    print('Exiting now...')

except Exception as e:
    print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")
    sys.exit()


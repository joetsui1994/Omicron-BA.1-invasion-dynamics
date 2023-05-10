#!/usr/bin/env python3

## Load in modules
import pandas as pd
import geopandas as gpd
import math
from shapely.geometry import Point
import dendropy
import datetime as dt
import tqdm
import json
import os
import sys

## Utility function(s)
def map2region(row, region_map):
    head_point = Point(float(row.head_long_4326), float(row.head_lat_4326))
    tail_point = Point(float(row.tail_long_4326), float(row.tail_lat_4326))

    head_region = None
    tail_region = None
    head_region_amb = False
    tail_region_amb = False
    head_region_amb_dists = {}
    tail_region_amb_dists = {}

    for region, polygon in zip(region_map.region.values, region_map.geometry.values):
        head_region_amb_dist = polygon.distance(head_point)
        head_region_amb_dists[region] = head_region_amb_dist
        tail_region_amb_dist = polygon.distance(tail_point)
        tail_region_amb_dists[region] = tail_region_amb_dist

        if not head_region and polygon.contains(head_point):
            head_region = region
        if not tail_region and polygon.contains(tail_point):
            tail_region = region
        if head_region and tail_region:
            break

    else:
        if not head_region:
            head_region_amb = True
            min_dist_region = sorted([(region, dist) for region, dist in head_region_amb_dists.items()], key=lambda x: x[1])[0]
            head_region = min_dist_region[0]
        if not tail_region:
            tail_region_amb = True
            min_dist_region = sorted([(region, dist) for region, dist in tail_region_amb_dists.items()], key=lambda x: x[1])[0]
            tail_region = min_dist_region[0]

    return [head_region, tail_region, head_region_amb, tail_region_amb]

def get_displacement(pos_1, pos_2, km=True):
    R = 6371e3 ## Earth's radius in meters
    phi_1 = pos_1.y*math.pi/180
    phi_2 = pos_2.y*math.pi/180
    delta_phi = (pos_2.y - pos_1.y)*math.pi/180
    delta_lambda = (pos_2.x - pos_1.x)*math.pi/180

    a = math.sin(delta_phi/2)**2 + math.cos(phi_1)*math.cos(phi_2)*math.sin(delta_lambda/2)**2
    c = 2*math.atan2(math.sqrt(a), math.sqrt(1-a))
    d = R*c

    return (d/1000 if km else d) ## return shortest distance in metres


## Load in NUT1 level merged shapefiles and set up
region_map = gpd.read_file('./merged_regions_shp/merged_regions.shp')
region_map['region'] = region_map.OBJECTID.apply(lambda x: x.split('_')[0])

## Loop over split_aug_edge_infos files
ref_file = ''
while not os.path.exists(ref_file):
    print('Enter path to tsv file containing table of transmission lineages to be processed:')
    ref_file = input()

in_dir = ''
while not os.path.isdir(in_dir):
    print('Enter path to input folder containing the mcc tree of each transmission lineage to be processed:')
    in_dir = input()

## Load in TL table
tls = pd.read_csv(ref_file, sep='\t')

try:
    for tl in tls.label.unique():
        tree_filename = os.path.join(in_dir, '%s.cut_200m.combined.mcc.tree' % tl)
        
        ## read in mcc tree
        tree = dendropy.Tree.get(path=tree_filename, schema='nexus')

        ## name internal nodes
        count = 0
        for node in tree.preorder_node_iter():
            count += 1
            node.label = count

        ## store information from tree
        edge_info = []
        for edge in tqdm.tqdm(tree.edges()):
            if edge.tail_node:
                head_node = edge.tail_node ## tail_node in edge indicates older node (confusing definition)
                original_head_lat = head_node.annotations['coordinates1'].value
                original_head_long = head_node.annotations['coordinates2'].value

                tail_node = edge.head_node ## head_node in edge indicates younger node (confusing definition)
                original_tail_lat = tail_node.annotations['coordinates1'].value
                original_tail_long = tail_node.annotations['coordinates2'].value

                geo_dist = get_displacement(Point(float(original_head_long), float(original_head_lat)), Point(float(original_tail_long), float(original_tail_lat)))

                edge_info.append(
                    {
                        'head_node': head_node.label,
                        'tail_node': tail_node.label,
                        'length': edge.length,
                        'geo_distance': geo_dist,
                        'head_lat_4326': original_head_lat,
                        'head_long_4326': original_head_long,
                        'tail_lat_4326': original_tail_lat,
                        'tail_long_4326': original_tail_long
                    })

        ## augment edge metadata
        edge_info = pd.DataFrame(edge_info)
        aug_edge_info = edge_info.apply(lambda row: map2region(row, region_map), axis='columns', result_type='expand')
        aug_edge_info = aug_edge_info.rename(columns={0: 'head_region', 1: 'tail_region', 2: 'head_region_amb', 3: 'tail_region_amb'})
        combined_edge_info = pd.concat([edge_info, aug_edge_info], axis='columns')

        ## write to file
        out_filename = os.path.join(in_dir, '%s_geo_summary.tsv' % tl)
        print('Writing aggregated geo-metadata summary file to %s' % out_filename)
        combined_edge_info.to_csv(out_filename, sep='\t', index=False)

    print('Exiting now...')

except Exception as e:
    print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")
    sys.exit()


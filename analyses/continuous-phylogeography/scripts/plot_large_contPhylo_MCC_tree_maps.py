#!/usr/bin/env python3

import geopandas as gpd
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, Normalize
import matplotlib.patches as mpatches
from matplotlib import cm
import tqdm
import os
import sys

## utility functions
def get_date_str(dec_date):
    date = dt.datetime(int(dec_date), 1, 1) + dt.timedelta(days = (dec_date % 1) * 365)
    return dt.datetime.strftime(date, '%Y-%m-%d')

## prompt input edge metadata file
edge_metadata_filename = ''
while not os.path.exists(edge_metadata_filename):
    print('Enter path to input edge metadata file:')
    edge_metadata_filename = input()

out_dir = ''
while not os.path.isdir(out_dir):
    print('Enter path to output folder to which viral movement maps will be written to:')
    out_dir = input()

try:
    
    ## load in shapefiles
    dirname, filename = os.path.split(os.path.realpath(__file__))
    uk_map = gpd.read_file(os.path.join(dirname, 'shp/shapefile_out.shp'))
    uk_map = uk_map.to_crs('epsg:3395')

    ## read in edge metadata file
    mcc_edge_df = pd.read_csv(edge_metadata_filename, sep='\t')
    mcc_edge_df = mcc_edge_df.sort_values('head_date', ascending=False)

    ## read in dates in decimal
    dec_date_df = pd.read_csv(os.path.join(dirname, 'dec_date_map.tsv'), sep='\t')
    date_dec_map = dict(zip(dec_date_df.date.values, dec_date_df.dec_date.values))
    ## all_dates = [date_dec_map[x] for x in (mcc_edge_df.head_date.unique().tolist() + mcc_edge_df.tail_date.unique().tolist())]
    ## norm = Normalize(vmin=min(all_dates), vmax=max(all_dates))

    all_dates = dec_date_df[dec_date_df.date >= '2021-11-07'].dec_date.values
    norm = Normalize(vmin=min(all_dates), vmax=max(all_dates))

    ## create colour map
    cmap = LinearSegmentedColormap.from_list("", ["#0C4C5F", "#de801b", "#fadc46"])
    colour_dict = {}
    for date in all_dates:
        colour_dict[date] = cmap(norm(date))
    
    ## specify cutoff-dates
    cutoff_dates = ['2021-11-30', '2021-12-03', '2021-12-05', '2021-12-07', '2021-12-10', '2021-12-15', '2021-12-20',
            '2021-12-25', '2021-12-31', '2022-01-10', '2022-01-15', '2022-01-20', '2022-01-31']

    ## plot
    for cutoff_date in cutoff_dates:
    
        print('Generating map of viral movements up to %s...' % cutoff_date)
    
        fig, ax = plt.subplots(1,1, figsize=(8, 8), dpi=200)
        uk_map.plot(ax=ax, color='whitesmoke', ec='lightgrey', linewidth=0.25, alpha=0.7)

        for index, row in tqdm.tqdm(mcc_edge_df[(mcc_edge_df.head_date <= cutoff_date) & (mcc_edge_df.tail_date <= cutoff_date)].iterrows()):
            head_pos = (row['head_long_3395'], row['head_lat_3395'])
            tail_pos = (row['tail_long_3395'], row['tail_lat_3395'])
            head_date = date_dec_map[row['head_date']]
            tail_date = date_dec_map[row['tail_date']]

            kw = dict(color="black", linewidth=0.09, alpha=0.75)

            liney = mpatches.FancyArrowPatch(head_pos, tail_pos, shrinkA=0, shrinkB=0, arrowstyle="-", connectionstyle="arc3, rad=.5", **kw)
    
            ax.add_patch(liney)
            ax.scatter(head_pos[0], head_pos[1], s=3, color=colour_dict[head_date], edgecolor='black', linewidth=0.15, alpha=0.85, zorder=5)
            ax.scatter(tail_pos[0], tail_pos[1], s=3, color=colour_dict[tail_date], edgecolor='black', linewidth=0.15, alpha=0.85, zorder=6)

        bar = fig.colorbar(cm.ScalarMappable(norm=norm, cmap=cmap), ax=ax, shrink=0.25)
        new_ticks = []
        for i in bar.get_ticks():
            new_ticks.append(get_date_str(i))
        bar.set_ticks(bar.get_ticks())
        bar.set_ticklabels(new_ticks)
        bar.ax.tick_params(labelsize=7)

        ax.set_axis_off()

        out_filename = os.path.join(out_dir, 'vMov_%s_map.pdf' % cutoff_date)
        print('Saving map of viral movements up to %s to %s...' % (cutoff_date, out_filename))
        plt.savefig(out_filename, bbox_inches='tight', dpi=200)

    print('Exiting now...')

except Exception as e:
    print(f"{type(e).__name__} at line {e.__traceback__.tb_lineno} of {__file__}: {e}")
    sys.exit()

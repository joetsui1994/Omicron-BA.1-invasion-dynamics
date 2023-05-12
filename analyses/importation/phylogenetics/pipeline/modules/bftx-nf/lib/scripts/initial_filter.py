#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from re import A
import click
import logging
import csv
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
from datetime import datetime
import subprocess


@click.command()
@click.argument('metadata', type=click.Path())
@click.argument('quality', type=click.Path())
@click.argument('output', type=click.Path())

def main(metadata,quality,output):
    """ Runs data processing scripts to turn raw data from (../raw) into
        cleaned data ready to be analyzed (saved in ../processed).
    """
    logger = logging.getLogger(__name__)

    dateformat = "%Y-%m-%d"
    wrong_pillar = 0
    long_lag = 0
    kept = 0
    cog_quality=0

    #log date issues from datapipe
    dateIssues = set()
    with open(quality, 'r') as issues:
        reader = csv.DictReader(issues)
        for line in reader:
            dateIssues.add(line["sequence_name"])

    with open("./qualityLog.csv", 'w') as log:
        log.write("sequence_name,note\n")
        with open(metadata,'r') as fin:
            with open (output,'w') as fout:
                reader = csv.DictReader(fin)
                writer = csv.DictWriter(fout,fieldnames=reader.fieldnames)
                writer.writeheader()        
                
                for row in reader:
                    if row["sequence_name"] in dateIssues:
                        cog_quality+=1
                        log.write("%s,%s\n" %(row["sequence_name"],"cog quality fail"))
                    else:
                        if row['is_pillar_2']!='N':
                            sample_date = datetime.strptime(row['sample_date'],dateformat).date()
                            published_date = datetime.strptime(row['published_date'],dateformat).date()
                            if sample_date <= published_date and (published_date-sample_date).days<28:
                                writer.writerow(row)
                                kept+=1
                            else:
                                long_lag+=1
                                log.write("%s,%s\n" %(row["sequence_name"],"sampling lag"))
                        else:
                            wrong_pillar+=1
                            log.write("%s,%s\n" %(row["sequence_name"],"Pillar other than pillar 2 reported"))

    logger.info("%d - sequences kept", kept)
    logger.info("%d - sequences removed because a pillar other than 2 was reported",wrong_pillar)
    logger.info("%d - sequences removed because of lag of 4 weeks or more between collection and publication",long_lag)
    logger.info("%d - sequences removed because of issued identified by cog quality checks",cog_quality)

if __name__ == '__main__':
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())

    main()

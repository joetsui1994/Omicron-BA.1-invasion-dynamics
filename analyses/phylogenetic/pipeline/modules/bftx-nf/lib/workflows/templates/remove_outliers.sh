#!/usr/bin/env bash

cat !{divergence_tree_nex} | sed -e 's/\[[^]]*\]//g' > uc_divergence_tree.nexus
cat !{time_tree_nex} | sed -e 's/\[[^]]*\]//g' > uc_time_tree.nexus

gotree prune -f !{outliers_tsv} -i uc_divergence_tree.nexus --format nexus > refined_divergence_tree.nh
gotree prune -f !{outliers_tsv} -i uc_time_tree.nexus --format nexus > refined_time_tree.nh
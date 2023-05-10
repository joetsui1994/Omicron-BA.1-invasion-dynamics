## Continuous phylogeography

### [Scripts](scripts/)

- `extract_between-cities_viral_movements_contPhylo_MCC.ipynb` Jupyter notebook for mapping viral lineage movements to relevant selected cities; relevant to Fig. S11

- `calculate_contPhylo_MCC_city_within-all-viral_lineages-ratio.R` R script for processing output of `extract_between-cities_viral_movements_contPhylo_MCC.ipynb` to calculate the weekly ratio between within-location versus all viral lineage movements for selected cities in England; relevant to Fig. S11

- `extract_contPhylo_MCC_edge_metadata.py` Bash script for processing and extracting metadata (e.g. mapping of inferred coordinates to corresponding LTLAs) from MCC trees

- `extract_contPhylo_MCC_node_geo.py` Bash script for processing and extracting metadata (e.g. mapping of inferred coordinates to corresponding LTLAs) from MCC trees (similar to `extract_contPhylo_MCC_edge_metadata.py`, with additional calculation of the great-circle displacement of viral lineages, and mapping of inferred coordinates to corresponding regions)

- `plot_contPhylo_MCC_regional_movements.R` R script for evaluating the intensity of among-region viral lineage movements; relevant to Fig. 3

- `plot_large_contPhylo_MCC_tree_maps.py` Bash script for mapping inferred coordinates to their corresponding LTLAs (from MCC trees) and visualising the spatial distribution of viral lineages on a map (optimised for large trees); relevant to Fig. 3 and S12

- `process_contPhylo_MCC_tree_map_BA.1_1800_n944.ipynb` `process_contPhylo_MCC_tree_map_BA.1_1944_n1406.ipynb` `process_contPhylo_MCC_tree_map_BA.1.1_1254_n2249.ipynb` `process_contPhylo_MCC_tree_map_BA.1.1_1467_n722.ipynb` `process_contPhylo_MCC_tree_map_BA.1.15_102_n2967.ipynb` `process_contPhylo_MCC_tree_map_BA.1.15_700_n713.ipynb` Jupyter notebooks for mapping inferred coordinates to their corresponding LTLAs (from MCC trees) and visualising the spatial distribution of viral lineages on a map; relevant to Fig. 3 and S12

- `plot_contPhylo_MCC_source_sink_map_BA.1_1944_n1406.ipynb``plot_contPhylo_MCC_source_sink_map_BA.1.1_1467_n722.ipynb` `plot_contPhylo_MCC_source_sink_map_BA.1.15_102_n2967.ipynb` `plot_contPhylo_MCC_source_sink_map_BA.1.17_175_n11351.ipynb` Jupyter notebooks for visualising the source-sink dynamics of BA.1 on a map from the MCC trees of selected transmission lineages; relevant to Fig. 3 and S12

### [XMLs](XMLs/)

- `large_BA.1_DTA_1944_n1406_contPhylo.xml` `large_BA.1_DTA_1207_n9727_contPhylo.xml` `large_BA.1.1_DTA_1467_n722_contPhylo.xml` `large_BA.1.1_DTA_1254_n2249_contPhylo.xml` `large_BA.1.17_DTA_175_n11351_contPhylo.xml` `large_BA.1.15_DTA_700_n713_contPhylo.xml` `large_BA.1_DTA_1800_n944_contPhylo.xml` `large_BA.1.15_DTA_102_n2975_contPhylo.xml` XMLs for reconstruction of large (# of tips > 700) local BA.1 transmission lineages with individual diffusion models

- `BA.1.1_2239_n116_contPhylo.p.xml` `BA.1.17_478_n113_contPhylo.p.xml` `BA.1.17_475_n146_contPhylo.p.xml` `BA.1.1_2076_n159_contPhylo.p.xml` `BA.1_503_n212_contPhylo.p.xml` `BA.1_1854_n104_contPhylo.p.xml` `BA.1.17_512_n181_contPhylo.p.xml` `BA.1_1060_n256_contPhylo.p.xml` `BA.1.1_592_n254_contPhylo.p.xml` `BA.1_198_n106_contPhylo.p.xml` `BA.1.1_1028_n136_contPhylo.p.xml` `BA.1.1_836_n102_contPhylo.p.xml` `BA.1_1551_n180_contPhylo.p.xml` `BA.1.17_16_n224_contPhylo.p.xml` `BA.1_351_n103_contPhylo.p.xml` `BA.1.1_1677_n368_contPhylo.p.xml` `BA.1.1_2050_n125_contPhylo.p.xml` `BA.1.1_758_n193_contPhylo.p.xml` XMLs for reconstruction of 18 smaller (# of tips > 4) local BA.1 transmission lineages with individual diffusion models

- `small_TLs_n524_joint_contPhylo.xml` XML for reconstruction of all smaller (# of tips > 4) local BA.1 transmission lineages with a joint diffusion model
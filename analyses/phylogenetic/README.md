## Phylogenetic Analyses

### [Pipeline](pipline/)
[TODO]

### [EIIs](EIIs/)
- [`calculate_USA_ESP_within-country_EIIs.R`](EIIs/calculate_USA_ESP_within-country_EIIs.R) R script for calculating weekly Omicron BA.1 EIIs for Spain at the autonomous community-level and the United States at the state-level
- [`exporter_country_test_per_capita_plot.R`](EIIs/exporter_country_test_per_capita_plot.R) R script for calculating weekly number of COVID-19 tests performed per capita at the country-level; relevant to Fig. S4
[TODO]

### [Lineage-branching-simulation](lineage-branching-simulation/)
- [`lineage-simulation.R`](lineage-branching-simulation/lineage-simulation.R) R script for running the branching process simulation and generating Figs. S7 and S8
- [`DTA_mcc_tls_metadata.tsv`](lineage-branching-simulation/DTA_mcc_tls_metadata.tsv) List of all Omicron BA.1 transmission lineages in England (identified from 2-state DTA) with their corresponding ID, size, and estimated time of importation
- [`DTA_taxon_tl_lookup.tsv`](lineage-branching-simulation/DTA_taxon_tl_lookup.tsv) List of all relevant English genomes with their corresponding sample date and transmission lineage-ID

### [Scripts](scripts/)
- [`BA.1_cases_attributed_TLs_rel_travel_restrictions_plot.R`](scripts/BA.1_cases_attributed_TLs_rel_travel_restrictions_plot.R) R script for calculating the daily proportions of reported Omicron BA.1 cases attributable to transmission lineages with different times of importation and visualising the importation intensity inferred from phylogenetics with breakdown by size of resulting transmission lineage; relevant to Fig. 1
- [`local_TLs_size_distribution_plot.R`](scripts/local_TLs_size_distribution_plot.R) R script for visualising the size distribution of Omicron BA.1 transmission lineages inferred from phylogenetics; relevant to Fig. S3
- [`local_TLs_tpmrca-tmrca_plot.R`](scripts/local_TLs_tpmrca-tmrca_plot.R) R script for visualising the distribution of TPMRCA and TMRCA of Omicron BA.1 transmission lineages inferred from phylogenetics; relevant to Fig. 1
- [`phylo_importation_exp_fit_plot.R`](scripts/phylo_importation_exp_fit_plot.R) R script for fitting an exponential model to the importation intensity inferred from phylogenetics; relevant to Fig. 1
- [`UTLA_BA.1_genome_cases_distribution_plot.R`](scripts/UTLA_BA.1_genome_cases_distribution_plot.R) R script for evaluating the correlation between estimated number of Omicron BA.1 cases and number of Omicron BA.1 genomes sampled across UTLAs in England; relevant to Fig. S9
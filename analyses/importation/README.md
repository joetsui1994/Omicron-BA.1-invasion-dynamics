## Importation Analyses

### [Phylogenetics](phylogenetics/)
This directory contains all scripts and materials relevant to the phylogenetic/phylodynamic analyses.

### [EIIs](EIIs/)
- `calculate_USA_ESP_within-country_EIIs.R` R script for calculating weekly Omicron BA.1 EIIs for Spain at the autonomous community-level and the United States at the state-level
- `exporter_country_test_per_capita_plot.R` R script for calculating weekly number of COVID-19 tests performed per capita at the country-level; relevant to Fig. S3
- `calculate_country-level_EIIs.R` R script for calculating weekly Omicron BA.1 EIIs for all selected potential exporters at country-level; relevant to Fig. S4 and S5
- `USA_ESP_within-country_sensitivity_EIIs_plot.R` R script for sensitivity analysis with EIIs calculated for Spain and the United States at the autonomous community- and state-level, respectively; relevant to Fig. S6
- `travel-history_EII_phylo_importation_comparison_plot.R` R script comparing importation intensity inferred from phylogenetic analysis with EIIs and country of origin of inbound travellers tested positive for BA.1 (data from UKHSA); relevant to Fig. 2

### [Lineage-branching-simulation](lineage-branching-simulation/)
- `lineage-simulation.R` R script for running the branching process simulation and generating Figs. S7 and S8
- `DTA_mcc_tls_metadata.tsv` List of all Omicron BA.1 transmission lineages in England (identified from 2-state DTA) with their corresponding ID, size, and estimated time of importation
- `DTA_taxon_tl_lookup.tsv` List of all relevant English genomes with their corresponding sample date and transmission lineage-ID
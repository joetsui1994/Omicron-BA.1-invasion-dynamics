## Phylogenetic Analyses

### [Pipeline](pipline/)
This directory contains scripts and code used to run the phylogenetic analysis pipeline. The pipeline is implemented using Nextflow and currently divided into separate modules. This is because, for certain processes (e.g. checks for MCMC convergence), the current implementation requires the output to be evaluated manually or visually using external programs before any downstream analysis can proceed. This means that some steps have to be triggered manually before the workflow can continue running; work has been done to streamline this process however. Please also note that, much of the data used in these analyses cannot be shared publicly for privacy reasons and so these scripts might not run as expected.

### [Scripts](scripts/)
- `BA.1_cases_attributed_TLs_rel_travel_restrictions_plot.R` R script for calculating the daily proportions of reported Omicron BA.1 cases attributable to transmission lineages with different times of importation and visualising the importation intensity inferred from phylogenetics with breakdown by size of resulting transmission lineage; relevant to Fig. 1
- `local_TLs_size_distribution_plot.R` R script for visualising the size distribution of Omicron BA.1 transmission lineages inferred from phylogenetics; relevant to Fig. S3
- `local_TLs_tpmrca-tmrca_plot.R` R script for visualising the distribution of TPMRCA and TMRCA of Omicron BA.1 transmission lineages inferred from phylogenetics; relevant to Fig. 1
- `phylo_importation_exp_fit_plot.R` R script for fitting an exponential model to the importation intensity inferred from phylogenetics; relevant to Fig. 1
- `UTLA_BA.1_genome_cases_distribution_plot.R` R script for evaluating the correlation between estimated number of Omicron BA.1 cases and number of Omicron BA.1 genomes sampled across UTLAs in England; relevant to Fig. S9
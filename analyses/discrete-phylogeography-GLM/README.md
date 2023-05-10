## Discrete phylogeography with GLM

### [Scripts](scripts/)

- `booster_uptake_cumulative_BA.1_cases_age-dependency_plot.R` R script for evaluating the age-dependency of booster uptake and cumulative BA.1 case counts; relevant to Fig. S16

- `effective_booster_uptake_avg_age_distr_epoch12_maps.ipynb` Jupyter notebook for visualising the spatial distribution of effective booster uptake at the LTLA-level in England; relevant to Fig. S16

- `time-homogeneous_GLM_posterior_coefficient_plots.R` R script for visualising the posterior distribution of coefficient * inclusion probability of predictors considered in the time-homogeneous DTA-GLM analysis

- `time-inhomogeneous_GLM_posterior_coefficient_deviance_plots.R` R script for visualising the posterior distribution of coefficient * inclusion probability and deviance ranking of predictors considered in the time-inhomogeneous DTA-GLM analysis

- `TL-A_time-inhomogeneous_GLM_posterior_coefficient_booster_uptake.R` R script for visualising the posterior distribution of coefficient * inclusion probability of predictors considered in the time-inhomogeneous DTA-GLM analysis of Transmission Lineage-A, with booster uptake at origin/destination as additional covariates

### [XMLs](XMLs/)
This contains XMLs for running the discrete phylogeographic analyses with GLM for the 8 largest local Omicron BA.1 transmission lineages identified from phylogenetics. Note that these XMLs have to be run with a recent build of the hmc-clock branch of BEAST (https://github.com/beast-dev/beast-mcmc/tree/hmc-clock) and a recent build of the hmc-clock branch of BEAGLE (https://github.com/beagle-dev/beagle-lib/tree/hmc-clock). Some of the epoch analyses may require increasing the number of convolution buffers through the `-beagle_extra_buffer_count` argument.

Structure of the directory is shown below:

```
├── time-homogeneous
│   ├── BA.1_DTA_1207_n9727.xml
│   ├── BA.1.15_DTA_102_n2975.xml
│   ├── BA.1.17_DTA_175_n11351.xml
│   └── combined5.xml
└── time-inhomogeneous
    ├── BA.1_DTA_1207_n9727.xml
    ├── BA.1.15_DTA_102_n2975.xml
    ├── BA.1.17_DTA_175_n11351.xml
    ├── combined5.xml
    └── covariate-importance
        ├── epoch1
        │   ├── BA.1_DTA_1207_n9727.xml
        │   ├── BA.1.15_DTA_102_n2975.xml
        │   ├── BA.1.17_DTA_175_n11351.xml
        │   └── combined5.xml
        └── epoch2
            ├── BA.1_DTA_1207_n9727.xml
            ├── BA.1.15_DTA_102_n2975.xml
            ├── BA.1.17_DTA_175_n11351.xml
            └── combined5.xml
```
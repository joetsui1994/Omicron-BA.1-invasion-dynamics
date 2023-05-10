library(dplyr)
library(ggplot2)

## read in LTLA population
ltla_pop.df <- read.csv('./data/ONS_mid2020_LTLA_population_5yr-age-breakdown.csv')
ltla_pop.df <- ltla_pop.df %>%
  rename(ltla=code)

## calculate age proportions
ltla_pop.df <- ltla_pop.df %>%
  mutate(
    X0_4.p=X0_4/total,
    X5_11.p=X5_11/total,
    X12_15.p=X12_15/total,
    X16_17.p=X16_17/total,
    X18_24.p=X18_24/total,
    X25_29.p=X25_29/total,
    X30_34.p=X30_34/total,
    X35_39.p=X35_39/total,
    X40_44.p=X40_44/total,
    X45_49.p=X45_49/total,
    X50_54.p=X50_54/total,
    X55_59.p=X55_59/total,
    X60_64.p=X60_64/total,
    X65_69.p=X65_69/total,
    X70_74.p=X70_74/total,
    X75_79.p=X75_79/total,
    X80_84.p=X80_84/total,
    X85_89.p=X85_89/total,
    X90_999.p=X90_999/total
  )

## pivot ltla_pop.df into long format
## containing only population (excluding proportion)
ltla_pop_long.df <- ltla_pop.df %>%
  select(names(ltla_pop.df)[1:20]) %>%
  pivot_longer(cols=names(ltla_pop.df)[2:20], names_to='age', values_to='pop')
ltla_pop_long.df$age_min <- sapply(
  ltla_pop_long.df$age, function(x) as.integer(strsplit(substr(x, 2, nchar(x)), '_')[[1]][1]))
ltla_pop_long.df$age_max <- sapply(
  ltla_pop_long.df$age, function(x) as.integer(strsplit(substr(x, 2, nchar(x)), '_')[[1]][2]))
ltla_pop_long.df <- ltla_pop_long.df %>% select(-age)

## calculate populated-weighted average age structure
total_pop <- sum(ltla_pop.df$total)
ltla_pop_avg.df <- data.frame(
  X0_4.p=sum(ltla_pop.df$X0_4.p*ltla_pop.df$total)/total_pop,
  X5_11.p=sum(ltla_pop.df$X5_11.p*ltla_pop.df$total)/total_pop,
  X12_15.p=sum(ltla_pop.df$X12_15.p*ltla_pop.df$total)/total_pop,
  X16_17.p=sum(ltla_pop.df$X16_17.p*ltla_pop.df$total)/total_pop,
  X18_24.p=sum(ltla_pop.df$X18_24.p*ltla_pop.df$total)/total_pop,
  X25_29.p=sum(ltla_pop.df$X25_29.p*ltla_pop.df$total)/total_pop,
  X30_34.p=sum(ltla_pop.df$X30_34.p*ltla_pop.df$total)/total_pop,
  X35_39.p=sum(ltla_pop.df$X35_39.p*ltla_pop.df$total)/total_pop,
  X40_44.p=sum(ltla_pop.df$X40_44.p*ltla_pop.df$total)/total_pop,
  X45_49.p=sum(ltla_pop.df$X45_49.p*ltla_pop.df$total)/total_pop,
  X50_54.p=sum(ltla_pop.df$X50_54.p*ltla_pop.df$total)/total_pop,
  X55_59.p=sum(ltla_pop.df$X55_59.p*ltla_pop.df$total)/total_pop,
  X60_64.p=sum(ltla_pop.df$X60_64.p*ltla_pop.df$total)/total_pop,
  X65_69.p=sum(ltla_pop.df$X65_69.p*ltla_pop.df$total)/total_pop,
  X70_74.p=sum(ltla_pop.df$X70_74.p*ltla_pop.df$total)/total_pop,
  X75_79.p=sum(ltla_pop.df$X75_79.p*ltla_pop.df$total)/total_pop,
  X80_84.p=sum(ltla_pop.df$X80_84.p*ltla_pop.df$total)/total_pop,
  X85_89.p=sum(ltla_pop.df$X85_89.p*ltla_pop.df$total)/total_pop,
  X90_999.p=sum(ltla_pop.df$X90_999.p*ltla_pop.df$total)/total_pop
)
## pivot ltla_pop_avg.df into long format
ltla_pop_avg_long.df <- ltla_pop_avg.df %>%
  pivot_longer(cols=names(ltla_pop_avg.df), names_to='age', values_to='pop.p')
ltla_pop_avg_long.df$age_min <- sapply(
  ltla_pop_avg_long.df$age, function(x) as.integer(strsplit(substr(x, 2, nchar(x)-2), '_')[[1]][1]))
ltla_pop_avg_long.df$age_max <- sapply(
  ltla_pop_avg_long.df$age, function(x) as.integer(strsplit(substr(x, 2, nchar(x)-2), '_')[[1]][2]))
ltla_pop_avg_long.df <- ltla_pop_avg_long.df %>% select(-age)

## read in UK government age-specific vaccination data
uk_gov_age_specific_vac_raw.df <- read.csv('../data/GOV.UK_LTLA_vaccine_age_demographics.csv', sep=',')
uk_gov_age_specific_vac.df <- uk_gov_age_specific_vac_raw.df %>%
  select(areaCode, date, age,
         cumPeopleVaccinatedThirdInjectionByVaccinationDate,
         newPeopleVaccinatedThirdInjectionByVaccinationDate,
         cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage) %>%
  rename(
    ltla=areaCode,
    cumPeopleVaccinated=cumPeopleVaccinatedThirdInjectionByVaccinationDate,
    newPeopleVaccinated=newPeopleVaccinatedThirdInjectionByVaccinationDate,
    cumUptakePercent=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage
  )
uk_gov_age_specific_vac.df$date <- as.Date(uk_gov_age_specific_vac.df$date)
uk_gov_age_specific_vac_ext.df <- uk_gov_age_specific_vac.df %>%
  filter(date >= as.Date('2021-10-01') &
           date <= as.Date('2022-01-31') &
           startsWith(ltla, 'E0'))
## assume 0 for 05_11
uk_gov_age_specific_vac_ext.df[uk_gov_age_specific_vac_ext.df$age == '05_11',
                               ]$cumUptakePercent <- 0
## ignore 50+ and 75+
uk_gov_age_specific_vac_ext.df <- uk_gov_age_specific_vac_ext.df %>%
  filter(!age %in% c('50+', '75+'))
## convert age column from string to numeric
uk_gov_age_specific_vac_ext.df$age_min <- sapply(uk_gov_age_specific_vac_ext.df$age,
                                                 function(x) as.integer(unlist(strsplit(x, '_'))[1]))
uk_gov_age_specific_vac_ext.df$age_max <- sapply(uk_gov_age_specific_vac_ext.df$age,
                                                 function(x) as.integer(unlist(strsplit(x, '_'))[2]))
uk_gov_age_specific_vac_ext.df <- uk_gov_age_specific_vac_ext.df %>%
  mutate(
    age_min=ifelse(age == '90+', 90, age_min),
    age_max=ifelse(age == '90+', 999, age_max),
  )

## make plot of booster uptake vs age for all LTLAs
## 2021-12-25
uk_gov_age_specific_vac_ext.20211225.df <- uk_gov_age_specific_vac_ext.df %>%
  filter(date == as.Date('2021-12-25'))
uk_gov_age_specific_vac_ext.20211225.df$age_min <- as.factor(uk_gov_age_specific_vac_ext.20211225.df$age_min)
ggplot() +
  geom_boxplot(dat=uk_gov_age_specific_vac_ext.20211225.df,
               aes(x=age_min, y=cumUptakePercent/100),
               alpha=0.9, color='#484848', width=0.6
  ) +
  scale_x_discrete(labels=c('5-11', '12-15', '16-17', '18-24', '25-29', '30-34', '35-39', '40-44',
                            '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84',
                            '85-89', '90+')) +
  scale_y_continuous(limits=c(0, 1)) +
  labs(x='Age', y='Proportion of population who have received\na booster dose by 2021-12-25') +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    panel.grid.major.y = element_line(colour='black', linewidth=0.05),
    axis.title.y = element_text(vjust=2, size=13, color='black'),
    axis.title.x = element_text(size=13, vjust=-0.4, color='black'),
    axis.text.x = element_text(size=10, angle=90, vjust=0.5, hjust=1),
    axis.text.y = element_text(size=10),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/booster_uptake-age_20211225.pdf',
       device='pdf', width=7.6, height=4.5)

## 2022-01-31
uk_gov_age_specific_vac_ext.20220131.df <- uk_gov_age_specific_vac_ext.df %>%
  filter(date == as.Date('2022-01-31'))
uk_gov_age_specific_vac_ext.20220131.df$age_min <- as.factor(uk_gov_age_specific_vac_ext.20220131.df$age_min)
ggplot() +
  geom_boxplot(dat=uk_gov_age_specific_vac_ext.20220131.df,
               aes(x=age_min, y=cumUptakePercent/100),
               alpha=0.9, color='#484848', width=0.6
  ) +
  scale_x_discrete(labels=c('5-11', '12-15', '16-17', '18-24', '25-29', '30-34', '35-39', '40-44',
                            '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84',
                            '85-89', '90+')) +
  scale_y_continuous(limits=c(0, 1)) +
  labs(x='Age', y='Proportion of population who have received\na booster dose by 2022-01-31') +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    panel.grid.major.y = element_line(colour='black', linewidth=0.05),
    axis.title.y = element_text(vjust=2, size=13, color='black'),
    axis.title.x = element_text(size=13, vjust=-0.4, color='black'),
    axis.text.x = element_text(size=10, angle=90, vjust=0.5, hjust=1),
    axis.text.y = element_text(size=10),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/booster_uptake-age_20220131.pdf',
       device='pdf', width=7.6, height=4.5)

## check LTLA consistency (with population data as reference)
setdiff(unique(uk_gov_age_specific_vac_ext.df$ltla), unique(ltla_pop.df$ltla))
setdiff(unique(ltla_pop.df$ltla), unique(uk_gov_age_specific_vac_ext.df$ltla))
## merge legacy LTLAs
## legacy LTLAs: E07000007, E07000004, E07000006, E07000005
## new LTLA: E06000060 (Buckinghamshire)
uk_gov_age_specific_vac_ext.df <- uk_gov_age_specific_vac_ext.df %>%
  select(-cumUptakePercent, -age) %>%
  mutate(ltla=ifelse(ltla %in% c('E07000007', 'E07000004', 'E07000006', 'E07000005'),
                     'E06000060', ltla)) %>%
  group_by(ltla, date, age_min, age_max) %>%
  summarise(
    cumPeopleVaccinated=sum(cumPeopleVaccinated),
    newPeopleVaccinated=sum(newPeopleVaccinated),
  )
## disaggregate E09000012 (Hackney and City of London) into E09000012 (Hackney) and E09000001 (City of London)
## weighted by population
## calculate population ratio between E09000012 and E09000001 for each age bin
E09000012_E09000001_age_specific_pop_r.df <- ltla_pop_long.df %>%
  filter(ltla == 'E09000001') %>%
  rename(pop.E09000001=pop) %>%
  select(-ltla) %>%
  left_join(ltla_pop_long.df %>%
              filter(ltla == 'E09000012') %>%
              rename(pop.E09000012=pop) %>%
              select(-ltla), by=c('age_min', 'age_max')) %>%
  mutate(
    total.pop=pop.E09000001+pop.E09000012,
    pop.r=pop.E09000001/total.pop,
    pop.r_r=pop.E09000012/total.pop,
  ) %>%
  select(age_min, age_max, total.pop, pop.E09000001, pop.E09000012, pop.r, pop.r_r)
## calculate age-specific booster uptake for E09000001 from E09000012
E09000001_age_specific_vac.df <- uk_gov_age_specific_vac_ext.df %>%
  filter(ltla == 'E09000012') %>%
  left_join(E09000012_E09000001_age_specific_pop_r.df, by=c('age_min', 'age_max')) %>%
  mutate(
    cumPeopleVaccinated=cumPeopleVaccinated*pop.r,
    newPeopleVaccinated=newPeopleVaccinated*pop.r,
    ltla='E09000001'
  ) %>%
  select(ltla, date, age_min, age_max, cumPeopleVaccinated, newPeopleVaccinated)
## calculate age-specific booster uptake for E09000001 from E09000012
E09000012_age_specific_vac.df <- uk_gov_age_specific_vac_ext.df %>%
  filter(ltla == 'E09000012') %>%
  left_join(E09000012_E09000001_age_specific_pop_r.df, by=c('age_min', 'age_max')) %>%
  mutate(
    cumPeopleVaccinated=cumPeopleVaccinated*pop.r_r,
    newPeopleVaccinated=newPeopleVaccinated*pop.r_r,
  ) %>%
  select(ltla, date, age_min, age_max, cumPeopleVaccinated, newPeopleVaccinated)
## remove original and add new
uk_gov_age_specific_vac_ext_E09000012_disagg.df <- rbind(
  uk_gov_age_specific_vac_ext.df %>%
    filter(!ltla %in% c('E09000001', 'E09000012')),
  E09000001_age_specific_vac.df, E09000012_age_specific_vac.df)
uk_gov_age_specific_vac_ext_E09000012_disagg.df <- uk_gov_age_specific_vac_ext_E09000012_disagg.df %>%
  arrange(ltla, date)
## check LTLA consistency again (with population data as reference)
setdiff(unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$ltla), unique(ltla_pop.df$ltla))
setdiff(unique(ltla_pop.df$ltla), unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$ltla))

## calculate booster uptake (as % of population)
## add 0-4 age group
uk_gov_age_specific_vac_ext_E09000012_disagg_0_4_added.df <- rbind(
  uk_gov_age_specific_vac_ext_E09000012_disagg.df,
  data.frame(
    ltla=rep(unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$ltla),
             each=length(unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$date))),
    date=rep(unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$date),
             length(unique(uk_gov_age_specific_vac_ext_E09000012_disagg.df$ltla))),
    age_min=0,
    age_max=4,
    cumPeopleVaccinated=0,
    newPeopleVaccinated=0
  )
)
uk_gov_age_specific_vac_ext_E09000012_disagg_0_4_added.df <- uk_gov_age_specific_vac_ext_E09000012_disagg_0_4_added.df %>%
  arrange(ltla, date, age_min)
## merge with age-specific population dataframe and calculate age-specific booster uptake
uk_gov_age_specific_booster_uptake.df <- uk_gov_age_specific_vac_ext_E09000012_disagg_0_4_added.df %>%
  left_join(ltla_pop_long.df, by=c('ltla', 'age_min', 'age_max')) %>%
  mutate(cumPeopleVaccinated.p=cumPeopleVaccinated/pop)
## calculate overall booster uptake (assuming population-weighted average age structure)
uk_gov_avg_age_structure_booster_uptake.df <- uk_gov_age_specific_booster_uptake.df %>%
  left_join(ltla_pop_avg_long.df, by=c('age_min', 'age_max')) %>%
  group_by(ltla, date) %>%
  summarise(cumPeopleVaccinated.p=sum(cumPeopleVaccinated.p*pop.p))

## read in UK gov COVID-19 dashboard data
uk_gov_case_dat.df <- read.csv(file = "./data/master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv")
uk_gov_case_dat.df <- uk_gov_case_dat.df %>%
  rename(total_omicron_cases = omicron_cases_confirmed_by_SGTF) %>%
  mutate(samples = total_omicron_cases + Non_omicron) %>%
  select(LTLA_code, specimen_date, Total_number_of_cases,
         total_omicron_cases, samples) %>%
  rename(
    date=specimen_date,
    ltla=LTLA_code)
## calculate BA.1-specific case count
uk_gov_case_dat.df <- uk_gov_case_dat.df %>%
  mutate(prob = total_omicron_cases/samples ,
         prob_mean=(1+total_omicron_cases)/(2+samples),
         count_mean=((1+total_omicron_cases)/(2+samples))*Total_number_of_cases,
         prob_LL=qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_LL=(qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases,
         prob_UL=qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_UL=(qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases)
uk_gov_case_dat.df$date <- as.Date(uk_gov_case_dat.df$date)

## check consistency of LTLAs
setdiff(unique(uk_gov_avg_age_structure_booster_uptake.df$ltla), unique(uk_gov_case_dat.df$ltla))
setdiff(unique(uk_gov_case_dat.df$ltla), unique(uk_gov_avg_age_structure_booster_uptake.df$ltla))

## make plot of cumulative case counts vs age (>65)
## 2021-12-25
plot_cum_BA.1_age.20211225.df <- uk_gov_case_dat.df %>%
  filter(date <= as.Date('2021-12-25')) %>%
  group_by(ltla) %>%
  mutate(
    cum_count_mean=cumsum(count_mean)
  ) %>%
  select(ltla, date, count_mean, cum_count_mean) %>%
  filter(date == as.Date('2021-12-25')) %>%
  select(-date) %>%
  left_join(unique(ltla_pop_long.df %>%
                     mutate(age_bin=ifelse(age_min < 65, 'young', 'old')) %>%
                     group_by(ltla) %>%
                     mutate(total_pop=sum(pop)) %>%
                     group_by(ltla, age_bin) %>%
                     summarise(
                       pop=sum(pop),
                       pop.p=pop/total_pop,
                       total_pop=total_pop)), by='ltla')
ggplot() +
  geom_point(dat=plot_cum_BA.1_age.20211225.df %>%
               filter(age_bin == 'old' & ltla != 'E06000053'),
             aes(x=log10(pop.p), y=log10(cum_count_mean/total_pop)),
             color='#484848', alpha=0.9, size=1.1) +
  geom_smooth(dat=plot_cum_BA.1_age.20211225.df %>%
                filter(age_bin == 'old' & ltla != 'E06000053'),
              aes(x=log10(pop.p), y=log10(cum_count_mean/total_pop)),
              method='lm', formula=y~x,
              color='#1B7883', fill='#484848', alpha=0.1, linewidth=1.3) +
  labs(x='Proportion of population aged > 65 (log10)',
       y='Cumulative number of Omicron BA.1 cases\nper capita by 2021-12-25 (log10)') +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    axis.title.y = element_text(vjust=2, size=13, color='black'),
    axis.title.x = element_text(size=13, vjust=-0.4, color='black'),
    axis.text.x = element_text(size=10, angle=90, vjust=0.5, hjust=1),
    axis.text.y = element_text(size=10),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/cumulative_BA.1_cases_vs_population_65_20211225.pdf',
       device='pdf', width=5.04, height=4.5)

## 2022-01-31
plot_cum_BA.1_age.20220131.df <- uk_gov_case_dat.df %>%
  filter(date <= as.Date('2022-01-31')) %>%
  group_by(ltla) %>%
  mutate(
    cum_count_mean=cumsum(count_mean)
  ) %>%
  select(ltla, date, count_mean, cum_count_mean) %>%
  filter(date == as.Date('2022-01-31')) %>%
  select(-date) %>%
  left_join(unique(ltla_pop_long.df %>%
                     mutate(age_bin=ifelse(age_min < 65, 'young', 'old')) %>%
                     group_by(ltla) %>%
                     mutate(total_pop=sum(pop)) %>%
                     group_by(ltla, age_bin) %>%
                     summarise(
                       pop=sum(pop),
                       pop.p=pop/total_pop,
                       total_pop=total_pop)), by='ltla')
ggplot() +
  geom_point(dat=plot_cum_BA.1_age.20220131.df %>%
               filter(age_bin == 'old' & ltla != 'E06000053'),
             aes(x=log10(pop.p), y=log10(cum_count_mean/total_pop)),
             color='#484848', alpha=0.9, size=1.1) +
  geom_smooth(dat=plot_cum_BA.1_age.20220131.df %>%
                filter(age_bin == 'old' & ltla != 'E06000053'),
              aes(x=log10(pop.p), y=log10(cum_count_mean/total_pop)),
              method='lm', formula=y~x,
              color='#1B7883', fill='#484848', alpha=0.1, linewidth=1.3) +
  labs(x='Proportion of population aged > 65 (log10)',
       y='Cumulative number of Omicron BA.1 cases\nper capita by 2022-01-31 (log10)') +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    axis.title.y = element_text(vjust=2, size=13, color='black'),
    axis.title.x = element_text(size=13, vjust=-0.4, color='black'),
    axis.text.x = element_text(size=10, angle=90, vjust=0.5, hjust=1),
    axis.text.y = element_text(size=10),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/cumulative_BA.1_cases_vs_population_65_20220131.pdf',
       device='pdf', width=4.96, height=4.5)

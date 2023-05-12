library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyr)
library(stats)
library(readr)
library(HDInterval)

########################################################################
## read in estimated BA.1 case incidence and aggregate national
########################################################################
omicron_case_dat.df <- read.csv("./data/master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv", sep=',')
omicron_case_dat.df <- omicron_case_dat.df %>% rename(total_omicron_cases = omicron_cases_confirmed_by_SGTF)
omicron_case_dat.df$samples = omicron_case_dat.df$total_omicron_cases + omicron_case_dat.df$Non_omicron
omicron_case_dat.eng.df <- omicron_case_dat.df %>%
  group_by(specimen_date) %>%
  summarise(
    Total_number_of_cases = sum(Total_number_of_cases),
    total_omicron_cases = sum(total_omicron_cases),
    Non_omicron = sum(Non_omicron),
    samples = sum(samples),
    est_omicron_cases = Total_number_of_cases*(total_omicron_cases/samples)
  )
omicron_case_dat.eng.df <- omicron_case_dat.eng.df %>%
  mutate(
    est_omicron_cases_rolling_avg = rollmean(est_omicron_cases, 7, na.pad=T)
  )

########################################################################
## read in importation events identified from the MCC tree
########################################################################
## BA.1.15
BA115_mcc_tls.df <- read.csv('./data/mcc_tls/BA.1.15_mcc_tls.tsv', sep='\t')
BA115_mcc_tls.df <- BA115_mcc_tls.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est))
  )
## BA.1.17
BA117_mcc_tls.df <- read.csv('./data/mcc_tls/BA.1.17_mcc_tls.tsv', sep='\t')
BA117_mcc_tls.df <- BA117_mcc_tls.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est))
  )
## BA.1.1
BA11_mcc_tls.df <- read.csv('./data/mcc_tls/BA.1.1_mcc_tls.tsv', sep='\t')
BA11_mcc_tls.df <- BA11_mcc_tls.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est))
  )
## BA.1
BA1_mcc_tls.df <- read.csv('./data/mcc_tls/BA.1_mcc_tls.tsv', sep='\t')
BA1_mcc_tls.df <- BA1_mcc_tls.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est))
  )

## create aggregated dataframe for all temporal data
all_mcc_tls.df <- rbind(BA115_mcc_tls.df %>% select(ntaxa, tmrca, tmrca_date, ptmrca, ptmrca_date,
                                                    first_seen, first_seen_date, last_seen, last_seen_date,
                                                    import_est, import_est_date),
                        BA117_mcc_tls.df %>% select(ntaxa, tmrca, tmrca_date, ptmrca, ptmrca_date,
                                                    first_seen, first_seen_date, last_seen, last_seen_date,
                                                    import_est, import_est_date),
                        BA11_mcc_tls.df %>% select(ntaxa, tmrca, tmrca_date, ptmrca, ptmrca_date,
                                                   first_seen, first_seen_date, last_seen, last_seen_date,
                                                   import_est, import_est_date),
                        BA1_mcc_tls.df %>% select(ntaxa, tmrca, tmrca_date, ptmrca, ptmrca_date,
                                                  first_seen, first_seen_date, last_seen, last_seen_date,
                                                  import_est, import_est_date))

## bin tls by pre-, during-, or post-travel-restrictions
all_mcc_tls.df <- all_mcc_tls.df %>%
  mutate(
    import_prepost = ifelse(import_est_date < as.Date('2021-11-26'), 'pre',
                            ifelse(import_est_date > as.Date('2021-12-15'), 'post', 'during')),
  )

########################################################################################
## read in importation events identified from posterior trees for each subtype
## BA.1.15
BA115_ptls <- read.csv('./data/posterior_tls/BA.1.15_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA115_ptls <- BA115_ptls %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
    ntaxa_grp = ifelse(ntaxa == 1, 'singleton',
                       ifelse(ntaxa < 5, 'small',
                              ifelse(ntaxa >= 5 & ntaxa <= 50, 'medium', 'large')))
  )
## all
BA115_ptls.all.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls$p_lab))),
  p_lab=rep(unique(BA115_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.all.daily[is.na(BA115_ptls.all.daily$count),]$count <- 0
## singleton
BA115_ptls.singleton.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls$p_lab))),
  p_lab=rep(unique(BA115_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls %>%
      filter(ntaxa_grp == 'singleton') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.singleton.daily[is.na(BA115_ptls.singleton.daily$count),]$count <- 0
## small
BA115_ptls.small.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls$p_lab))),
  p_lab=rep(unique(BA115_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls %>%
      filter(ntaxa_grp == 'small') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.small.daily[is.na(BA115_ptls.small.daily$count),]$count <- 0
## medium
BA115_ptls.medium.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls$p_lab))),
  p_lab=rep(unique(BA115_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls %>%
      filter(ntaxa_grp == 'medium') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.medium.daily[is.na(BA115_ptls.medium.daily$count),]$count <- 0
## large
BA115_ptls.large.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls$p_lab))),
  p_lab=rep(unique(BA115_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls %>%
      filter(ntaxa_grp == 'large') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.large.daily[is.na(BA115_ptls.large.daily$count),]$count <- 0

## BA.1.17
BA117_ptls <- read.csv('./data/posterior_tls/BA.1.17_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA117_ptls <- BA117_ptls %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
    ntaxa_grp = ifelse(ntaxa == 1, 'singleton',
                       ifelse(ntaxa < 5, 'small',
                              ifelse(ntaxa >= 5 & ntaxa <= 50, 'medium', 'large')))
  )
## all
BA117_ptls.all.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls$p_lab))),
  p_lab=rep(unique(BA117_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA117_ptls %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.all.daily[is.na(BA117_ptls.all.daily$count),]$count <- 0
## singleton
BA117_ptls.singleton.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls$p_lab))),
  p_lab=rep(unique(BA117_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA117_ptls %>%
      filter(ntaxa_grp == 'singleton') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.singleton.daily[is.na(BA117_ptls.singleton.daily$count),]$count <- 0
## small
BA117_ptls.small.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls$p_lab))),
  p_lab=rep(unique(BA117_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))
) %>%
  left_join(
    BA117_ptls %>%
      filter(ntaxa_grp == 'small') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.small.daily[is.na(BA117_ptls.small.daily$count),]$count <- 0
## medium
BA117_ptls.medium.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls$p_lab))),
  p_lab=rep(unique(BA117_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))
) %>%
  left_join(
    BA117_ptls %>%
      filter(ntaxa_grp == 'medium') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.medium.daily[is.na(BA117_ptls.medium.daily$count),]$count <- 0
## large
BA117_ptls.large.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls$p_lab))),
  p_lab=rep(unique(BA117_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))
) %>%
  left_join(
    BA117_ptls %>%
      filter(ntaxa_grp == 'large') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.large.daily[is.na(BA117_ptls.large.daily$count),]$count <- 0

## BA.1.1
BA11_ptls <- read.csv('./data/posterior_tls/BA.1.1_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA11_ptls <- BA11_ptls %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
    ntaxa_grp = ifelse(ntaxa == 1, 'singleton',
                       ifelse(ntaxa < 5, 'small',
                              ifelse(ntaxa >= 5 & ntaxa <= 50, 'medium', 'large')))
  )
## all
BA11_ptls.all.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls$p_lab))),
  p_lab=rep(unique(BA11_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.all.daily[is.na(BA11_ptls.all.daily$count),]$count <- 0
## singleton
BA11_ptls.singleton.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls$p_lab))),
  p_lab=rep(unique(BA11_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls %>%
      filter(ntaxa_grp == 'singleton') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.singleton.daily[is.na(BA11_ptls.singleton.daily$count),]$count <- 0
## small
BA11_ptls.small.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls$p_lab))),
  p_lab=rep(unique(BA11_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls %>%
      filter(ntaxa_grp == 'small') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.small.daily[is.na(BA11_ptls.small.daily$count),]$count <- 0
## medium
BA11_ptls.medium.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls$p_lab))),
  p_lab=rep(unique(BA11_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls %>%
      filter(ntaxa_grp == 'medium') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.medium.daily[is.na(BA11_ptls.medium.daily$count),]$count <- 0
## large
BA11_ptls.large.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls$p_lab))),
  p_lab=rep(unique(BA11_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls %>%
      filter(ntaxa_grp == 'large') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.large.daily[is.na(BA11_ptls.large.daily$count),]$count <- 0

## BA.1
BA1_ptls <- read.csv('./data/posterior_tls/BA.1_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA1_ptls <- BA1_ptls %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
    ntaxa_grp = ifelse(ntaxa == 1, 'singleton',
                       ifelse(ntaxa < 5, 'small',
                              ifelse(ntaxa >= 5 & ntaxa <= 50, 'medium', 'large')))
  )
## all
BA1_ptls.all.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls$p_lab))),
  p_lab=rep(unique(BA1_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.all.daily[is.na(BA1_ptls.all.daily$count),]$count <- 0
## singleton
BA1_ptls.singleton.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls$p_lab))),
  p_lab=rep(unique(BA1_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls %>%
      filter(ntaxa_grp == 'singleton') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.singleton.daily[is.na(BA1_ptls.singleton.daily$count),]$count <- 0
## small
BA1_ptls.small.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls$p_lab))),
  p_lab=rep(unique(BA1_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls %>%
      filter(ntaxa_grp == 'small') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.small.daily[is.na(BA1_ptls.small.daily$count),]$count <- 0
## medium
BA1_ptls.medium.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls$p_lab))),
  p_lab=rep(unique(BA1_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls %>%
      filter(ntaxa_grp == 'medium') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.medium.daily[is.na(BA1_ptls.medium.daily$count),]$count <- 0
## large
BA1_ptls.large.daily <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls$p_lab))),
  p_lab=rep(unique(BA1_ptls$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls %>%
      filter(ntaxa_grp == 'large') %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.large.daily[is.na(BA1_ptls.large.daily$count),]$count <- 0

## merge all all.daily
all_ptls.all.daily <- BA115_ptls.all.daily %>%
  left_join(BA117_ptls.all.daily, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.all.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.all.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculate rolling average for each p_lab
all_ptls.all.daily <- all_ptls.all.daily %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.all.hpd.daily <- all_ptls.all.daily %>%
  group_by(date) %>%
  mutate(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## merge all singleton.daily
all_ptls.singleton.daily <- BA115_ptls.singleton.daily %>%
  left_join(BA117_ptls.singleton.daily, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.singleton.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.singleton.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculate rolling average for each p_lab
all_ptls.singleton.daily <- all_ptls.singleton.daily %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.singleton.hpd.daily <- all_ptls.singleton.daily %>%
  group_by(date) %>%
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## merge all small.daily
all_ptls.small.daily <- BA115_ptls.small.daily %>%
  left_join(BA117_ptls.small.daily, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.small.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.small.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculating rolling average for each p_lab
all_ptls.small.daily <- all_ptls.small.daily %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.small.hpd.daily <- all_ptls.small.daily %>%
  group_by(date) %>%
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## merge all medium.daily
all_ptls.medium.daily <- BA115_ptls.medium.daily %>%
  left_join(BA117_ptls.medium.daily, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.medium.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.medium.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculating rolling average for each p_lab
all_ptls.medium.daily <- all_ptls.medium.daily %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.medium.hpd.daily <- all_ptls.medium.daily %>%
  group_by(date) %>%
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## merge all large.daily
all_ptls.large.daily <- BA115_ptls.large.daily %>%
  left_join(BA117_ptls.large.daily, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.large.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.large.daily, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculating rolling average for each p_lab
all_ptls.large.daily <- all_ptls.large.daily %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.large.hpd.daily <- all_ptls.large.daily %>%
  group_by(date) %>%
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## calculate total daily (without CI)
all_ptls.all_sizes.median.daily <- all_ptls.singleton.hpd.daily %>%
  select(date, total.count.7day_rm.median) %>%
  left_join(all_ptls.small.hpd.daily %>%
              select(date, total.count.7day_rm.median), by='date') %>%
  rename(
    singleton.median=total.count.7day_rm.median.x,
    small.median=total.count.7day_rm.median.y
  ) %>%
  left_join(all_ptls.medium.hpd.daily %>%
              select(date, total.count.7day_rm.median), by='date') %>%
  rename(medium.median=total.count.7day_rm.median) %>%
  left_join(all_ptls.large.hpd.daily %>%
              select(date, total.count.7day_rm.median), by='date') %>%
  rename(large.median=total.count.7day_rm.median) %>%
  mutate(
    total.median=singleton.median+small.median+medium.median+large.median
  )

########################################################################
## read in sequence-level metadata attributed to TLs from MCC
########################################################################
## BA.1.15
BA115_mcc_tls_taxa.df <- read_delim('./data/mcc_tls_transformed/BA.1.15_transformed_taxa_metadata.tsv',
                                 delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA115_mcc_tls_taxa.df$sample_date <- sapply(BA115_mcc_tls_taxa.df$name,
                                            function(x) as.Date(unlist(strsplit(x, '\\|'))[3], format='%Y-%m-%d'))
BA115_mcc_tls_taxa.df$sample_date <- as.Date(BA115_mcc_tls_taxa.df$sample_date)

## BA.1.17
BA117_mcc_tls_taxa.df <- read_delim('./data/mcc_tls_transformed/BA.1.17_transformed_taxa_metadata.tsv',
                                 delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA117_mcc_tls_taxa.df$sample_date <- sapply(BA117_mcc_tls_taxa.df$name,
                                            function(x) as.Date(unlist(strsplit(x, '\\|'))[3], format='%Y-%m-%d'))
BA117_mcc_tls_taxa.df$sample_date <- as.Date(BA117_mcc_tls_taxa.df$sample_date)

## BA.1.1
BA11_mcc_tls_taxa.df <- read_delim('./data/mcc_tls_transformed/BA.1.1_transformed_taxa_metadata.tsv',
                                delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA11_mcc_tls_taxa.df$sample_date <- sapply(BA11_mcc_tls_taxa.df$name,
                                           function(x) as.Date(unlist(strsplit(x, '\\|'))[3], format='%Y-%m-%d'))
BA11_mcc_tls_taxa.df$sample_date <- as.Date(BA11_mcc_tls_taxa.df$sample_date)

## BA.1
BA1_mcc_tls_taxa.df <- read_delim('./data/mcc_tls_transformed/BA.1_transformed_taxa_metadata.tsv',
                               delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_mcc_tls_taxa.df$sample_date <- sapply(BA1_mcc_tls_taxa.df$name,
                                          function(x) as.Date(unlist(strsplit(x, '\\|'))[3], format='%Y-%m-%d'))
BA1_mcc_tls_taxa.df$sample_date <- as.Date(BA1_mcc_tls_taxa.df$sample_date)

## create combined dataframe
all_mcc_tls_taxa.df <- rbind(
  BA115_mcc_tls_taxa.df %>% select(sample_date, tmrca, ptmrca) %>% mutate(subtree='BA.1.15'),
  BA117_mcc_tls_taxa.df %>% select(sample_date, tmrca, ptmrca) %>% mutate(subtree='BA.1.17'),
  BA11_mcc_tls_taxa.df %>% select(sample_date, tmrca, ptmrca) %>% mutate(subtree='BA.1.1'),
  BA1_mcc_tls_taxa.df %>% select(sample_date, tmrca, ptmrca) %>% mutate(subtree='BA.1')
)
all_mcc_tls_taxa.df <- all_mcc_tls_taxa.df %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal((tmrca + ptmrca)/2)),
    import_pre.post.during = ifelse(import_est_date < as.Date('2021-11-26'), 'pre',
                            ifelse(import_est_date > as.Date('2021-12-15'), 'post', 'during')),
  )

## calculate daily proportion of pre/during/post
all_mcc_tls_taxa.daily.df <- data.frame(
  date = seq(as.Date('2021-11-01'), as.Date('2022-01-31'), by=1)) %>%
  left_join(
    all_mcc_tls_taxa.df %>%
      group_by(sample_date) %>%
      summarise(
        pre.count=sum(import_pre.post.during == 'pre'),
        during.count=sum(import_pre.post.during == 'during'),
        post.count=sum(import_pre.post.during == 'post')
      ) %>%
      mutate(
        total.count=pre.count + during.count + post.count,
        pre.p = pre.count/total.count,
        during.p = during.count/total.count,
        post.p = post.count/total.count
      ) %>%
      rename(date=sample_date), by='date'
  )
all_mcc_tls_taxa.daily.df[!complete.cases(all_mcc_tls_taxa.daily.df),
                          c("pre.count", "during.count", "post.count", "total.count",
                            "pre.p", "during.p", "post.p")] <- 0

## merge with estimated case numbers
all_mcc_tls_taxa.daily.omicron_cases.df <- all_mcc_tls_taxa.daily.df %>%
  left_join(
    omicron_case_dat.eng.df %>%
      rename(date=specimen_date) %>%
      mutate(date=as.Date(date)) %>%
      select(date, est_omicron_cases, est_omicron_cases_rolling_avg), by='date'
  ) %>%
  mutate(
    pre.cases=pre.p*est_omicron_cases,
    during.cases=during.p*est_omicron_cases,
    post.cases=post.p*est_omicron_cases
  ) %>%
  select(date, pre.cases, during.cases, post.cases) %>%
  rename(
    c.pre=pre.cases,
    c.during = during.cases,
    c.post = post.cases
  ) %>%
  select(date, c.pre, c.during, c.post) %>%
  pivot_longer(-date, names_to='c.bin')
all_mcc_tls_taxa.daily.omicron_cases.df$c.bin <- factor(
  all_mcc_tls_taxa.daily.omicron_cases.df$c.bin,
  levels=c('c.post', 'c.during', 'c.pre')
)

########################################################################
## make plots
########################################################################
cases_max_y <- 210000
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = cases_max_y
)
stripes[8, 'xmax'] <- as.Date('2022-02-03')
travel_ban <- data.frame(
  xmin = c(as.Date('2021-11-26')),
  xmax = c(as.Date('2021-12-15')),
  ymin = 0,
  ymax = cases_max_y
)
tl_ntaxa_line.colors <- c('#b52e2a', '#bf612e', '#d19824', '#6F444B')
tl_ntaxa_ribbon.colors <- c('#D43F3A', '#E6824C', '#F8C55D', '#6F444B')
ribbon_alpha <- 0.3
linewidth <- 0.7
large_tls_mcc_plots.df <- all_mcc_tls.df %>%
  filter(ntaxa > 700) %>%
  arrange(import_est_date) %>%
  mutate(rank=row_number())
secondary_axis_factor <- 820
ggplot() +
  geom_errorbarh(data=large_tls_mcc_plots.df,
                 aes(xmin=ptmrca_date, xmax=tmrca_date, y=15000*rank), height=2000, alpha=0.8) +
  geom_point(data=large_tls_mcc_plots.df,
             aes(x=import_est_date, y=15000*rank), size=1) +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(data=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.3) +
  geom_bar(data=all_mcc_tls_taxa.daily.omicron_cases.df,
           aes(x=date, y=value, fill=c.bin),
           position='stack', stat='identity', color='black', width=1, alpha=0.75, size=0.15) +
  geom_ribbon(data=all_ptls.singleton.hpd.daily, aes(x=date,
                                                 ymin=total.count.7day_rm.lw*secondary_axis_factor,
                                                 ymax=total.count.7day_rm.up*secondary_axis_factor),
              fill=tl_ntaxa_ribbon.colors[4], alpha=ribbon_alpha) +
  geom_line(data=all_ptls.singleton.hpd.daily, aes(x=date,
                                               y=total.count.7day_rm.median*secondary_axis_factor),
            color=tl_ntaxa_line.colors[4], size=linewidth) +
  geom_ribbon(data=all_ptls.small.hpd.daily, aes(x=date,
                                             ymin=total.count.7day_rm.lw*secondary_axis_factor,
                                             ymax=total.count.7day_rm.up*secondary_axis_factor),
              fill=tl_ntaxa_ribbon.colors[3], alpha=ribbon_alpha) +
  geom_line(data=all_ptls.small.hpd.daily, aes(x=date,
                                           y=total.count.7day_rm.median*secondary_axis_factor),
            color=tl_ntaxa_line.colors[3], size=linewidth) +
  geom_ribbon(data=all_ptls.medium.hpd.daily, aes(x=date,
                                              ymin=total.count.7day_rm.lw*secondary_axis_factor,
                                              ymax=total.count.7day_rm.up*secondary_axis_factor),
              fill=tl_ntaxa_ribbon.colors[2], alpha=ribbon_alpha) +
  geom_line(data=all_ptls.medium.hpd.daily, aes(x=date,
                                            y=total.count.7day_rm.median*secondary_axis_factor),
            color=tl_ntaxa_line.colors[2], size=linewidth) +
  geom_ribbon(data=all_ptls.large.hpd.daily, aes(x=date,
                                             ymin=total.count.7day_rm.lw*secondary_axis_factor,
                                             ymax=total.count.7day_rm.up*secondary_axis_factor),
              fill=tl_ntaxa_ribbon.colors[1], alpha=ribbon_alpha) +
  geom_line(data=all_ptls.large.hpd.daily, aes(x=date,
                                           y=total.count.7day_rm.median*secondary_axis_factor),
            color=tl_ntaxa_line.colors[1], size=linewidth) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(
    name = "Estimated daily number of Omicron BA.1 cases",
    limits=c(0, cases_max_y),
    sec.axis = sec_axis(trans=~./secondary_axis_factor,
                        name="Daily number of inferred importations",
                        breaks=c(0, 50, 100, 150, 200, 250)),
    expand=c(0, 0)
  ) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-02'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_fill_manual(values=c('#3E4855', '#6F444B', '#1B7883')) +
  labs(x='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2.5),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black')
  )

## output plot to file
ggsave('./figures/omicron_ba.1_cases_importations_freq_tls_added.pdf',
       device='pdf', width=12, height=6)


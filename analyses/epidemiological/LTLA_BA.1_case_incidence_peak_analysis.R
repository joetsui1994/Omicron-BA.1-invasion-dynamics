library(dplyr)
library(readr)
library(ggplot2)
library(zoo)
library(stats)
library(pracma)

## Read master dataset (daily) from end of October 2021 to 31st January 2021
BA.1_cases_daily.dat <- read.csv(file = "../data/master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv")
BA.1_cases_daily.dat <- BA.1_cases_daily.dat %>%
  rename(total_omicron_cases=omicron_cases_confirmed_by_SGTF) %>%
  mutate(
    samples=total_omicron_cases + Non_omicron
  )
BA.1_cases_daily.dat <- BA.1_cases_daily.dat %>%
  mutate(prob=total_omicron_cases/samples ,
         prob_mean=(1+total_omicron_cases)/(2+samples),
         count_mean=((1+total_omicron_cases)/(2+samples))*Total_number_of_cases,
         prob_LL=qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_LL=(qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases,
         prob_UL=qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_UL=(qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases) %>%
  select(LTLA_code, specimen_date,
         Total_number_of_cases, total_omicron_cases, Non_omicron, samples,
         prob, prob_mean, prob_LL, prob_UL,
         count_mean, count_LL, count_UL) %>%
  rename(
    origin=LTLA_code,
    date=specimen_date
  )
BA.1_cases_daily.dat$date <- as.Date(BA.1_cases_daily.dat$date)

## calculate 7-day rolling averages
## restrict first peak to before 15 Jan 2022
## a small number of LTLAs have twin-peak, one at the beginning of January 2022 and one in late January or early Feb 2022
## we are only interested in the first peak here
BA.1_cases_daily.7day_rm.dat <- BA.1_cases_daily.dat %>%
  select(origin, date, count_mean) %>%
  group_by(origin) %>%
  mutate(cases_ma.7=rollmean(x=count_mean, 7, na.pad=T)) %>%
  filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-01-15')) %>%
  ungroup()
BA.1_cases_daily.7day_rm.dat <- BA.1_cases_daily.7day_rm.dat[complete.cases(BA.1_cases_daily.7day_rm.dat),]

## find first peak using findpeaks
BA.1_cases_daily.7day_rm.peaks <- BA.1_cases_daily.7day_rm.dat %>%
  group_by(origin) %>%
  summarise(peak.i=findpeaks(cases_ma.7, sortstr=TRUE)[1,2])
## map indices to date
peak.dates <- c()
for (i in 1:nrow(BA.1_cases_daily.7day_rm.peaks)) {
  origin <- unique(BA.1_cases_daily.7day_rm.peaks$origin)[i]
  peak.i <- as.numeric(BA.1_cases_daily.7day_rm.peaks[i,'peak.i'])
  peak.date <- BA.1_cases_daily.7day_rm.dat[BA.1_cases_daily.7day_rm.dat$origin == origin,]$date[peak.i]
  peak.dates <- c(peak.dates, peak.date)
}
BA.1_cases_daily.7day_rm.peaks$peak.date <- as.Date(peak.dates)

## plot timing of first peak and quantify uncertainty by looking at
## time-range of some factor lambda of peak value
peak_lambda = 0.85
BA.1_cases_daily.7day_rm.peaks <- BA.1_cases_daily.7day_rm.peaks %>%
  left_join(BA.1_cases_daily.7day_rm.dat %>%
              rename(peak.date=date) %>%
              select(-count_mean), by=c('origin', 'peak.date')) %>%
  mutate(peak.threshold=cases_ma.7*peak_lambda) %>%
  select(-peak.i)
## find "width" of peak in estimated BA.1 case incidence
## defined as the first and last day when the daily case incidence is greater than 85% of the peak case incidence
BA.1_cases_daily.7day_rm.peak_intervals <- BA.1_cases_daily.7day_rm.dat %>%
  left_join(BA.1_cases_daily.7day_rm.peaks %>%
              select(origin, peak.threshold), by='origin') %>%
  mutate(under_threshold=cases_ma.7 >= peak.threshold) %>%
  filter(under_threshold) %>%
  group_by(origin) %>%
  arrange(date) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  arrange(origin, date)
## a peak is considered to be "before" Christmas if the upper bound of the interval lies before Christmas
## .. "after" Christmas if the lower bound of the interval lies after Christmas
## .. "during" Christmas if the interval encloses Christmas
BA.1_cases_daily.7day_rm.peak_intervals.p <- data.frame(
  origin = unique(BA.1_cases_daily.7day_rm.peak_intervals$origin),
  peak_LL.date = BA.1_cases_daily.7day_rm.peak_intervals[seq(1, nrow(BA.1_cases_daily.7day_rm.peak_intervals), 2),]$date,
  peak_UL.date = BA.1_cases_daily.7day_rm.peak_intervals[seq(2, nrow(BA.1_cases_daily.7day_rm.peak_intervals), 2),]$date
  ) %>%
  mutate(
    peak_mid.date = peak_LL.date + floor((peak_UL.date - peak_LL.date)/2),
    relative_to_dec = ((peak_LL.date - as.Date('2021-12-01')) + (peak_UL.date - as.Date('2021-12-01')))/2,
    relative_to_christmas = ifelse(peak_UL.date < as.Date('2021-12-25'), 'before',
                                   ifelse(peak_LL.date > as.Date('2021-12-25'), 'after', 'during'))
  ) %>%
  arrange(relative_to_dec)

## output intermediate results to file
write.csv(BA.1_cases_daily.7day_rm.peak_intervals.p,
          './LTLA_BA.1_case_incidence_peak_out.csv', row.names=FALSE, quote=FALSE)




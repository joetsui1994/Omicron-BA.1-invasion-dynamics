library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(stats)
library(ggplot2)

############################################################
##### LTLAs to be omitted (Isle of Wight has no adjacent LTLAs)
##### South Tyneside (E08000023) has no mobility data
##### Isles of Scilly (E06000053) has no adjacent LTLAs and no mobility data (in study period)
##### City of London (E09000001) has no mobility data
############################################################
ignore.ltlas <- c('E09000001', 'E06000053', 'E08000023')

## read in LTLA population estimates
ltla_population.df <- read.csv('../../GitHub/data/spatial-geographic/ONS_mid2020_LTLA_population.csv', sep=",")

## read in results from Omicron cases incidence peak analysis
omicron_case_daily.peak_output <- read.csv('./LTLA_BA.1_case_incidence_peak_out.csv', sep=",")

## read master dataset (daily) from end of October 2021 to 31st January 2021
omicron_case_daily.dat <- read.csv(file = './master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv')
omicron_case_daily.dat <- omicron_case_daily.dat %>%
  rename(total_omicron_cases = omicron_cases_confirmed_by_SGTF) %>%
  mutate(
    samples = total_omicron_cases + Non_omicron
  )
omicron_case_daily.dat <- omicron_case_daily.dat %>%
  mutate(prob = total_omicron_cases/samples ,
         prob_mean = (1+total_omicron_cases)/(2+samples),
         count_mean = ((1+total_omicron_cases)/(2+samples))*Total_number_of_cases,
         prob_LL = qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_LL = (qbeta(0.025, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases,
         prob_UL = qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1),
         count_UL = (qbeta(0.975, 1+total_omicron_cases, samples - total_omicron_cases + 1))*Total_number_of_cases) %>%
  select(LTLA_code, specimen_date,
         Total_number_of_cases, total_omicron_cases, Non_omicron, samples,
         prob, prob_mean, prob_LL, prob_UL,
         count_mean, count_LL, count_UL) %>%
  rename(
    origin = LTLA_code,
    date = specimen_date
  )

## calculate 7-day rolling averages
omicron_case_daily.RA_added <- omicron_case_daily.dat %>%
  select(origin, date, count_mean) %>%
  group_by(origin) %>%
  mutate(cases_ma.7 = rollmean(x=count_mean, 7, na.pad=T)) %>%
  ungroup()
omicron_case_daily.RA_added$date <- as.Date(omicron_case_daily.RA_added$date)
omicron_case_daily.RA_added$origin <- as.character(omicron_case_daily.RA_added$origin)
omicron_case_daily.RA_added <- omicron_case_daily.RA_added[complete.cases(omicron_case_daily.RA_added),]
omicron_case_daily.RA_added <- merge(omicron_case_daily.RA_added,
                                     ltla.infos[c('origin', 'pop')], by='origin')
omicron_case_daily.RA_added <- omicron_case_daily.RA_added %>%
  mutate(
    count_mean_nm = count_mean/pop,
    cases_ma.7_nm = cases_ma.7/pop
  )
omicron_case_daily.RA_added <- merge(omicron_case_daily.RA_added,
                                     omicron_case_daily.peak_output[c('origin', 'relative_to_christmas')], by='origin')

## make plot
start_date <- as.Date('2021-11-05')
end_date <- as.Date('2022-02-06')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = 6
)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(data=omicron_case_daily.RA_added[omicron_case_daily.RA_added$date > '2021-11-01' &
                                               omicron_case_daily.RA_added$date < '2022-02-10' &
                                               !omicron_case_daily.RA_added$origin %in% ignore.ltlas,],
            aes(x=date, y=cases_ma.7_nm*1000, group=origin, color=relative_to_christmas), alpha=0.7, size=0.5) +
  scale_color_manual(values=c('#484848', '#D43F3A', '#F8C55D')) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, 6)) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  geom_vline(xintercept=as.Date('2021-12-25'), color='#b50000', size=0.7, linetype='dashed') +
  labs(x='', y='Estimated daily number of Omicron BA.1\ncases per 1000 people') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/daily_omicron_cases_rolling_avg.pdf',
       device='pdf', width=7.645, height=4.5)


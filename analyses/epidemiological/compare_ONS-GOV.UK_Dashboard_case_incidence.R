library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(zoo)

## read in LTLA-level population data
ltla_population.df <- read.csv('./data/ONS_mid2020_LTLA_population.csv', sep=',')
ltla_population.df <- ltla_population.df %>%
  select(code, total) %>%
  rename(ltla=code, population=total)

## read in ONS infection survey data
ONS_files <- list.files(path='./data/ONS_incidence_downloads', pattern='*xlsx', full.names=TRUE, recursive=FALSE)
ONS_date_ranges <- lapply(ONS_files, function(x) {
  start_date_str <- strsplit(x, '_')[[1]][7]
  end_date_str <- strsplit(strsplit(x, '_')[[1]][8], '.xlsx')[[1]][1]
  return(c(as.Date(start_date_str, '%d%B%Y'), as.Date(end_date_str, '%d%B%Y')))
})

## read into a single dataframe with records of start/end dates
ONS_dat.df <- data.frame()
i <- 1
for (ons_file in ONS_files) {
  print(i)
  tmp_1o.df <- data.frame(read_xlsx(ons_file, sheet='1o', skip=4))
  names(tmp_1o.df) <- tolower(names(tmp_1o.df))
  tmp_1o.df <- tmp_1o.df[complete.cases(tmp_1o.df),]
  tmp_1o.df['start_date'] <- ONS_date_ranges[[i]][1]
  tmp_1o.df['end_date'] <- ONS_date_ranges[[i]][2]
  tmp_1p.df <- data.frame(read_xlsx(ons_file, sheet='1p', skip=4))
  names(tmp_1p.df) <- tolower(names(tmp_1p.df))
  tmp_1p.df <- tmp_1p.df[complete.cases(tmp_1p.df),]
  tmp_1p.df['start_date'] <- ONS_date_ranges[[i]][1]
  tmp_1p.df['end_date'] <- ONS_date_ranges[[i]][2]
  if (names(tmp_1o.df)[1] == 'date') {
    ONS_dat.df <- rbind(ONS_dat.df, tmp_1o.df)
  } else if (names(tmp_1p.df)[1] == 'date') {
    ONS_dat.df <- rbind(ONS_dat.df, tmp_1p.df)
  } else {
    tmp_1o.df <- data.frame(read_xlsx(ons_file, sheet='1o', skip=6))
    names(tmp_1o.df) <- tolower(names(tmp_1o.df))
    tmp_1o.df <- tmp_1o.df[complete.cases(tmp_1o.df),]
    tmp_1o.df['start_date'] <- ONS_date_ranges[[i]][1]
    tmp_1o.df['end_date'] <- ONS_date_ranges[[i]][2]
    ONS_dat.df <- rbind(ONS_dat.df, tmp_1o.df)
  }
  i <- i + 1
}

## extract only relevant columns
ONS_dat_p.df <- ONS_dat.df %>%
  select(date, modelled.covid.19.incidence.rate.per.10.000.people.per.day,
         x95..lower.credible.interval...3, x95..upper.credible.interval...4, start_date, end_date) %>%
  mutate(date=as.Date(as.numeric(date), origin=as.Date('1899-12-30'))) %>%
  arrange(date, desc(start_date)) %>%
  group_by(date) %>%
  filter(row_number()==1) %>%
  rename(
    incidence_per_10000=modelled.covid.19.incidence.rate.per.10.000.people.per.day,
    incidence_lw=x95..lower.credible.interval...3,
    incidence_up=x95..upper.credible.interval...4
  ) %>%
  select(-start_date, end_date)

## 7-day rolling mean
ONS_dat_p.df$incidence_per_10000.rm <- rollmean(ONS_dat_p.df$incidence_per_10000, 7, fill=NA)
ONS_dat_p.df$incidence_lw.rm <- rollmean(ONS_dat_p.df$incidence_lw, 7, fill=NA)
ONS_dat_p.df$incidence_up.rm <- rollmean(ONS_dat_p.df$incidence_up, 7, fill=NA)

## convert to incidence per 1000
ONS_dat_p.df <- ONS_dat_p.df %>%
  mutate(
    incidence_per_1000=incidence_per_10000/10,
    incidence_per_1000.lw=incidence_lw/10,
    incidence_per_1000.up=incidence_up/10,
    incidence_per_1000.rm=incidence_per_10000.rm/10,
    incidence_per_1000.lw.rm=incidence_lw.rm/10,
    incidence_per_1000.up.rm=incidence_up.rm/10
  ) %>%
  select(-incidence_per_10000, -incidence_lw, -incidence_up, 
         -incidence_per_10000.rm, -incidence_lw.rm, -incidence_up.rm)

## a quick plot for sanity checks
ggplot() +
  geom_point(dat=ONS_dat_p.df %>%
               filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-02-01')),
             aes(x=date, y=incidence_per_1000))

########################################################
## read in UK gov COVID-19 dashboard data
uk_gov_dat.df <- read.csv(file = "./data/master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv")
uk_gov_dat.df <- uk_gov_dat.df %>% rename(total_omicron_cases = omicron_cases_confirmed_by_SGTF)
uk_gov_dat.df$date = as.Date(uk_gov_dat.df$specimen_date)

## calculate total (relevant) population
setdiff(unique(ltla_population.df$ltla), unique(uk_gov_dat.df$LTLA_code))
setdiff(unique(uk_gov_dat.df$LTLA_code), unique(ltla_population.df$ltla))
uk_gov_total_pop <- sum(ltla_population.df$population)

## aggregate at national level
uk_gov_dat_agg.df <- uk_gov_dat.df %>%
  rename(
    ltla=LTLA_code,
    total_cases=Total_number_of_cases
  ) %>%
  select(ltla, date, total_cases) %>%
  group_by(date) %>%
  summarise(
    total_cases=sum(total_cases)
  ) %>%
  mutate(
    total_cases.1_week.rm = rollmean(total_cases, 7, na.pad=T),
    total_cases.2_weeks.rm = rollmean(total_cases, 14, na.pad=T),
    cum_total_cases = cumsum(total_cases),
    total_cases.2_prev_weeks.rm = (cum_total_cases - lag(cum_total_cases, 14))/14
  )

## calculate cases per 1000
uk_gov_dat_agg.df <- uk_gov_dat_agg.df %>%
  mutate(
    total_cases.p=total_cases/uk_gov_total_pop,
    total_cases.1_week.rm.p=total_cases.1_week.rm*1000/uk_gov_total_pop,
    total_cases.2_weeks.rm.p=total_cases.2_weeks.rm*1000/uk_gov_total_pop,
    total_cases.2_prev_weeks.rm.p=total_cases.2_prev_weeks.rm*1000/uk_gov_total_pop
  )

########################################################
## plot ONS and UKGOV dashboard case incidence
## daily
ymin <- 0
ymax <- max(max((ONS_dat_p.df %>% filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-02-06')))$incidence_per_1000.up,
                na.rm=T), max(uk_gov_dat_agg.df$total_cases.1_week.rm.p), na.rm=T) + 0.8
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-15')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(dat=ONS_dat_p.df %>%
              filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-02-06')),
            aes(x=date, y=incidence_per_1000.rm),
            color='#0C4C5F', alpha=0.9, linewidth=1) +
  geom_ribbon(dat=ONS_dat_p.df %>%
                filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-02-06')),
              aes(x=date, ymax=incidence_per_1000.lw.rm, ymin=incidence_per_1000.up.rm,),
              alpha=0.3, fill='#0C4C5F') +
  geom_line(dat=uk_gov_dat_agg.df %>%
              filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-02-06')),
            aes(x=date, y=total_cases.1_week.rm.p),
            color='#484848', alpha=0.9, linewidth=1.6) +
  geom_vline(xintercept=as.Date('2021-12-25'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  geom_vline(xintercept=as.Date('2022-01-01'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, ymax)) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  labs(x='', y='\nDaily number of COVID19 cases per 1000 people') +
  coord_cartesian(clip='on', ylim=c(0, ymax), xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    # panel.grid.major.y = element_line(color='#484848', size=0.05),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(vjust=2, size=13),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.2, 0.2), "cm")
  )

## weekly
ONS_dat_p_weekly.df <- ONS_dat_p.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    incidence_per_1000.weekly=sum(incidence_per_1000),
    incidence_per_1000.weekly.lw=sum(incidence_per_1000.lw),
    incidence_per_1000.weekly.up=sum(incidence_per_1000.up)
  )
uk_gov_dat_agg_weekly.df <- uk_gov_dat_agg.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    total_cases.1_week.rm.p.weekly=sum(total_cases.1_week.rm.p)
  )
ymin <- 0
ymax <- 62
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-15')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(dat=ONS_dat_p_weekly.df %>%
              filter(week >= as.Date('2021-11-01') & week <= as.Date('2022-02-06')),
            aes(x=week, y=incidence_per_1000.weekly),
            color='#0C4C5F', alpha=0.9, linewidth=1) +
  geom_ribbon(dat=ONS_dat_p_weekly.df %>%
                filter(week >= as.Date('2021-11-01') & week <= as.Date('2022-02-06')),
              aes(x=week, ymax=incidence_per_1000.weekly.up, ymin=incidence_per_1000.weekly.lw,),
              alpha=0.3, fill='#0C4C5F') +
  geom_line(dat=uk_gov_dat_agg_weekly.df %>%
              filter(week >= as.Date('2021-11-01') & week <= as.Date('2022-02-06')),
            aes(x=week, y=total_cases.1_week.rm.p.weekly),
            color='#484848', alpha=0.9, linewidth=1.6) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, ymax)) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  labs(x='Week commencing', y='\nWeekly case incidence per 1000 people') +
  coord_cartesian(clip='on', ylim=c(0, ymax), xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/weekly_case_incidence_per_1000_compare.pdf',
       device='pdf', width=7.9, height=4.5)

########################################################
## find transformation that minimises their differences
## fit to central estimates of ONS incidence from beginning of November 2021 to end of January 2022 (roughly)
lm_fit_ONS_uk_gov_case_incidence.df <- ONS_dat_p_weekly.df %>%
  left_join(uk_gov_dat_agg_weekly.df, by='week') %>%
  mutate(
    y=incidence_per_1000.weekly,
    x=total_cases.1_week.rm.p.weekly
  )
lm_fit_ONS_uk_gov_case_incidence <- lm(y ~ x,
                                       dat=lm_fit_ONS_uk_gov_case_incidence.df %>%
                                         filter(week >= as.Date('2021-10-31') & week <= as.Date('2022-01-23'))
)
summary(lm_fit_ONS_uk_gov_case_incidence)

## compute residuals
ONS_rescaled_uk_gov_case_incidence_compare.df <- lm_fit_ONS_uk_gov_case_incidence.df %>%
  mutate(
    rs.diff = predict(lm_fit_ONS_uk_gov_case_incidence,
                      lm_fit_ONS_uk_gov_case_incidence.df) - lm_fit_ONS_uk_gov_case_incidence.df$y,
    rs.diff.p=rs.diff/y
  )

## regression plot
ggplot() +
  geom_smooth(dat=ONS_rescaled_uk_gov_case_incidence_compare.df %>%
                filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')),
              aes(x=x, y=y), method='lm', formula=y~x,
              color='#1B7883', fill='#484848', alpha=0.1, linewidth=1.3) +
  geom_point(dat=ONS_rescaled_uk_gov_case_incidence_compare.df %>%
               filter((week >= as.Date('2021-11-01') & week < as.Date('2021-11-28')) |
                        (week <= as.Date('2022-02-06') & week > as.Date('2022-01-23'))),
             aes(x=x, y=incidence_per_1000.weekly), shape=3, size=2) +
  geom_point(dat=ONS_rescaled_uk_gov_case_incidence_compare.df %>%
               filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')),
             aes(x=x, y=incidence_per_1000.weekly), size=2) +
  labs(x='GOV.UK Dashboard weekly case incidence per 1000 people', y='ONS weekly case incidence per 1000 people') +
  coord_cartesian(clip='on') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color='#484848', size=0.04),
    panel.grid.major.y = element_line(color='#484848', size=0.04),
    panel.border = element_rect(color='black', size=0.9, fill=NA),
    axis.title.y = element_text(vjust=2, size=16),
    axis.title.x = element_text(size=16, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/weekly_case_incidence_per_1000_compare_regression.pdf',
       # device='pdf', width=8, height=7.5)
       device='pdf', width=7, height=7.7)

## separate plot of residuals
ymin <- -1
ymax <- 1
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-15')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_bar(dat=ONS_rescaled_uk_gov_case_incidence_compare.df %>%
             mutate(sign=rs.diff.p >= 0),
           aes(x=week+3.5, y=rs.diff.p, fill=sign), stat='identity',
           alpha=0.7, width=6) +
  geom_hline(yintercept=0, linetype='dashed') +
  scale_fill_manual(values=c('#A24243', '#1B7883')) +
  geom_vline(xintercept=as.Date('2021-11-28'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  geom_vline(xintercept=as.Date('2022-01-31'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  scale_y_continuous(expand=c(0, 0), limits=c(ymin, ymax)) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  labs(x='Week commencing', y='Relative bias in weekly case incidence\nwith reference to ONS estimates') +
  coord_cartesian(clip='on', ylim=c(-1, 1), xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 0.5, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/weekly_case_incidence_per_1000_compare_residuals.pdf',
       device='pdf', width=8, height=4)

################################################################ 
## repeat analysis with two-week (preceding) smoothing
ONS_dat_p_weekly.2weeks_preceding.df <- as.data.frame(ONS_dat_p.df) %>%
  mutate(
    incidence_per_1000.cumsum=cumsum(incidence_per_1000),
    incidence_per_1000.lw.cumsum=cumsum(incidence_per_1000.lw),
    incidence_per_1000.up.cumsum=cumsum(incidence_per_1000.up),
    incidence_per_1000.2weeks.mean=(incidence_per_1000.cumsum - lag(incidence_per_1000.cumsum, 13))/14,
    incidence_per_1000.2weeks.lw.mean=(incidence_per_1000.lw.cumsum - lag(incidence_per_1000.lw.cumsum, 13))/14,
    incidence_per_1000.2weeks.up.mean=(incidence_per_1000.up.cumsum - lag(incidence_per_1000.up.cumsum, 13))/14
  ) %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    incidence_per_1000.2weeks.mean.weekly=sum(incidence_per_1000.2weeks.mean),
    incidence_per_1000.2weeks.lw.mean.weekly=sum(incidence_per_1000.2weeks.lw.mean),
    incidence_per_1000.2weeks.up.mean.weekly=sum(incidence_per_1000.2weeks.up.mean)
  )
uk_gov_dat_agg_weekly.2weeks_preceding.df <- uk_gov_dat_agg.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    total_cases.2_prev_weeks.p.weekly=sum(total_cases.2_prev_weeks.rm.p)
  )

## find multiplicative factor that minimises their differences
lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df <- ONS_dat_p_weekly.2weeks_preceding.df %>%
  left_join(uk_gov_dat_agg_weekly.2weeks_preceding.df, by='week') %>%
  mutate(
    y=incidence_per_1000.2weeks.mean.weekly,
    x=total_cases.2_prev_weeks.p.weekly
  )
lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding <- lm(y ~ x,
                                                        dat=lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df %>%
                                                          filter(week >= as.Date('2021-10-31') & week <= as.Date('2022-01-23'))
)
summary(lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding)

## compute residuals
ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df <- lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df %>%
  mutate(
    rs.diff = predict(lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding,
                      lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df) - lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df$y,
    rs.diff.p=rs.diff/y
  )

## compute residuals
ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df <- lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df %>%
  mutate(
    rs.total_cases.2weeks_preceding.weekly = predict(lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding,
                                                     lm_fit_ONS_uk_gov_case_incidence.2weeks_preceding.df),
    rs.diff = rs.total_cases.2weeks_preceding.weekly - y,
    rs.diff_lw = rs.total_cases.2weeks_preceding.weekly - incidence_per_lw.2weeks.mean_weekly/10,
    rs.diff_up = rs.total_cases.2weeks_preceding.weekly - incidence_per_up.2weeks.mean_weekly/10,
    rs.diff.p = rs.diff/rs.total_cases.2weeks_preceding.weekly,
    rs.diff.p_lw = rs.diff_lw/rs.total_cases.2weeks_preceding.weekly,
    rs.diff.p_up = rs.diff_up/rs.total_cases.2weeks_preceding.weekly
  )

## regression plot
ggplot() +
  geom_smooth(dat=ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df %>%
                filter(week >= as.Date('2021-11-21') & week <= as.Date('2022-02-01')),
              aes(x=x, y=y), method='lm', formula=y~x,
              color='#1B7883', fill='#484848', alpha=0.1, linewidth=1.3) +
  geom_point(dat=ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df %>%
               filter((week >= as.Date('2021-11-01') & week < as.Date('2021-11-28')) |
                        (week <= as.Date('2022-02-06') & week > as.Date('2022-01-23'))),
             aes(x=x, y=y), shape=3, size=2) +
  geom_point(dat=ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df %>%
               filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')),
             aes(x=x, y=y), size=2) +
  labs(x='GOV.UK Dashboard weekly case incidence per 1000 people', y='ONS weekly case incidence per 1000 people') +
  coord_cartesian(clip='on') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color='#484848', size=0.04),
    panel.grid.major.y = element_line(color='#484848', size=0.04),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y = element_text(vjust=2, size=16),
    axis.title.x = element_text(size=16, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/weekly_case_incidence_per_1000_compare_regression.2weeks_preceding.pdf',
       # device='pdf', width=8, height=7.5)
       device='pdf', width=7, height=7.7)

## plot of residuals
ymin <- -1
ymax <- 1
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-15')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_bar(dat=ONS_rescaled_uk_gov_case_incidence_compare.2weeks_preceding.df %>%
             mutate(sign=rs.diff.p >= 0),
           aes(x=week+3.5, y=rs.diff.p, fill=sign), stat='identity',
           alpha=0.7, width=6) +
  geom_hline(yintercept=0, linetype='dashed') +
  scale_fill_manual(values=c('#A24243', '#1B7883')) +
  geom_vline(xintercept=as.Date('2021-11-28'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  geom_vline(xintercept=as.Date('2022-01-31'), linetype='dashed', linewidth=0.8, color='#D43F3A', alpha=0.9) +
  scale_y_continuous(expand=c(0, 0), limits=c(ymin, ymax)) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  labs(x='Week commencing', y='Relative bias in weekly case incidence\nwith reference to ONS estimates') +
  coord_cartesian(clip='on', ylim=c(-1, 1), xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 0.5, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/weekly_case_incidence_per_1000_compare_residuals.2weeks_preceding.pdf',
       device='pdf', width=8, height=4)


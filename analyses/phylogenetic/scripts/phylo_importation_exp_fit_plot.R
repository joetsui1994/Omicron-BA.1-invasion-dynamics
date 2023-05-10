library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(stats)
library(scales)
library(HDInterval)

######################################################################
## read in and process posterior TLs
######################################################################

## BA.1.15
BA115_ptls <- read.csv('./posterior_tls/BA.1.15_posterior_tls.tsv', sep='\t')
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

## BA.1.17
BA117_ptls <- read.csv('./posterior_tls/BA.1.17_posterior_tls.tsv', sep='\t')
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

## BA.1.1
BA11_ptls <- read.csv('./posterior_tls/BA.1.1_posterior_tls.tsv', sep='\t')
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

## BA.1
BA1_ptls <- read.csv('./posterior_tls/BA.1_posterior_tls.tsv', sep='\t')
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
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
  )

## fit exponential curve to daily frequency of inferred importations (7-day rolling avg)
adjustedRs <- c()
earliest_fit_date <- as.Date('2021-11-13')
earliest_fit_date_i <- which(earliest_fit_date == all_ptls.all.hpd.daily$date)
for (date in all_ptls.all.hpd.daily$date[earliest_fit_date_i:nrow(all_ptls.all.hpd.daily)]) {
  lm_exp <- lm(log(total.count.7day_rm.median) ~ date,
               data=all_ptls.all.hpd.daily[
                 all_ptls.all.hpd.daily$date >= as.Date('2021-11-04') &
                   all_ptls.all.hpd.daily$date <= date,])
  adjustedRs <- c(adjustedRs, summary(lm_exp)$adj.r.squared)
}
bestfit_date <- all_ptls.all.hpd.daily$date[earliest_fit_date_i:nrow(all_ptls.all.hpd.daily)][match(max(adjustedRs), adjustedRs)]
bestfit_date

## plot exponential growth of importation
ymin <- 0.2
ymax <- 400
start_date <- as.Date('2021-10-31')
end_date <- as.Date('2022-01-31')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
stripes[1, 'xmin'] <- '2021-11-03'
travel_ban <- data.frame(
  xmin = c(as.Date('2021-11-26')),
  xmax = c(as.Date('2021-12-15')),
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(data=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_point(data=all_ptls.all.hpd.daily[
    all_ptls.all.hpd.daily$date >= as.Date('2021-11-10') &
      all_ptls.all.hpd.daily$date <= as.Date('2022-02-03'),],
             aes(x=date, y=total.count.7day_rm.median), alpha=0.85, size=1.2) +
  geom_smooth(data=all_ptls.all.hpd.daily[
    all_ptls.all.hpd.daily$date >= as.Date('2021-11-10') &
      all_ptls.all.hpd.daily$date <= as.Date(bestfit_date),],
    aes(x=date, y=total.count.7day_rm.median), formula = y ~ log(x),
    method="lm", se=TRUE, color='#539AA1', size=0.8, alpha=0.9) +
  geom_errorbar(data=all_ptls.all.hpd.daily[
    all_ptls.all.hpd.daily$date >= as.Date('2021-11-10') &
      all_ptls.all.hpd.daily$date <= as.Date('2022-12-31'),],
    aes(x=date, ymin=total.count.7day_rm.lw, ymax=total.count.7day_rm.up), width=.1, alpha=0.85) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-03'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_log10(limits=c(ymin, ymax),
                breaks=c(0.1, 1, 10, 100),
                # breaks=trans_breaks("log10", function(x) 10^x),
                labels=trans_format("log10", math_format(10^.x)),
                expand=c(0, 0)) +
  geom_vline(xintercept=as.Date('2021-12-25'), linetype='dashed', color='#bf0a0a', size=1) +
  labs(x='', y='Daily frequency of\ninferred importations') +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    # axis.title.y.right = element_text(vjust=2.5, size=12),
    axis.text.x = element_text(face=1, size=14.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14.5, color='black'),
    plot.margin = unit(c(0.5, 1, 0, 0.5), "cm")
  )

## output plot to file
ggsave('./daily_phylogenetic_importation_exp_fit.pdf',
       device='pdf', width=7.3, height=3.9)

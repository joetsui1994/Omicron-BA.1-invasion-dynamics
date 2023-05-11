library(dplyr)
library(readr)
library(lubridate)
library(HDInterval)
library(tidyr)
library(ggtext)

##################################################################
## read in and process UKHSA traveller history data
##################################################################

## function to convert epiweek to corresponding starting Sunday
epiweek_to_date <- function(year, week) {
  epiweek_start <- as.Date(paste0(year, "-W", sprintf("%02d", week), "-1"), format = "%Y-W%W-%u")
  calendar_date <- epiweek_start - 1
  return(calendar_date)
}

## read in UKHSA data
UKHSA_travellers_dat.df <- read.csv('./data/BA1_Travellers_by_Epiweek_20230510.csv', sep=',')
UKHSA_travellers_dat.df <- UKHSA_travellers_dat.df %>%
  mutate(week=epiweek_to_date(ifelse(epiweek < 10, 2022, 2021), epiweek))

## selected countries
selected_countries.df <- data.frame(
  country=c(
    'South Africa',
    'Spain',
    'United States',
    'Nigeria',
    'Netherlands',
    'Austria',
    'Ireland',
    'Others'
  ),
  abbr=c(
    'ZAF',
    'ESP',
    'USA',
    'NGA',
    'NLD',
    'AUT',
    'IRL',
    'OTHERS'
  ),
  colour=c(
    '#D43F3A',
    '#0C4C5F',
    '#E6824C',
    '#f0b154',
    '#1c8a99',
    '#4B4652',
    '#947A47',
    '#a6a6a6'
  )
)

## bin by selected countries + others
UKHSA_travellers_dat.country_binned.df <- UKHSA_travellers_dat.df %>%
  mutate(country_origin=ifelse(country_origin %in% selected_countries.df$abbr, country_origin, 'OTHERS')) %>%
  select(-epiweek) %>%
  group_by(week, country_origin) %>%
  summarise(n=sum(n)) %>%
  rename(abbr=country_origin) %>%
  left_join(selected_countries.df, by='abbr')

## rank by total imports
UKHSA_travellers_dat.country_binned.agg.df <- UKHSA_travellers_dat.country_binned.df %>%
  filter(!country %in% c('Others')) %>%
  group_by(country) %>%
  summarise(n.total=sum(n))
UKHSA_travellers_dat.country_binned.agg.df <- UKHSA_travellers_dat.country_binned.agg.df[
  order(UKHSA_travellers_dat.country_binned.agg.df$n.total, decreasing=TRUE),]
## add to UKHSA_travellers_dat.country_binned.df
UKHSA_travellers_dat.country_binned.df$country_ordered <- factor(
  UKHSA_travellers_dat.country_binned.df$country,
  levels=c(unique(UKHSA_travellers_dat.country_binned.agg.df$country), 'Others'))

## sanity checks
ggplot() +
  geom_histogram(dat=UKHSA_travellers_dat.country_binned.df,
                 aes(x=week, y=n, fill=country_ordered), stat='identity', position='stack')


######################################################################
## read in and process posterior TLs
######################################################################

## BA.1.15
BA115_ptls.df <- read.csv('./data/posterior_tls/BA.1.15_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA115_ptls.df <- BA115_ptls.df %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
  )
## all
BA115_ptls.all.daily.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA115_ptls.df$p_lab))),
  p_lab=rep(unique(BA115_ptls.df$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA115_ptls.df %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA115_ptls.all.daily.df[is.na(BA115_ptls.all.daily.df$count),]$count <- 0

## BA.1.17
BA117_ptls.df <- read.csv('./data/posterior_tls/BA.1.17_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA117_ptls.df <- BA117_ptls.df %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
  )
## all
BA117_ptls.all.daily.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA117_ptls.df$p_lab))),
  p_lab=rep(unique(BA117_ptls.df$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA117_ptls.df %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA117_ptls.all.daily.df[is.na(BA117_ptls.all.daily.df$count),]$count <- 0

## BA.1.1
BA11_ptls.df <- read.csv('./data/posterior_tls/BA.1.1_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA11_ptls.df <- BA11_ptls.df %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
  )
## all
BA11_ptls.all.daily.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA11_ptls.df$p_lab))),
  p_lab=rep(unique(BA11_ptls.df$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA11_ptls.df %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA11_ptls.all.daily.df[is.na(BA11_ptls.all.daily.df$count),]$count <- 0

## BA.1
BA1_ptls.df <- read.csv('./data/posterior_tls/BA.1_posterior_tls.tsv', sep='\t')
## take only importation events that led to further local transmission
BA1_ptls.df <- BA1_ptls.df %>%
  filter(source == 'nonENG') %>%
  mutate(
    tmrca_date = as.Date(date_decimal(tmrca)),
    ptmrca_date = as.Date(date_decimal(ptmrca)),
    first_seen_date = as.Date(date_decimal(first_seen)),
    last_seen_date = as.Date(date_decimal(last_seen)),
    import_est = (tmrca + ptmrca)/2,
    import_est_date = as.Date(date_decimal(import_est)),
  )
## all
BA1_ptls.all.daily.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1),
           length(unique(BA1_ptls.df$p_lab))),
  p_lab=rep(unique(BA1_ptls.df$p_lab),
            each=length(seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)))) %>%
  left_join(
    BA1_ptls.df %>%
      group_by(import_est_date, p_lab) %>%
      summarise(count = n()) %>%
      rename(date=import_est_date) %>%
      arrange(p_lab, date), by=c('date', 'p_lab')
  )
BA1_ptls.all.daily.df[is.na(BA1_ptls.all.daily.df$count),]$count <- 0

## merge all all.daily
all_ptls.all.daily.df <- BA115_ptls.all.daily.df %>%
  left_join(BA117_ptls.all.daily.df, by=c('date', 'p_lab')) %>%
  rename(
    BA.1.15=count.x,
    BA.1.17=count.y
  ) %>%
  left_join(BA11_ptls.all.daily.df, by=c('date', 'p_lab')) %>%
  rename(BA.1.1=count) %>%
  left_join(BA1_ptls.all.daily.df, by=c('date', 'p_lab')) %>%
  rename(BA.1=count)
## aggregate across subtypes and calculate rolling average for each p_lab
all_ptls.all.daily.df <- all_ptls.all.daily.df %>%
  group_by(p_lab) %>%
  mutate(
    total.count=BA.1.15+BA.1.17+BA.1.1+BA.1,
    total.count.7day_rm=rollmean(total.count, 7, fill=NA)
  )
## find distribution across p_lab
all_ptls.all.hpd.daily.df <- all_ptls.all.daily.df %>%
  group_by(date) %>%
  summarise(
    total.count.7day_rm.median=median(total.count.7day_rm),
    total.count.7day_rm.lw = as.numeric(hdi(total.count.7day_rm, 0.95)['lower']),
    total.count.7day_rm.up = as.numeric(hdi(total.count.7day_rm, 0.95)['upper'])
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

## bin importation events by import_est_date
all_mcc_tls.daily.df <- data.frame(
  date=seq(as.Date('2021-10-01'), as.Date('2022-01-31'), by=1)
  ) %>%
  left_join(
    all_mcc_tls.df %>%
      group_by(import_est_date) %>%
      summarise(count=n()) %>%
      rename(date=import_est_date)
  )
all_mcc_tls.daily.df[is.na(all_mcc_tls.daily.df$count),]$count <- 0

########################################################################
## read in pre-calculated EIIs
########################################################################

weekly_country_level_EIIs.df <- read.csv('./data/weekly_country-level_EIIs.csv', sep=',')
weekly_country_level_EIIs.df$week <- as.Date(weekly_country_level_EIIs.df$week)

########################################################################
## make plots
########################################################################

## main plot
ymin <- 0
ymax <- 45000
start_date <- as.Date('2021-10-10')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
travel_ban <- data.frame(
  xmin = c(as.Date('2021-11-26')),
  xmax = c(as.Date('2021-12-15')),
  ymin = 0,
  ymax = ymax
)
secondary_axis_factor <- 125
secondary_axis_factor.2 <- 5.8
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(dat=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_bar(data=UKHSA_travellers_dat.country_binned.df,
           aes(x=week, y=n*secondary_axis_factor.2/1000, fill=factor(country_ordered)), stat='identity', position='stack',
           color='black', alpha=0.8, linewidth=0.3) +
  scale_fill_manual(values=c(selected_countries.df[
    match(unique(UKHSA_travellers_dat.country_binned.agg.df$country), selected_countries.df$country),]$colour, '#a6a6a6')) +
  geom_bar(dat=all_mcc_tls.daily.df,
           aes(x=date, y=count*secondary_axis_factor/1000), stat='identity',
           color='black', alpha=0.6, linewidth=0.2, width=1, fill='#3E4855') +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              group_by(week) %>%
              summarise(EII.weekly.positivity.w2_preceding=sum(EII.weekly.positivity.w2_preceding, na.rm=T)),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000),
            linewidth=1.5, color='#484848') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b', name='',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(
    name = "Weekly Omicron BA.1 EII (10<sup>3</sup>) (black curve)",
    limits=c(ymin, ymax),
    sec.axis = sec_axis(trans=~./(secondary_axis_factor/1000),
                        name="Daily number of phylogenetic importation\nevents (dark grey bars)",
                        breaks=seq(0, 500, by=100)),
    expand=c(0, 0)
  ) +
  coord_cartesian(clip='on',
                  xlim=c(as.Date('2021-10-18'), as.Date('2022-02-06')),
                  ylim=c(0, 55)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(size=12, vjust=2),
    axis.title.y.right = element_text(size=12, vjust=2),
    axis.title.x = element_text(size=12, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=10, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=10, color='black'),
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/fig-2_main_v3.pdf',
       device='pdf', width=8, height=4.5)

## make plot (main plot: EII + VAM)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(dat=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_bar(data=UKHSA_travellers_dat.country_binned.df,
           aes(x=week, y=n*secondary_axis_factor.2/1000, fill=factor(country_ordered)), stat='identity', position='stack',
           color='black', alpha=0.55, linewidth=0.3) +
  scale_fill_manual(values=c(selected_countries.df[
    match(unique(UKHSA_travellers_dat.country_binned.agg.df$country), selected_countries.df$country),]$colour, '#a6a6a6')) +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              group_by(week) %>%
              summarise(EII.weekly.positivity.w2_preceding=sum(EII.weekly.positivity.w2_preceding, na.rm=T)),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000),
            linewidth=1.2, color='#484848') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b', name='',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(
    name = "Omicron BA.1 EII (10<sup>3</sup>) (solid curves)",
    limits=c(ymin, ymax),
    sec.axis = sec_axis(trans=~./(secondary_axis_factor.2),
                        name="Weekly number of Omicron BA.1 positive tests among<br >inbound travellers (10<sup>3</sup>) (coloured bars)",
                        breaks=seq(0, 10, by=2)),
    expand=c(0, 0)
  ) +
  coord_cartesian(clip='on',
                  xlim=c(as.Date('2021-10-18'), as.Date('2022-02-06')),
                  ylim=c(0, 55)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.line.y.right = element_line(color='#ba7a50'),
    axis.ticks.y.right = element_line(color='#ba7a50'),
    axis.text.y.right = element_text(color='#ba7a50'),
    axis.title.y.left = element_markdown(size=12, vjust=2),
    axis.title.y.right = element_markdown(size=12, vjust=2, color='#ba7a50'),
    axis.title.x = element_text(size=12, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=10, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=10, color='black'),
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/fig-2_main_axis_v1.pdf',
       device='pdf', width=8, height=4.5)

## make plot (inset)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_bar(dat=all_mcc_tls.daily.df,
           aes(x=date, y=count*secondary_axis_factor/1000), stat='identity',
           color='black', alpha=0.8, linewidth=0.4, width=1, fill='#3E4855') +
  geom_bar(data=UKHSA_travellers_dat.country_binned.df,
           aes(x=week, y=n*secondary_axis_factor.2/1000, fill=factor(country_ordered)), stat='identity', position='stack',
           color='black', alpha=0.8, linewidth=0.5) +
  scale_fill_manual(values=c('#D43F3A', selected_countries.df[
    match(unique(UKHSA_travellers_dat.country_binned.agg.df$country), selected_countries.df$country),]$colour)) +
  scale_fill_manual(values=c(selected_countries.df[
    match(unique(UKHSA_travellers_dat.country_binned.agg.df$country), selected_countries.df$country),]$colour, '#a6a6a6')) +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              group_by(week) %>%
              summarise(EII.weekly.positivity.w2_preceding=sum(EII.weekly.positivity.w2_preceding, na.rm=T)),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000),
            linewidth=2.5, color='#3b3b3b') +
  scale_x_date(date_breaks='7 days', date_labels='%d %b', name='',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(
    name = "Omicron BA.1 EII (10<sup>3</sup>) (solid curves)",
    limits=c(ymin, ymax),
    sec.axis = sec_axis(trans=~./(secondary_axis_factor/1000),
                        name="Daily number of phylogenetic importation\nevents (grey bars)",
                        breaks=seq(0, 400, by=2)),
    expand=c(0, 0)
  ) +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-10'), as.Date('2021-12-03')),
                  ylim=c(0, 1.1)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(size=13, vjust=2),
    axis.title.y.right = element_text(size=13, vjust=2),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=18, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=18, color='black'),
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/fig-2_inset_v2.pdf',
       device='pdf', width=7.5, height=4.7)

## make plot (inset axis)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_bar(data=UKHSA_travellers_dat.country_binned.df,
           aes(x=week, y=n*secondary_axis_factor.2/1000, fill=factor(country_ordered)), stat='identity', position='stack',
           color='black', alpha=0.7, linewidth=0.3) +
  scale_fill_manual(values=c('#D43F3A', selected_countries.df[
    match(unique(UKHSA_travellers_dat.country_binned.agg.df$country), selected_countries.df$country),]$colour)) +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Nigeria')) %>%
              group_by(week) %>%
              summarise(EII.weekly.positivity.w2_preceding=sum(EII.weekly.positivity.w2_preceding, na.rm=T)),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000),
            linewidth=1.8, color='#1B7883') +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              filter(country %in% c('South Africa', 'Nigeria')) %>%
              group_by(week) %>%
              summarise(EII.weekly.positivity.w2_preceding=sum(EII.weekly.positivity.w2_preceding, na.rm=T)),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000),
            linewidth=1.8, color='#c24243') +
  scale_x_date(date_breaks='7 days', date_labels='%d %b', name='',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(
    name = "Omicron BA.1 EII (10<sup>3</sup>) (solid curves)",
    limits=c(ymin, ymax),
    sec.axis = sec_axis(trans=~./(secondary_axis_factor.2),
                        name="",
                        breaks=seq(0, 3000, by=0.05)),
    expand=c(0, 0)
  ) +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-10'), as.Date('2021-12-03')),
                  ylim=c(0, 1.1)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.line.y.right = element_line(color='#ba7a50'),
    axis.ticks.y.right = element_line(color='#ba7a50'),
    axis.text.y.right = element_text(color='#ba7a50'),
    axis.title.y.left = element_markdown(size=13, vjust=2),
    axis.title.y.right = element_text(size=13, vjust=2, color='#ba7a50'),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=18, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=18, color='black'),
    plot.margin = unit(c(0.5, 0.3, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/fig-2_inset_axis.pdf',
       device='pdf', width=7.5, height=4.7)

## country-level weekly EII from positivity rates
## specify colour scheme
country_colours.df <- data.frame(
  country=c("others", "South Africa", "Spain", "United States", "Italy", "Ireland", "Scotland",
            "Portugal", "Germany", "Poland", "Northern Ireland", "France", "Switzerland", "United Arab Emirates",
            "India", "Turkey", "Netherlands", "Romania", "Denmark", "Greece", "Sweden", "Canada",
            "Pakistan", "Norway", "Cyprus", "Egypt", "Austria", "Nigeria"),
  colour=c("#484848", "#D43F3A", "#0C4C5F", "#E6824C", "#EAB059", "#947A47", "#893E45", "#335059",
           "#D6854C", "#60444F", "#F1B95A", "#313030", "#b38936", "#963B40", "#CD7348", "#CFA654",
           "#1c8a99", "#6E424C", "#C29D51", "#7C4149", "#BA513F", "#384E5B", "#1B7883", "#9E8149",
           "#524551", "#B6944E", "#4B4652", "#f0b154")
)

ymin <- 0
ymax <- 36
start_date <- as.Date('2021-10-10')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
travel_ban <- data.frame(
  xmin = c(as.Date('2021-11-26')),
  xmax = c(as.Date('2021-12-15')),
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(dat=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000, group=country),
            linetype='dashed', linewidth=0.5, alpha=0.6, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=5),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  coord_cartesian(clip='on',
                  xlim=c(as.Date('2021-10-18'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=12),
    axis.title.x = element_text(size=12, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=10, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=10, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.positivity.w2_preceding_smooth_long.pdf',
       device='pdf', width=7.645, height=4.56)

## country-level weekly EII from positivity rates (inset)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_rect(dat=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000, group=country),
            linetype='dashed', linewidth=0.5, alpha=0.6, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.85, 2.1)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, 3, by=1),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  coord_cartesian(clip='on', ylim=c(0, 3.2), xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=17, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=17, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.positivity.w2_preceding_smooth_v3_inset.pdf',
       device='pdf', width=7.5, height=4.7)


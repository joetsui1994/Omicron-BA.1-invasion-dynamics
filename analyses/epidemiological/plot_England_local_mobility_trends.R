library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)

## read in (England) local mobility data (with great circle distance)
mob_dat.df <- read.csv('./data/LTLA_England_112019_052022_crsTransformed_14Sep2022.csv', sep=',')
mob_dat.df$week_of <- as.Date(mob_dat.df$week_of)

## bin movements by distance
mob_dat.binned.df <- mob_dat.df %>%
  mutate(
    dist.bin=ifelse(distance < 30, 'local',
                    ifelse(distance >= 30 & distance < 60, 'short',
                           ifelse(distance >= 60 & distance < 100, 'mid', 'long')))
  )

## aggregate data at weekly-level
mob_dat.binned.agg.df <- data.frame(
  week=rep(seq(as.Date('2019-11-03'), as.Date('2022-03-287'), by=7), 4),
  dist.bin=rep(c('local', 'short', 'mid', 'long'), each=length(seq(as.Date('2019-11-03'), as.Date('2022-03-287'), by=7)))
  ) %>%
  left_join(mob_dat.binned.df %>%
              group_by(week_of, dist.bin) %>%
              summarise(movement=sum(movement)) %>%
              rename(week=week_of), by=c('week', 'dist.bin'))

## specify pre-pandemic reference period
mob_dat.binned.agg.ref.df <- mob_dat.binned.agg.df %>%
  filter(week >= as.Date('2019-11-03') & week <= as.Date('2019-12-22')) %>%
  group_by(dist.bin) %>%
  summarise(movement.ref=mean(movement))

## calculate movement relative to reference period
mob_dat.binned.agg.rel.df <- mob_dat.binned.agg.df %>%
  left_join(mob_dat.binned.agg.ref.df, by='dist.bin') %>%
  mutate(movement.rel=movement/movement.ref)

## make plots
ymin <- -0.1
ymax <- 1
npi_stripes <- data.frame(
  xmin=as.Date(c('2020-11-05', '2020-12-21', '2021-12-10')),
  xmax=as.Date(c('2020-12-02', '2021-03-08', '2022-01-27')),
  ymin=ymin,
  ymax=ymax
)
ggplot() +
  geom_rect(data=npi_stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#6F444B', alpha=0.15) +
  geom_line(dat=mob_dat.binned.agg.rel.df %>%
              filter(week >= as.Date('2020-09-01')),
            aes(x=week, y=movement.rel, group=dist.bin, color=dist.bin),
            linewidth=1.2, alpha=0.85) +
  geom_hline(yintercept=0, linewidth=0.2, alpha=0.5, color='#484848') +
  labs(x='Week commencing', y='Weekly relative mobility flow') +
  scale_color_manual(values=c('#D43F3A', '#1B7883', '#484848', '#EAB059')) +
  scale_x_date(date_breaks='2 months', date_labels='%b-%y',
               limits=c(as.Date('2020-09-01'), as.Date('2022-02-10')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax), expand=c(0, 0)) +
  coord_cartesian(clip='on', xlim=c(as.Date('2020-09-20'), as.Date('2022-02-05'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color='#484848', size=0.04),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./multi-scale_England_mobility_trends.pdf',
       device='pdf', width=7.645, height=4.5)

## look into local (within-LTLA) mobility only
local_mob_dat.df <- mob_dat.df %>%
  filter(origin == destination) %>%
  select(-destination, -distance) %>%
  rename(week=week_of)

## specify pre-Omicron reference period
local_mob_dat.ref.df <- local_mob_dat.df %>%
  filter(week >= as.Date('2021-09-12') & week <= as.Date('2021-10-31')) %>%
  group_by(origin) %>%
  summarise(movement.ref=mean(movement))

## calculate local movement relative to reference period
local_mob_dat.rel.df <- local_mob_dat.df %>%
  left_join(local_mob_dat.ref.df, by='origin') %>%
  mutate(movement.rel=movement/movement.ref)

## calculate national average
local_mob_dat.rel.national.df <- local_mob_dat.rel.df %>%
  group_by(week) %>%
  summarise(movement.rel=mean(movement.rel))

## make plot
ymin <- 0.45
ymax <- 1.25
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = ymin,
  ymax = ymax
)
npi_stripes <- data.frame(
  xmin=as.Date(c('2020-11-05', '2020-12-21', '2021-12-10')),
  xmax=as.Date(c('2020-12-02', '2021-03-08', '2022-01-27')),
  ymin=ymin,
  ymax=ymax
)
ggplot() +
  geom_rect(data=npi_stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#6F444B', alpha=0.15) +
  geom_line(dat=local_mob_dat.rel.df,
            aes(x=week, y=movement.rel, group=origin),
            linewidth=0.2, alpha=0.1, color='#484848') +
  geom_line(dat=local_mob_dat.rel.national.df,
            aes(x=week, y=movement.rel),
            linewidth=1.5, alpha=0.95, color='black') +
  geom_vline(xintercept=as.Date('2021-12-25'), linetype='dashed', linewidth=0.8, color='#bf0404') +
  labs(x='Week commencing', y='Weekly changes in within-LTLA mobility\nrelative to pre-Omicron period') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     # breaks=seq(0, , by=0.1),
                     expand=c(0, 0)) +
  coord_cartesian(clip='off', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color='#484848', size=0.04),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./within-LTLA_mobility.pdf',
       device='pdf', width=7.75, height=4.5)




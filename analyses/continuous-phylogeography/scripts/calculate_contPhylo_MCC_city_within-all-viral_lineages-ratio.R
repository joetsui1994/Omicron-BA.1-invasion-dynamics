library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(zoo)

## read in combined mcc output from continous phylogeographic reconstruction of all local transmission lineages
mcc_geo_combined.df <- read_delim('./mcc_geo_combined_hubs_mapped.csv', delim=',')
mcc_geo_combined.df$mid_date <- as.Date(mcc_geo_combined.df$mid_date)
mcc_geo_combined.df <- mcc_geo_combined.df %>%
  mutate(mid_week=floor_date(mid_date, unit='week'))

## Birmingham
## inflow
birmingham_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_birmingham & tail_birmingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
birmingham_within.df <- mcc_geo_combined.df %>%
  filter(head_birmingham & tail_birmingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
birmingham_outflow.df <- mcc_geo_combined.df %>%
  filter(head_birmingham & !tail_birmingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
birmingham_combined.df <- merge(birmingham_inflow.df, birmingham_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
birmingham_combined.df <- merge(birmingham_combined.df, birmingham_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
birmingham_combined.df[is.na(birmingham_combined.df)] = 0
birmingham_combined.df <- birmingham_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## Leeds
## inflow
leeds_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_leeds & tail_leeds) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
leeds_within.df <- mcc_geo_combined.df %>%
  filter(head_leeds & tail_leeds) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
leeds_outflow.df <- mcc_geo_combined.df %>%
  filter(head_leeds & !tail_leeds) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
leeds_combined.df <- merge(leeds_inflow.df, leeds_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
leeds_combined.df <- merge(leeds_combined.df, leeds_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
leeds_combined.df[is.na(leeds_combined.df)] = 0
leeds_combined.df <- leeds_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## Newcastle
## inflow
newcastle_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_newcastle & tail_newcastle) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
newcastle_within.df <- mcc_geo_combined.df %>%
  filter(head_newcastle & tail_newcastle) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
newcastle_outflow.df <- mcc_geo_combined.df %>%
  filter(head_newcastle & !tail_newcastle) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
newcastle_combined.df <- merge(newcastle_inflow.df, newcastle_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
newcastle_combined.df <- merge(newcastle_combined.df, newcastle_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
newcastle_combined.df[is.na(newcastle_combined.df)] = 0
newcastle_combined.df <- newcastle_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## Nottingham
## inflow
nottingham_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_nottingham & tail_nottingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
nottingham_within.df <- mcc_geo_combined.df %>%
  filter(head_nottingham & tail_nottingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
nottingham_outflow.df <- mcc_geo_combined.df %>%
  filter(head_nottingham & !tail_nottingham) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
nottingham_combined.df <- merge(nottingham_inflow.df, nottingham_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
nottingham_combined.df <- merge(nottingham_combined.df, nottingham_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
nottingham_combined.df[is.na(nottingham_combined.df)] = 0
nottingham_combined.df <- nottingham_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## Manchester
## inflow
manchester_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_manchester & tail_manchester) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
manchester_within.df <- mcc_geo_combined.df %>%
  filter(head_manchester & tail_manchester) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
manchester_outflow.df <- mcc_geo_combined.df %>%
  filter(head_manchester & !tail_manchester) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
manchester_combined.df <- merge(manchester_inflow.df, manchester_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
manchester_combined.df <- merge(manchester_combined.df, manchester_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
manchester_combined.df[is.na(manchester_combined.df)] = 0
manchester_combined.df <- manchester_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## Greater London
greaterLondon_inflow.df <- mcc_geo_combined.df %>%
  filter(!head_london & tail_london) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## within
greaterLondon_within.df <- mcc_geo_combined.df %>%
  filter(head_london & tail_london) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## outflow
greaterLondon_outflow.df <- mcc_geo_combined.df %>%
  filter(head_london & !tail_london) %>%
  group_by(mid_week) %>%
  summarise(count=n())
## combine
greaterLondon_combined.df <- merge(greaterLondon_inflow.df, greaterLondon_within.df, by='mid_week', all=TRUE) %>%
  rename(
    inflow='count.x',
    within='count.y'
  )
greaterLondon_combined.df <- merge(greaterLondon_combined.df, greaterLondon_outflow.df, by='mid_week', all=TRUE) %>%
  rename(
    outflow='count'
  )
greaterLondon_combined.df[is.na(greaterLondon_combined.df)] = 0
greaterLondon_combined.df <- greaterLondon_combined.df %>%
  group_by(mid_week) %>%
  mutate(
    r_within_inflow = within/inflow,
    r_within_all = within/(within + inflow + outflow),
    r_inflow_all = inflow/(within + inflow + outflow)
  )

## make plot
## specify colour scheme
colors <- c(
  '#313030',
  '#1B7883',
  '#855f66',
  '#D43F3A',
  '#E6824C',
  '#F8C55D'
)
stripes <- data.frame(
  xmin = c(as.Date('2021-11-14'), as.Date('2021-11-28'), as.Date('2021-12-12'), as.Date('2021-12-26'),
           as.Date('2022-01-09'), as.Date('2022-01-23')),
  xmax = c(as.Date('2021-11-21'), as.Date('2021-12-05'), as.Date('2021-12-19'), as.Date('2022-01-02'),
           as.Date('2022-01-16'), as.Date('2022-01-30')),
  ymin = 0,
  ymax = 1
)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  geom_line(data=birmingham_combined.df[birmingham_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[1], linewidth=1.2, alpha=0.85) +
  geom_line(data=leeds_combined.df[leeds_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[2], linewidth=1.2, alpha=0.85) +
  geom_line(data=newcastle_combined.df[newcastle_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[3], linewidth=1.2, alpha=0.85) +
  geom_line(data=nottingham_combined.df[nottingham_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[4], linewidth=1.2, alpha=0.85) +
  geom_line(data=manchester_combined.df[manchester_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[5], linewidth=1.2, alpha=0.85) +
  geom_line(data=greaterLondon_combined.df[greaterLondon_combined.df$mid_week < as.Date('2022-01-30'),],
            aes(x=mid_week, r_within_all), color=colors[6], linewidth=1.2, alpha=0.85) +
  geom_vline(xintercept=as.Date('2021-12-25'), linetype='dashed', color='#bf0a0a', linewidth=1) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-14'), as.Date('2022-01-23')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1),
                     breaks=c(0, 0.25, 0.5, 0.75, 1),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Ratio between within-location and\nall viral movements (weekly)') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-14'), as.Date('2022-01-23'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )
  
## output plot to file
ggsave('./figures/cont_phylogeog_cities_within_all_ratio.pdf',
       device='pdf', width=8, height=4.5)


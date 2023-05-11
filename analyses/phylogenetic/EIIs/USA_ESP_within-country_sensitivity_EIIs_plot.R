library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(stats)
library(dplyr)

########################################################################
## read in pre-calculated EIIs at country-level
########################################################################

weekly_country_level_EIIs.df <- read.csv('./data/weekly_country-level_EIIs.csv', sep=',')
weekly_country_level_EIIs.df$week <- as.Date(weekly_country_level_EIIs.df$week)

########################################################################
## read in pre-calculated EIIs with within-country disaggregation
## for Spain and the US
########################################################################

## ESP
ESP_AC_weekly_EIIs_case_per_capita.df <- read.csv('./data/ESP_AC_weekly_EIIs_case_per_capita.csv', sep=',')
ESP_AC_weekly_EIIs_case_per_capita.df$week <- as.Date(ESP_AC_weekly_EIIs_case_per_capita.df$week)
ESP_AC_weekly_EIIs_positivity.df <- read.csv('./data/ESP_AC_weekly_EIIs_positivity.csv', sep=',')
ESP_AC_weekly_EIIs_positivity.df$week <- as.Date(ESP_AC_weekly_EIIs_positivity.df$week)

## USA
USA_state_weekly_EIIs_case_per_capita.df <- read.csv('./data/USA_state_weekly_EIIs_case_per_capita.csv', sep=',')
USA_state_weekly_EIIs_case_per_capita.df$week <- as.Date(USA_state_weekly_EIIs_case_per_capita.df$week)
USA_state_weekly_EIIs_positivity.df <- read.csv('./data/USA_state_weekly_EIIs_positivity.csv', sep=',')
USA_state_weekly_EIIs_positivity.df$week <- as.Date(USA_state_weekly_EIIs_positivity.df$week)

########################################################################
## merge and compare
########################################################################

weekly_country_level_EIIs.ESP_USA_disagg.df <- weekly_country_level_EIIs.df %>%
  filter(!country %in% c('Spain', 'United States')) %>%
  rename(
    EII.case=EII.weekly.cases_per_capita.w2_preceding,
    EII.positivity=EII.weekly.positivity.w2_preceding
  ) %>%
  rbind(
    ESP_AC_weekly_EIIs_case_per_capita.df %>%
      rename(EII.case=EII.weekly) %>%
      left_join(ESP_AC_weekly_EIIs_positivity.df %>%
                  rename(EII.positivity=EII.weekly), by='week') %>%
      mutate(country='Spain') %>%
      select(country, week, EII.case, EII.positivity)) %>%
  rbind(
    USA_state_weekly_EIIs_case_per_capita.df %>%
      rename(EII.case=EII.weekly) %>%
      left_join(USA_state_weekly_EIIs_positivity.df %>%
                  rename(EII.positivity=EII.weekly), by='week') %>%
      mutate(country='United States') %>%
      select(country, week, EII.case, EII.positivity))
weekly_country_level_EIIs.ESP_USA_disagg.df$country_ordered <- factor(weekly_country_level_EIIs.ESP_USA_disagg.df$country,
                                                                      levels=unique(weekly_country_level_EIIs.ESP_USA_disagg.df$country))

########################################################################
## make plots
########################################################################

## specify country colours
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

## absolute EIIs (case per capita)
ymin <- 0
ymax <- 1.05
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
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
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.case/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.case/1000,
                group=country_ordered, color=country_ordered, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
             linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.ESP_USA_disagg.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=0.2),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.ESP_USA-disagg.case.v2.pdf',
       device='pdf', width=7.645, height=4.5)

## absolute EIIs (case per capita) (inset)
ymin <- 0
ymax <- 1.05
start_date <- as.Date('2021-10-24')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.case/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.case/1000,
                group=country_ordered, color=country_ordered, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.ESP_USA_disagg.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=0.04),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25')), ylim=c(0, 0.13)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=14, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.ESP_USA-disagg.case.inset.pdf',
       device='pdf', width=7.4, height=4.5)

## absolute EIIs (positivity rates)
ymin <- 0
ymax <- 28
start_date <- as.Date('2021-10-05')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
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
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.positivity/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.positivity/1000,
                group=country_ordered, color=country_ordered, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
             linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.ESP_USA_disagg.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=5),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=12, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=12, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.ESP_USA-disagg.positivity.pdf',
       device='pdf', width=7.645, height=4.5)

## absolute EIIs (positivity rates) (inset)
ymin <- 0
ymax <- 28
start_date <- as.Date('2021-10-05')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = ymax
)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.positivity/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=weekly_country_level_EIIs.ESP_USA_disagg.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.positivity/1000,
                group=country_ordered, color=country_ordered, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(weekly_country_level_EIIs.ESP_USA_disagg.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=1),
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
    axis.text.x = element_text(face=1, size=14, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.ESP_USA-disagg.positivity.inset.pdf',
       device='pdf', width=7.14, height=4.5)



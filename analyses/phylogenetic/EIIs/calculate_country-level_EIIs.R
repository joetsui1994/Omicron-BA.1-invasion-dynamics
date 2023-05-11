library(tidyr)
library(stats)
library(ggplot2)
library(ggtext)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)

##################################################################
## read in and process air passenger data (world, at airport lvl)
##################################################################

world_flight_dat.df <- read.csv('../WorldToUK_Oct2021_Feb2022.csv', sep=',')
unique(world_flight_dat.df$destinationLocationName)

## assign UK airport to either England, Wales, Northern Ireland or Scotland
Eng_airports <- c(
  "London Heathrow Airport",
  "London Gatwick Airport",
  "London Stansted Airport",
  "Birmingham Airport",
  "Liverpool Airport",
  "Bristol Airport",
  "London City Airport",
  "East Midlands Airport",
  "Leeds Bradford International Airport",
  "Newcastle International Airport",
  "Bournemouth Airport",
  "Southampton / Weather Centre",
  "Robin Hood Airport Doncaster Sheffield",
  "Durham Tees Valley Airport",
  "Newquay Cornwall Airport",
  "Norwich International Airport",
  "Exeter Airport",
  "St Mary's Airport",
  "Land's End Airport",
  "Humberside Airport",
  "London Southend Airport"
)
Wales_airports <- c(
  "Cardiff Airport",
  "Prestwick Airport"
)
NorthIreland_airports <- c(
  "Belfast International Airport",
  "George Best Belfast City Airport",
  "City of Derry Airport"
)
Scotland_airports <- c(
  "Edinburgh Airport",
  "Glasgow Airport",
  "Aberdeen International Airport",
  "Inverness Dalcross Airport",
  "Sumburgh Airport",
  "Stornoway Airport",
  "Kirkwall Airport",
  "Dundee Riverside Airport",
  "Benbecula Airport",
  "Islay Airport",
  "Barra Airport",
  "Tiree Airport",
  "Campbeltown Airport",
  "Fair Isle Airport",
  "Lerwick Tingwall Airport",
  "Foula Airport",
  "Coll Airport",
  "Oban Airport",
  "Colonsay Airstrip"
)

## assign UK airports to either England, Wales, Scotland or Northern Ireland
world_flight_dat.UK_split.df <- world_flight_dat.df %>%
  mutate(destCountry=ifelse(destinationLocationName %in% Eng_airports, 'England',
                            ifelse(destinationLocationName %in% Wales_airports, 'Wales',
                                   ifelse(destinationLocationName %in% NorthIreland_airports, 'Northern Ireland',
                                          ifelse(destinationLocationName %in% Scotland_airports, 'Scotland',
                                                 destinationCountryName)))),
         originCountry=ifelse(originLocationName %in% Eng_airports, 'England',
                              ifelse(originLocationName %in% Wales_airports, 'Wales',
                                     ifelse(originLocationName %in% NorthIreland_airports, 'Northern Ireland',
                                            ifelse(originLocationName %in% Scotland_airports, 'Scotland',
                                                   originCountryName)))))

## look at only air passengers arriving in English airports
world_flight_dat.UK_split.Eng.df <- world_flight_dat.UK_split.df %>%
  filter(destCountry == 'England' & originCountry != 'England')

## aggregate over origin airports by originCountry (i.e. ignoring USA and ESP disaggregation)
world_flight_dat.UK_split.Eng.originCountry.df <- world_flight_dat.UK_split.Eng.df %>%
  group_by(aggregationMonth, originCountry) %>%
  summarise(totalPassengerVolume=sum(totalPassengerVolume))
world_flight_dat.UK_split.Eng.originCountry.df$year <- sapply(world_flight_dat.UK_split.Eng.originCountry.df$aggregationMonth,
                                                              function(x) as.numeric(strsplit(x, '-')[[1]][1]))
world_flight_dat.UK_split.Eng.originCountry.df$month <- sapply(world_flight_dat.UK_split.Eng.originCountry.df$aggregationMonth,
                                                               function(x) as.numeric(strsplit(x, '-')[[1]][2]))
world_flight_dat.UK_split.Eng.originCountry.df <- world_flight_dat.UK_split.Eng.originCountry.df %>%
  ungroup(aggregationMonth) %>%
  select(-aggregationMonth) %>%
  mutate(month=ifelse(month == 10, as.Date('2021-10-01'),
                      ifelse(month == 11, as.Date('2021-11-01'),
                             ifelse(month == 12, as.Date('2021-12-01'),
                                    ifelse(month == 1, as.Date('2022-01-01'),
                                           as.Date('2022-02-01')))))) %>%
  select(-year)
world_flight_dat.UK_split.Eng.originCountry.df$month <- as.Date(world_flight_dat.UK_split.Eng.originCountry.df$month)

## select countries with most air traffic flow, accounting for 80% of total
world_flight_dat.UK_split.Eng.originCountry.total_NovDecJan.df <- world_flight_dat.UK_split.Eng.originCountry.df %>%
  filter(month >= as.Date('2021-11-01') & month < as.Date('2022-02-01')) %>%
  group_by(originCountry) %>%
  summarise(totalPassengerVolume=sum(totalPassengerVolume)) %>%
  mutate(totalPassengerVolume.p=totalPassengerVolume/sum(totalPassengerVolume)) %>%
  arrange(-totalPassengerVolume.p) %>%
  mutate(
    totalPassengerVolume.cp=cumsum(totalPassengerVolume.p),
    rank=row_number()
    )
total_80p_countries <- (world_flight_dat.UK_split.Eng.originCountry.total_NovDecJan.df %>%
  filter(totalPassengerVolume.cp < 0.8))$originCountry
## add South Africa
total_80p_countries.ZAF_added <- c('South Africa', total_80p_countries)

## extract data for only these countries
top_80p_countries_flight_dat.df <- world_flight_dat.UK_split.Eng.originCountry.df %>%
  filter(originCountry %in% total_80p_countries.ZAF_added) %>%
  rename(country=originCountry)

## disaggregate data into weekly level
top_80p_countries_flight_dat.weekly.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-03-01'), by=1),
           length(unique(top_80p_countries_flight_dat.df$country))),
  country=rep(unique(top_80p_countries_flight_dat.df$country),
              each=length(seq(as.Date('2021-10-01'), as.Date('2022-03-01'), by=1)))
  ) %>%
  mutate(
    month=floor_date(date, unit='month'),
    week=floor_date(date, unit='week')
  ) %>%
  left_join(top_80p_countries_flight_dat.df, by=c('country', 'month')) %>%
  group_by(country, month) %>%
  mutate(totalPassengerVolume.daily=totalPassengerVolume/n()) %>%
  filter(date >= as.Date('2021-10-03') & date <= as.Date('2022-02-26')) %>%
  group_by(country, week) %>%
  summarise(totalPassengerVolume.weekly=sum(totalPassengerVolume.daily))

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

## stacked bar plot
world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df <- world_flight_dat.UK_split.Eng.originCountry.df %>%
  filter(month >= as.Date('2021-11-01') & month < as.Date('2022-02-01')) %>%
  mutate(originCountry=ifelse(originCountry %in% total_80p_countries.ZAF_added, originCountry, 'others')) %>%
  group_by(originCountry, month) %>%
  summarise(totalPassengerVolume=sum(totalPassengerVolume))
world_flight_dat.UK_split.Eng.originCountry.stacked_plot.agg.df <- world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df %>%
  filter(!originCountry %in% c('South Africa', 'others')) %>%
  group_by(originCountry) %>%
  summarise(overall_totalPassengerVolume=sum(totalPassengerVolume)) %>%
  arrange(-overall_totalPassengerVolume)
## add to world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df
world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df$originCountry_ordered <- factor(
  world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df$originCountry,
  levels=c('South Africa',
           unique(world_flight_dat.UK_split.Eng.originCountry.stacked_plot.agg.df$originCountry),
           'others')
)
## compute proportions manually (to fix small kinks in geom_area)
world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df <- world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df %>%
  group_by(month) %>%
  mutate(
    totalPassengerVolume.p=totalPassengerVolume/sum(totalPassengerVolume)
  )
## generate plot
ggplot() +
  geom_bar(dat=world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df %>%
             mutate(month_str=ifelse(month == as.Date('2021-11-01'), 'a',
                                     ifelse(month == as.Date('2021-12-01'), 'b', 'c'))),
           aes(x=month_str, y=totalPassengerVolume/1000000, fill=originCountry_ordered),
           position='stack', stat='identity', width=0.95, color='#484848', alpha=0.8, linewidth=0.2) +
  labs(x='', y='Monthly number of air passengers arriving<br>in England (10^6)') +
  scale_fill_manual(values=country_colours.df[
    match(unique((world_flight_dat.UK_split.Eng.originCountry.stacked_plot.df %>%
                    arrange(originCountry_ordered))$originCountry), country_colours.df$country),]$colour) +
  scale_x_discrete(labels=c('Nov-2021', 'Dec-2021', 'Jan-2022'), expand=c(0.7, 0)) +
  scale_y_continuous(limits=c(0, 3.2), expand=c(0, 0)) +
  coord_cartesian(clip='off') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_markdown(size=13),
    axis.title.x = element_markdown(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/country_air_traffic_monthly_stacked_bars_v2.pdf',
       device='pdf', width=11, height=4.5)

##################################################################
## read in GISAID metadata
##################################################################

gisaid_dat.raw.df <- read.csv('./metadata_20230502_reduced.tsv', sep='\t')
gisaid_dat.df <- gisaid_dat.raw.df %>%
  rename(date=Collection.date)
gisaid_dat.df$date <- as.Date(gisaid_dat.df$date)
gisaid_dat.df$country <- sapply(gisaid_dat.df$Location, function(x) strsplit(x, ' / ')[[1]][2])

## USA -> United States
gisaid_dat.df <- gisaid_dat.df %>%
  mutate(country=ifelse(country == 'USA', 'United States', country))
## label Scotland and Northern Ireland
gisaid_dat.df <- gisaid_dat.df %>%
  mutate(country=ifelse(grepl('Scotland', Location), 'Scotland',
                        ifelse(grepl('Northern Ireland', Location), 'Northern Ireland', country)))
## Canary Islands -> Spain
gisaid_dat.df <- gisaid_dat.df %>%
  mutate(country=ifelse(country == 'Canary Islands', 'Spain', country))

## check data availability
setdiff(unique(top_80p_countries_flight_dat.weekly.df$country),
        unique(gisaid_dat.df$country))

## extract only relevant data
gisaid_dat.top_80p_countries.df <- gisaid_dat.df %>%
  filter(country %in% unique(top_80p_countries_flight_dat.weekly.df$country)) %>%
  select(-Location)

## label BA.1 vs non-BA.1
gisaid_dat.top_80p_countries.df <- gisaid_dat.top_80p_countries.df %>%
  mutate(BA.1=grepl('BA.1', Pango.lineage, ignore.case = TRUE)) %>%
  select(-Pango.lineage)

## aggregate
gisaid_dat.top_80p_countries.df <- gisaid_dat.top_80p_countries.df %>%
  group_by(country, date, BA.1) %>%
  summarise(count=n())

## pivot to wide
gisaid_dat.top_80p_countries.wide.df <- gisaid_dat.top_80p_countries.df %>%
  pivot_wider(names_from=BA.1, values_from=count, values_fill=0) %>%
  rename(
    BA.1=`TRUE`,
    non.BA.1=`FALSE`
  )

## apply smoothing
gisaid_dat.top_80p_countries.wide.smoothed.df <- data.frame(
  date=rep(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1),
           length(unique(top_80p_countries_flight_dat.weekly.df$country))),
  country=rep(unique(top_80p_countries_flight_dat.weekly.df$country),
              each=length(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1)))
) %>%
  left_join(gisaid_dat.top_80p_countries.wide.df, by=c('country', 'date')) %>%
  mutate(
    non.BA.1=ifelse(is.na(non.BA.1), 0, non.BA.1),
    BA.1=ifelse(is.na(BA.1), 0, BA.1)
  ) %>%
  group_by(country) %>%
  mutate(
    cum_count.non.BA.1=cumsum(non.BA.1),
    cum_count.BA.1=cumsum(BA.1),
    w2_preceding.non.BA.1=cum_count.non.BA.1-lag(cum_count.non.BA.1, 13),
    w2_preceding.BA.1=cum_count.BA.1-lag(cum_count.BA.1, 13),
    day14_rm.non.BA.1=lead(cum_count.non.BA.1, 7) - lag(cum_count.non.BA.1, 7),
    day14_rm.BA.1=lead(cum_count.BA.1, 7) - lag(cum_count.BA.1, 7)
  )

## aggregate at weekly level
gisaid_dat.top_80p_countries.weekly.df <- gisaid_dat.top_80p_countries.wide.smoothed.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(country, week) %>%
  summarise(
    w2_preceding.non.BA.1=sum(w2_preceding.non.BA.1, na.rm=T),
    w2_preceding.BA.1=sum(w2_preceding.BA.1, na.rm=T),
    day14_rm.non.BA.1=sum(day14_rm.non.BA.1, na.rm=T),
    day14_rm.BA.1=sum(day14_rm.BA.1, na.rm=T),
    w2_preceding.BA.1.p=w2_preceding.BA.1/(w2_preceding.BA.1 + w2_preceding.non.BA.1),
    w2_preceding.BA.1.p.lw=ifelse(w2_preceding.BA.1 + w2_preceding.non.BA.1 > 0,
                                  as.numeric(binom.test(w2_preceding.BA.1, w2_preceding.BA.1 + w2_preceding.non.BA.1,
                                                        conf.level=0.95)$conf.int[1]), 0),
    w2_preceding.BA.1.p.up=ifelse(w2_preceding.BA.1 + w2_preceding.non.BA.1 > 0,
                                  as.numeric(binom.test(w2_preceding.BA.1, w2_preceding.BA.1 + w2_preceding.non.BA.1,
                                                        conf.level=0.95)$conf.int[2]), 0),
    day14_rm.BA.1.p=day14_rm.BA.1/(day14_rm.BA.1 + day14_rm.non.BA.1),
    day14_rm.BA.1.p.lw=ifelse(day14_rm.BA.1 + day14_rm.non.BA.1 > 0,
                              as.numeric(binom.test(day14_rm.BA.1, day14_rm.BA.1 + day14_rm.non.BA.1,
                                                    conf.level=0.95)$conf.int[1]), 0),
    day14_rm.BA.1.p.up=ifelse(day14_rm.BA.1 + day14_rm.non.BA.1 > 0,
                              as.numeric(binom.test(day14_rm.BA.1, day14_rm.BA.1 + day14_rm.non.BA.1,
                                                    conf.level=0.95)$conf.int[2]), 0),
    non.BA.1=sum(non.BA.1, na.rm=T),
    BA.1=sum(BA.1, na.rm=T),
    BA.1.p=BA.1/(BA.1 + non.BA.1),
    BA.1.p.lw=ifelse(BA.1 + non.BA.1 > 0, as.numeric(binom.test(BA.1, BA.1 + non.BA.1, conf.level=0.95)$conf.int[1]), 0),
    BA.1.p.up=ifelse(BA.1 + non.BA.1 > 0, as.numeric(binom.test(BA.1, BA.1 + non.BA.1, conf.level=0.95)$conf.int[2]), 0)
  )

## impute BA.1.p value for week commencing on 2022-01-30 for United Arab Emirates (and next few weeks)
## take average of nearest available values (w2_preceding.BA.1.p)
UAE_20220123_w2_preceding.BA.1.p_nearest_value <- gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week == as.Date('2022-01-23'),]$w2_preceding.BA.1.p
UAE_20220123_w2_preceding.BA.1.p.lw_nearest_value <- gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week == as.Date('2022-01-23'),]$w2_preceding.BA.1.p.lw
UAE_20220123_w2_preceding.BA.1.p.up_nearest_value <- gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week == as.Date('2022-01-23'),]$w2_preceding.BA.1.p.up
gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week %in% c(as.Date('2022-01-30'),
                                                       as.Date('2022-02-06'),
                                                       as.Date('2022-02-13')),
  ]$w2_preceding.BA.1.p <- UAE_20220123_w2_preceding.BA.1.p_nearest_value
gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week %in% c(as.Date('2022-01-30'),
                                                       as.Date('2022-02-06'),
                                                       as.Date('2022-02-13')),
  ]$w2_preceding.BA.1.p.lw <- UAE_20220123_w2_preceding.BA.1.p.lw_nearest_value
gisaid_dat.top_80p_countries.weekly.df[
  gisaid_dat.top_80p_countries.weekly.df$country == 'United Arab Emirates' &
    gisaid_dat.top_80p_countries.weekly.df$week %in% c(as.Date('2022-01-30'),
                                                       as.Date('2022-02-06'),
                                                       as.Date('2022-02-13')),
  ]$w2_preceding.BA.1.p.up <- UAE_20220123_w2_preceding.BA.1.p.up_nearest_value

## make plot
ymin <- 0
ymax <- 1
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
  geom_line(dat=gisaid_dat.top_80p_countries.weekly.df,
            aes(x=week, y=w2_preceding.BA.1.p, group=country, color=country),
            alpha=0.85, linewidth=0.6) +
  geom_ribbon(dat=gisaid_dat.top_80p_countries.weekly.df,
              aes(x=week, ymin=w2_preceding.BA.1.p.lw, ymax=w2_preceding.BA.1.p.up, group=country, fill=country),
              alpha=0.25, color='transparent') +
  scale_color_manual(values=country_colours.df[
    match(unique(gisaid_dat.top_80p_countries.weekly.df$country), country_colours.df$country),]$colour) +
  scale_fill_manual(values=country_colours.df[
    match(unique(gisaid_dat.top_80p_countries.weekly.df$country), country_colours.df$country),]$colour) +
  labs(x='Week commencing', y='Estimated weekly Omicron BA.1\nrelative prevalence') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
  coord_cartesian(clip='off', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/country_BA.1_rel_prevalence.pdf',
       device='pdf', width=10.8, height=4.5)

##################################################################
## read in OWID data at country-level
##################################################################

owid_case_dat.raw.df <- read.csv("../Case_data/owid-covid-data.csv", sep=',')
owid_case_dat.df <- owid_case_dat.raw.df %>%
  select(iso_code, location, date, total_cases, new_cases, new_cases_smoothed,
         new_tests, new_tests_smoothed, new_tests_per_thousand, new_tests_smoothed_per_thousand,
         positive_rate, tests_per_case, population)
owid_case_dat.df$date <- as.Date(owid_case_dat.df$date)

## check data availability
setdiff(unique(top_80p_countries_flight_dat.weekly.df$country),
        unique(owid_case_dat.df$location))

## extract only relevant data
owid_case_dat.top_80p_countries.df <- owid_case_dat.df %>%
  filter(location %in% unique(top_80p_countries_flight_dat.weekly.df$country) &
           date >= as.Date('2021-09-01') & date <= as.Date('2022-03-01')) %>%
  filter(!location %in% c('Scotland', 'Northern Ireland'))
## case data are not available for Scotland and Northern Ireland

## identifiy missing values
owid_case_dat.top_80p_countries.df %>%
  filter(date >= as.Date('2021-10-01') & date <= as.Date('2022-02-15')) %>%
  filter(is.na(new_cases_smoothed))
## South Africa 2021-11-23
## interpolate by taking mean between nearest values
South_Africa_new_cases_smoothed_20211123_nearest_values <- (
  owid_case_dat.top_80p_countries.df %>%
    filter(location == 'South Africa' & date >= as.Date('2021-11-22') & date <= as.Date('2021-11-24')))$new_cases_smoothed
owid_case_dat.top_80p_countries.df[
  owid_case_dat.top_80p_countries.df$location == 'South Africa' &
    owid_case_dat.top_80p_countries.df$date == as.Date('2021-11-23'),
  ]$new_cases_smoothed <- mean(South_Africa_new_cases_smoothed_20211123_nearest_values, na.rm=T)
## Spain 2021-12-11
## interpolate by taking mean between nearest values
Spain_new_cases_smoothed_20211211_nearest_values <- (
  owid_case_dat.top_80p_countries.df %>%
    filter(location == 'Spain' & date >= as.Date('2021-12-10') & date <= as.Date('2021-12-12')))$new_cases_smoothed
owid_case_dat.top_80p_countries.df[
  owid_case_dat.top_80p_countries.df$location == 'Spain' &
    owid_case_dat.top_80p_countries.df$date == as.Date('2021-12-11'),
  ]$new_cases_smoothed <- mean(Spain_new_cases_smoothed_20211211_nearest_values, na.rm=T)
## Ireland 2022-02-15
## interpolate by taking mean between nearest values
Ireland_new_cases_smoothed_20220215_nearest_values <- (
  owid_case_dat.top_80p_countries.df %>%
    filter(location == 'Ireland' & date >= as.Date('2022-02-14') & date <= as.Date('2022-02-16')))$new_cases_smoothed
owid_case_dat.top_80p_countries.df[
  owid_case_dat.top_80p_countries.df$location == 'Ireland' &
    owid_case_dat.top_80p_countries.df$date == as.Date('2022-02-15'),
  ]$new_cases_smoothed <- mean(Ireland_new_cases_smoothed_20220215_nearest_values, na.rm=T)
## extract only new_cases_smoothed and new_tests_smoothed
owid_case_dat.top_80p_countries.df <- owid_case_dat.top_80p_countries.df %>%
  select(location, date, new_cases_smoothed, new_tests_smoothed, population)

## read in case data for Scotland
Scotland_case_dat.df <- read.csv('./UK-case_data/Scotland_data_2023_specimen_date-Apr-20.csv', sep=',')
Scotland_case_dat.df <- Scotland_case_dat.df %>%
  select(date, newCasesBySpecimenDate) %>%
  rename(new_cases=newCasesBySpecimenDate)
Scotland_case_dat.df$date <- as.Date(Scotland_case_dat.df$date)
Scotland_case_dat.df <- Scotland_case_dat.df %>%
  filter(date >= as.Date('2021-09-01') & date <= as.Date('2022-03-01')) %>%
  arrange(date) %>%
  mutate(
    location='Scotland',
    new_cases_smoothed=rollmean(new_cases, 7, fill=NA),
    population=unique(owid_case_dat.raw.df[owid_case_dat.raw.df$location == 'Scotland',]$population))

## read in test data for Scotland
Scotland_test_dat.df <- read.csv('./UK-case_data/Scotland_test_data_2023-Apr-20.csv', sep=',')
Scotland_test_dat.df <- Scotland_test_dat.df %>%
  select(date, newTestsByPublishDate)
Scotland_test_dat.df$date <- as.Date(Scotland_test_dat.df$date)
Scotland_test_dat.df <- Scotland_test_dat.df %>%
  filter(date >= as.Date('2021-09-01') & date <= as.Date('2022-03-01'))
Scotland_test_dat.df <- Scotland_test_dat.df %>%
  rename(new_tests=newTestsByPublishDate) %>%
  arrange(date) %>%
  mutate(
    new_tests_smoothed=rollmean(new_tests, 7, fill=NA))

## merge Scotland_case_dat.df and Scotland_test_dat.df
Scotland_case_test_dat.df <- Scotland_case_dat.df %>%
  select(location, date, new_cases_smoothed, population) %>%
  left_join(Scotland_test_dat.df %>%
              select(date, new_tests_smoothed), by='date') %>%
  select(location, date, new_cases_smoothed, new_tests_smoothed, population)

## read in case data for Northern Ireland
Northern_Ireland_case_dat.df <- read.csv('./UK-case_data/NorthernIreland_case_data_specimen_date_2023-Apr-20.csv', sep=',')
Northern_Ireland_case_dat.df <- Northern_Ireland_case_dat.df %>%
  select(date, newCasesBySpecimenDate) %>%
  rename(new_cases=newCasesBySpecimenDate)
Northern_Ireland_case_dat.df$date <- as.Date(Northern_Ireland_case_dat.df$date)
Northern_Ireland_case_dat.df <- Northern_Ireland_case_dat.df %>%
  filter(date >= as.Date('2021-09-01') & date <= as.Date('2022-03-01')) %>%
  arrange(date) %>%
  mutate(
    location='Northern Ireland',
    new_cases_smoothed=rollmean(new_cases, 7, fill=NA),
    population=unique(owid_case_dat.raw.df[owid_case_dat.raw.df$location == 'Northern Ireland',]$population))

## read in test data for Northern Ireland
Northern_Ireland_test_dat.df <- read.csv('./UK-case_data/NorthernIreland_test_data_2023-Apr-20.csv', sep=',')
Northern_Ireland_test_dat.df <- Northern_Ireland_test_dat.df %>%
  select(date, newTestsByPublishDate)
Northern_Ireland_test_dat.df$date <- as.Date(Northern_Ireland_test_dat.df$date)
Northern_Ireland_test_dat.df <- Northern_Ireland_test_dat.df %>%
  filter(date >= as.Date('2021-09-01') & date <= as.Date('2022-03-01'))
## investigate anomalous values
ggplot() +
  geom_point(dat=Northern_Ireland_test_dat.df,
             aes(x=date, y=newTestsByPublishDate)) +
  geom_vline(xintercept=Northern_Ireland_test_dat.df[Northern_Ireland_test_dat.df$newTestsByPublishDate == 0,]$date,
             color='red', alpha=0.9, linetype='dashed') +
  geom_vline(xintercept=Northern_Ireland_test_dat.df[Northern_Ireland_test_dat.df$newTestsByPublishDate > 60000,]$date,
             color='blue', alpha=0.9, linetype='dashed')
## impute dates with anomalous values
Northern_Ireland_test_dat.imputed.df <- Northern_Ireland_test_dat.df
smoothing_window <- 7*3 ## 3 weeks (21 days)
dates_of_interest <- Northern_Ireland_test_dat.df[Northern_Ireland_test_dat.df$newTestsByPublishDate == 0 |
                                                    Northern_Ireland_test_dat.df$newTestsByPublishDate > 60000,]$date
smoothing_dat.df <- Northern_Ireland_test_dat.df %>%
  filter(!date %in% dates_of_interest)
## perform cubic-smoothing
df <- 8
smoothing.fit <- smooth.spline(as.Date(smoothing_dat.df$date), smoothing_dat.df$newTestsByPublishDate, df=df)
smoothing_fit.df <- data.frame(
  x=smoothing.fit$x,
  y=smoothing.fit$y
)
## do some plots
ggplot() +
  geom_point(dat=smoothing_dat.df, aes(x=as.numeric(date), y=newTestsByPublishDate)) +
  geom_line(dat=smoothing_fit.df, aes(x=x, y=y)) +
  geom_vline(xintercept=as.numeric(dates_of_interest), color='red')
## extract interpolated value
fitted_vals <- predict(smoothing.fit, as.numeric(dates_of_interest))
print(fitted_vals)
## impute value
Northern_Ireland_test_dat.imputed.df[
  Northern_Ireland_test_dat.imputed.df$date %in% dates_of_interest,]$newTestsByPublishDate <- as.integer(fitted_vals$y)
## compute 7-day rolling averages
Northern_Ireland_test_dat.imputed.df <- Northern_Ireland_test_dat.imputed.df %>%
  rename(new_tests=newTestsByPublishDate) %>%
  arrange(date) %>%
  mutate(
    new_tests_smoothed=rollmean(new_tests, 7, fill=NA))

## merge Northern_Ireland_case_dat.df and Northern_Ireland_test_dat.imputed.df
Northern_Ireland_case_test_dat.df <- Northern_Ireland_case_dat.df %>%
  select(location, date, new_cases_smoothed, population) %>%
  left_join(Northern_Ireland_test_dat.imputed.df %>%
              select(date, new_tests_smoothed), by='date') %>%
  select(location, date, new_cases_smoothed, new_tests_smoothed, population)

## merge with owid_case_dat.top_80p_countries.df
owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.df <- owid_case_dat.top_80p_countries.df %>%
  rbind(Scotland_case_test_dat.df) %>%
  rbind(Northern_Ireland_case_test_dat.df)

## aggregate at weekly level
## only for case per capita for now
owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df <- owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(location, population, week) %>%
  summarise(
    new_cases_smoothed.weekly=sum(new_cases_smoothed),
    new_tests_smoothed.weekly=sum(new_tests_smoothed)
    ) %>%
  filter(week >= as.Date('2021-10-01') & week <= as.Date('2022-02-13')) %>%
  mutate(
    new_cases_smoothed.weekly.p=new_cases_smoothed.weekly/population,
    new_tests_smoothed.weekly.p=new_tests_smoothed.weekly/population,
    positivity.weekly=new_cases_smoothed.weekly/new_tests_smoothed.weekly
    )

## make plots (positivity rate)
ymin <- 0
ymax <- 0.9
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
  geom_line(dat=owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              filter(!location %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                      'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=positivity.weekly, group=location),
            linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              mutate(highlight=location %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                               'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=positivity.weekly,
                group=location, color=location, linewidth=highlight, alpha=highlight)) +
  labs(x='Week commencing', y='\nWeekly average test positivity rate') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df$location),
          country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax), expand=c(0, 0)) +
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
ggsave('./figures/country_weekly_positivity_rate_v3.pdf',
       device='pdf', width=10.8, height=4.5)

##################################################################
## calculate weekly EIIs at country level
##################################################################

## check country consistency
setdiff(unique(owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df$location),
        unique(gisaid_dat.top_80p_countries.weekly.df$country))
setdiff(unique(gisaid_dat.top_80p_countries.weekly.df$country),
        unique(owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df$location))

## merge all necessary dataframes
top_80p_country.weekly_EIIs.df <- top_80p_countries_flight_dat.weekly.df %>%
  left_join(gisaid_dat.top_80p_countries.weekly.df %>%
              select(country, week, w2_preceding.BA.1.p, day14_rm.BA.1.p, BA.1.p), by=c('country', 'week')) %>%
  left_join(owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              ungroup(population) %>%
              select(location, week, new_cases_smoothed.weekly.p, positivity.weekly) %>%
              rename(country=location), by=c('country', 'week')) %>%
  filter(week >= as.Date('2021-10-31') & week < as.Date('2022-02-20'))

## perform calculations
top_80p_country.weekly_EIIs.df <- top_80p_country.weekly_EIIs.df %>%
  mutate(
    EII.weekly.cases_per_capita.w2_preceding=totalPassengerVolume.weekly*new_cases_smoothed.weekly.p*w2_preceding.BA.1.p,
    EII.weekly.cases_per_capita.day14_rm=totalPassengerVolume.weekly*new_cases_smoothed.weekly.p*day14_rm.BA.1.p,
    EII.weekly.positivity.w2_preceding=totalPassengerVolume.weekly*positivity.weekly*w2_preceding.BA.1.p,
    EII.weekly.positivity.day14_rm=totalPassengerVolume.weekly*positivity.weekly*day14_rm.BA.1.p
    )

## make plots
## absolute EIIs (case per capita)
ymin <- 0
ymax <- 1150
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
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.cases_per_capita.w2_preceding, group=country),
            linetype='solid', color='#484848', linewidth=0.5, alpha=0.3) +
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                               'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.cases_per_capita.w2_preceding,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
             linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=200),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
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
ggsave('./figures//EII_weekly.case_per_capita.w2_preceding_smooth.pdf',
       device='pdf', width=7.645, height=4.5)

## absolute EIIs (positivity rate)
ymin <- 0
ymax <- 36
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
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  # geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
  #            linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-10-05'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=5),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10<sup>3</sup>)') +
  # coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  coord_cartesian(clip='on',
                  xlim=c(as.Date('2021-10-18'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_markdown(vjust=2, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/EII_weekly.positivity.w2_preceding_smooth_v3_long.pdf',
       device='pdf', width=7.645, height=4.56)

## inset (enlarged) (case per capita)
ymin <- 0
ymax <- 700
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
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.cases_per_capita.w2_preceding, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.cases_per_capita.w2_preceding,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 2.3)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, 500, by=100),
                     expand=c(0, 0)) +
  labs(x='', y='') +
  coord_cartesian(clip='on', ylim=c(0, 230), xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=14, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/inset-EII_weekly.case_per_capita.w2_preceding_smooth_v2.pdf',
       device='pdf', width=7.645, height=4.5)

## inset (enlarged) (positivity rate)
ymin <- 0
ymax <- 50
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
  # geom_rect(dat=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly.positivity.w2_preceding/1000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 2.3)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=seq(0, ymax, by=1),
                     expand=c(0, 0)) +
  labs(x='', y='') +
  coord_cartesian(clip='on', ylim=c(0, 3.2), xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=18, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=18, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/inset-EII_weekly.positivity_rate.w2_preceding_smooth_v2.pdf',
       # device='pdf', width=7.645, height=4.5)
       device='pdf', width=7.34, height=4.7)

##################################################################
## output EIIs to file
##################################################################

## country-level weekly EIIs using case per capita
write.csv(top_80p_country.weekly_EIIs.df %>%
            select(country, week,
                   EII.weekly.cases_per_capita.w2_preceding, EII.weekly.positivity.w2_preceding),
          file='./output/weekly_country-level_EIIs.csv', row.names=FALSE, quote=FALSE)

##################################################################
## swap in pre-calculated weekly EIIs for USA and ESP (sensitivity analysis)
##################################################################

## read in pre-calculated EIIs for ESP (using case per capita)
ESP_EIIs.case_per_capita.weekly.df <- read.csv('./ESP_AC_weekly_EIIs_case_per_capita.csv')
ESP_EIIs.case_per_capita.weekly.df$week <- as.Date(ESP_EIIs.case_per_capita.weekly.df$week)
## read in pre-calculated EIIs for ESP (using positivity rate)
ESP_EIIs.positivity.weekly.df <- read.csv('./ESP_AC_weekly_EIIs_positivity.csv')
ESP_EIIs.positivity.weekly.df$week <- as.Date(ESP_EIIs.positivity.weekly.df$week)

## read in pre-calculated EIIs for USA (using case per capita)
USA_EIIs.case_per_capita.weekly.df <- read.csv('./USA_state_weekly_EIIs_case_per_capita.csv')
USA_EIIs.case_per_capita.weekly.df$week <- as.Date(USA_EIIs.case_per_capita.weekly.df$week)
## read in pre-calculated EIIs for USE (using positivity rate)
USA_EIIs.positivity.weekly.df <- read.csv('./USA_state_weekly_EIIs_positivity_NJ_imputed.csv')
USA_EIIs.positivity.weekly.df$week <- as.Date(USA_EIIs.positivity.weekly.df$week)

## merge into top_80p_country.weekly_EIIs.df (case per capita)
top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df <- top_80p_country.weekly_EIIs.df %>%
  filter(!country %in% c('United States', 'Spain')) %>%
  select(country, week, EII.weekly.cases_per_capita.w2_preceding) %>%
  rename(EII.weekly=EII.weekly.cases_per_capita.w2_preceding) %>%
  rbind(ESP_EIIs.case_per_capita.weekly.df %>%
          mutate(country='Spain') %>%
          select(country, week, EII.weekly)) %>%
  rbind(USA_EIIs.case_per_capita.weekly.df %>%
          mutate(country='United States') %>%
          select(country, week, EII.weekly)) %>%
  arrange(country, week)

## merge into top_80p_country.weekly_EIIs.df (positivity rate)
top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df <- top_80p_country.weekly_EIIs.df %>%
  filter(!country %in% c('United States', 'Spain')) %>%
  select(country, week, EII.weekly.positivity.w2_preceding) %>%
  rename(EII.weekly=EII.weekly.positivity.w2_preceding) %>%
  rbind(ESP_EIIs.positivity.weekly.df %>%
          mutate(country='Spain') %>%
          select(country, week, EII.weekly)) %>%
  rbind(USA_EIIs.positivity.weekly.df %>%
          mutate(country='United States') %>%
          select(country, week, EII.weekly)) %>%
  arrange(country, week)

## make plots
## absolute EIIs (case per capita)
ymin <- 0
ymax <- 1020
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
  geom_line(dat=top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
             linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df$country), country_colours.df$country),]$colour) +
  # scale_color_manual(values=test) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=200),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII') +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
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
ggsave('./figures/EII_weekly.case_per_capita.USA_ESP_imputed_2.pdf',
       device='pdf', width=7.645, height=4.5)

## absolute EIIs (positivity)
ymin <- 0
ymax <- 2.7
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
  geom_line(dat=top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly/10000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly/10000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  geom_vline(xintercept=c(travel_ban$xmin[1], travel_ban$xmax[1]),
             linetype='dashed', 'black') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df$country), country_colours.df$country),]$colour) +
  # scale_color_manual(values=test) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, ymax, by=0.5),
                     expand=c(0, 0)) +
  labs(x='Week commencing', y='Weekly Omicron BA.1 EII (10^4)') +
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
ggsave('./figures/EII_weekly.positivity.USA_ESP_imputed_v2.pdf',
       device='pdf', width=7.54, height=4.5)

## inset (enlarged) (case per capita)
ymin <- 0
ymax <- 700
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
  geom_line(dat=top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 2.3)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.case_per_capita.USA_ESP_high_res.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, 500, by=50),
                     expand=c(0, 0)) +
  labs(x='', y='') +
  coord_cartesian(clip='on', ylim=c(0, 180), xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=14, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/inset-EII_weekly.case_per_capita.USA_ESP_imputed_v2.pdf',
       device='pdf', width=7.645, height=4.5)

## inset (enlarged) (positivity)
ymin <- 0
ymax <- 3
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
  geom_line(dat=top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df %>%
              filter(!country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                     'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly/10000, group=country),
            linetype='solid', linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df %>%
              mutate(highlight=country %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                              'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=EII.weekly/10000,
                group=country, color=country, alpha=highlight, linewidth=highlight),
            linetype='solid') +
  scale_linewidth_manual(values=c(0.65, 2.3)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(top_80p_country.weekly_EIIs.positivity.USA_ESP_high_res.df$country), country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, ymax),
                     breaks=seq(0, 2, by=0.1),
                     expand=c(0, 0)) +
  labs(x='', y='') +
  coord_cartesian(clip='on', ylim=c(0, 0.35), xlim=c(as.Date('2021-11-07'), as.Date('2021-12-25'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.8, fill=NA),
    axis.title.y.left = element_text(vjust=2, size=13),
    axis.title.y.right = element_text(vjust=2.5, size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=14, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/inset-EII_weekly.positivity_rate.USA_ESP_imputed_v2.pdf',
       device='pdf', width=7.645, height=4.5)

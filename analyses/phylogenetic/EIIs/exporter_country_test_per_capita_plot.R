library(ggplot2)
library(ggtext)
library(lubridate)
library(zoo)
library(stats)
library(dplyr)

##################################################################
## read in and process air passenger data (world, at airport lvl)
##################################################################

world_flight_dat.df <- read.csv('./data/WorldToUK_Oct2021_Feb2022.csv', sep=',')
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

## define colours for the 27 selected countries
# color_scheme <- c(
#   '#1B7883',
#   '#3E4855',
#   '#78424b',
#   '#ad3739',
#   '#F8C55D',
#   '#947A47',
#   '#313030'
# )
# ## specific colours for South Africa, Spain, United States, Ireland, and Scotland
# custom_colours <- c(
#   '#D43F3A',
#   '#0C4C5F',
#   '#E6824C',
#   '#EAB059',
#   '#947A47'
# )
# country_colours.r <- sample(colorRampPalette(color_scheme)(50),
#                           length(total_80p_countries.ZAF_added) - length(custom_colours))
# country_colours <- c(
#   custom_colours,
#   permute(country_colours.r)
# )
## fix country colours
country_colours.df <- data.frame(
  country=c("others", "South Africa", "Spain", "United States", "Italy", "Ireland", "Scotland",
            "Portugal", "Germany", "Poland", "Northern Ireland", "France", "Switzerland", "United Arab Emirates",
            "India", "Turkey", "Netherlands", "Romania", "Denmark", "Greece", "Sweden", "Canada",
            "Pakistan", "Norway", "Cyprus", "Egypt", "Austria", "Nigeria"),
  colour=c("#484848", "#D43F3A", "#0C4C5F", "#E6824C", "#EAB059", "#947A47", "#893E45", "#335059",
           "#D6854C", "#60444F", "#F1B95A", "#313030", "#b38936", "#963B40", "#CD7348", "#CFA654",
           "#1F727D", "#6E424C", "#C29D51", "#7C4149", "#BA513F", "#384E5B", "#1B7883", "#9E8149",
           "#524551", "#B6944E", "#f0b154", "#4B4652")
)

##################################################################
## read in OWID data at country-level
##################################################################

owid_case_dat.raw.df <- read.csv("./data/owid-covid-data.csv", sep=',')
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
Scotland_case_dat.df <- read.csv('./data/Scotland_data_2023_specimen_date-Apr-20.csv', sep=',')
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
Scotland_test_dat.df <- read.csv('./data/Scotland_test_data_2023-Apr-20.csv', sep=',')
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
Northern_Ireland_case_dat.df <- read.csv('./data/NorthernIreland_case_data_specimen_date_2023-Apr-20.csv', sep=',')
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
Northern_Ireland_test_dat.df <- read.csv('./data/NorthernIreland_test_data_2023-Apr-20.csv', sep=',')
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

## make plots (tests per capita)
ymin <- -9.6
ymax <- 0.5
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
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.35) +
  geom_line(dat=owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              filter(!location %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                      'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=log(new_tests_smoothed.weekly.p), group=location),
            color='#484848', linewidth=0.5, alpha=0.3) +
  geom_line(dat=owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              mutate(highlight=location %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                               'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=log(new_tests_smoothed.weekly.p),
                group=location, color=location, linewidth=highlight, alpha=highlight)) +
  labs(x='Week commencing', y='Weekly number of tests per capita (log)') +
  scale_linewidth_manual(values=c(0.65, 1.8)) +
  scale_alpha_manual(values=c(0, 0.9)) +
  scale_color_manual(values=country_colours.df[
    match(unique(owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df$location),
          country_colours.df$country),]$colour) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax), expand=c(0, 0)) +
  coord_cartesian(clip='on', xlim=c(as.Date('2021-11-07'), as.Date('2022-02-06'))) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color='black', linewidth=0.8, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/country_weekly_test_per_capita.pdf',
       device='pdf', width=7.645, height=4.5)

## make plots (case per capita)
ymin <- 0
ymax <- 0.056
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
            aes(x=week, y=new_cases_smoothed.weekly.p, group=location),
            linewidth=0.5, alpha=0.3, color='#484848') +
  geom_line(dat=owid_case_test_dat.top_80p_countries.Scotland.Northern_Ireland.weekly.df %>%
              mutate(highlight=location %in% c('South Africa', 'Spain', 'United States', 'Ireland',
                                               'Netherlands', 'Scotland', 'Nigeria', 'Austria')),
            aes(x=week, y=new_cases_smoothed.weekly.p,
                group=location, color=location, linewidth=highlight, alpha=highlight)) +
  labs(x='Week commencing', y='Weekly number of reported cases per capita') +
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
    panel.border = element_rect(color='black', linewidth=0.8, fill=NA),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13, vjust=-1, color='black'),
    axis.text.x = element_text(face=1, size=11, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11, color='black'),
    plot.margin = unit(c(0.5, 1, 0.3, 0.5), "cm")
  )

## output plot to file
ggsave('./figures/country_weekly_case_per_capita_v2.pdf',
       device='pdf', width=7.645, height=4.5)


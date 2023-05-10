library(tidyr)
library(ggplot2)
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

## read in USA airport <-> state mapping
USA_airport_state_map.df <- read.csv('./data/USA_airport-state_map.csv', sep=',')
USA_airport_state_map.df[USA_airport_state_map.df$airport == 'Lansing Capital City Airport',]$airport <- 'Lansing, Capital City Airport'
USA_airport_state_map.df[USA_airport_state_map.df$airport == 'Rochester International Airport',]$airport <- 'Rochester, Rochester International Airport'

## read in ESP airport <-> autonomous_community mapping
ESP_airport_ac_map.df <- read.csv('./data/ESP_airport-ac_map.csv', sep=',')

## assign USA and ESP airports to their corresponding state/autonomous_community, under originLocation
world_flight_dat.UK_split.df <- world_flight_dat.UK_split.df %>%
  left_join(USA_airport_state_map.df %>%
              rename(
                originLocationName=airport,
                originLocation=state), by='originLocationName') %>%
  left_join(ESP_airport_ac_map.df %>%
              rename(
                originLocationName=airport,
                originLocation=autonomous_community), by='originLocationName') %>%
  mutate(originLocation=ifelse(!is.na(originLocation.x) & originCountry == 'United States', originLocation.x,
                               ifelse(!is.na(originLocation.y) & originCountry == 'Spain', originLocation.y, originCountry))) %>%
  select(-originLocation.x, -originLocation.y)

## look at only air passengers arriving in English airports
world_flight_dat.UK_split.Eng.df <- world_flight_dat.UK_split.df %>%
  filter(destCountry == 'England' & originCountry != 'England')

## US states with incoming air traffic
unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation)
## New York, California, Florida, New Jersey, Nevada, Massachusetts, Virginia, Illinois, Texas,
## Georgia, Colorado, Washington, Arizona, Pennsylvania, North Carolina, Utah, Michigan, Minnesota,
## Louisiana, Oregon, Tennessee, Ohio, Kentucky, Indiana, Hawaii, Missouri, Maryland, South Carolina,
## New Mexico, Oklahoma, Nebraska, Iowa, Idaho, Alabama, Montana, Alaska, Arkansas, Kansas, Wyoming,
## Wisconsin, Vermont, Connecticut, Mississippi, Maine, North Dakota, South Dakota, West Virginia,
## Rhode Island, New Hampshire

## Spanish autonomous communities with incoming air traffic
unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation)
## Community of Madrid, Catalonia, Canary Islands, Andalusia, Valencian Community, Balearic Islands,
## Region of Murcia, Basque Country, Galicia, Aragon, Cantabria, Asturias, Navarre, Castile and Leon,
## Melilla, La Rioja, Extremadura
## no flights from Castilla-La Mancha and Ciudad de Ceuta

## aggregate over origin airports by originLocation
## take only United States and Spain
world_flight_dat.UK_split.Eng.agg.df <- world_flight_dat.UK_split.Eng.df %>%
  filter(originCountry %in% c('United States', 'Spain')) %>%
  group_by(aggregationMonth, originCountry, originLocation) %>%
  summarise(totalPassengerVolume=sum(totalPassengerVolume))
world_flight_dat.UK_split.Eng.agg.df$year <- sapply(world_flight_dat.UK_split.Eng.agg.df$aggregationMonth,
                                                    function(x) as.numeric(strsplit(x, '-')[[1]][1]))
world_flight_dat.UK_split.Eng.agg.df$month <- sapply(world_flight_dat.UK_split.Eng.agg.df$aggregationMonth,
                                                     function(x) as.numeric(strsplit(x, '-')[[1]][2]))
world_flight_dat.UK_split.Eng.agg.df <- world_flight_dat.UK_split.Eng.agg.df %>%
  ungroup(aggregationMonth) %>%
  select(-aggregationMonth) %>%
  mutate(month=ifelse(month == 10, as.Date('2021-10-01'),
                      ifelse(month == 11, as.Date('2021-11-01'),
                             ifelse(month == 12, as.Date('2021-12-01'),
                                    ifelse(month == 1, as.Date('2022-01-01'),
                                           as.Date('2022-02-01')))))) %>%
  select(-year)
world_flight_dat.UK_split.Eng.agg.df$month <- as.Date(world_flight_dat.UK_split.Eng.agg.df$month)

## disaggregate data into weekly level
world_flight_dat.UK_split.Eng.agg.weekly.df <- data.frame(
  date=rep(seq(as.Date('2021-10-01'), as.Date('2022-03-01'), by=1),
           length(unique(world_flight_dat.UK_split.Eng.agg.df$originLocation))),
  originLocation=rep(unique(world_flight_dat.UK_split.Eng.agg.df$originLocation),
                     each=length(seq(as.Date('2021-10-01'), as.Date('2022-03-01'), by=1)))) %>%
  mutate(
    month=floor_date(date, unit='month'),
    week=floor_date(date, unit='week')
  ) %>%
  left_join(world_flight_dat.UK_split.Eng.agg.df, by=c('originLocation', 'month')) %>%
  mutate(totalPassengerVolume=ifelse(is.na(totalPassengerVolume), 0, totalPassengerVolume)) %>%
  group_by(originLocation, month) %>%
  mutate(totalPassengerVolume.daily=totalPassengerVolume/n()) %>%
  filter(date >= as.Date('2021-10-03') & date <= as.Date('2022-02-26')) %>%
  group_by(originLocation, week) %>%
  summarise(totalPassengerVolume.weekly=sum(totalPassengerVolume.daily))
world_flight_dat.UK_split.Eng.agg.weekly.df <- world_flight_dat.UK_split.Eng.agg.weekly.df %>%
  left_join(unique(world_flight_dat.UK_split.Eng.agg.df[c('originCountry', 'originLocation')]), by='originLocation')

##################################################################
## read in ECDC case/test data (for Spain)
##################################################################

ECDC_case_test_dat.df <- read.csv('./data/ECDC_weekly_testing_data_EUEEAUK_2022-09-15.csv')
ESP_case_test_dat.df <- ECDC_case_test_dat.df %>%
  select(country, year_week, level, region_name, new_cases, tests_done, population, positivity_rate) %>%
  filter(!startsWith(year_week, '2020') & country == 'Spain' & level == 'subnational')
ESP_case_test_dat.df$week <- as.Date(sapply(ESP_case_test_dat.df$year_week, function(x) as.Date(paste(x, '1', sep = "-"), '%Y-W%U-%u')))

## Monday is used as the start of each week in this data
## we map them to their corresponding closest (previous) Sunday
ESP_case_test_dat.df <- ESP_case_test_dat.df %>%
  mutate(week=week-1) %>%
  select(-year_week, -level, -country) %>%
  rename(location=region_name)

## check names consistency
setdiff(unique(ESP_case_test_dat.df$location),
        unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation))
setdiff(unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation),
        unique(ESP_case_test_dat.df$location))
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Principado de Asturias',]$location <- 'Asturias'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'País Vasco',]$location <- 'Basque Country'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Comunidad Foral de Navarra',]$location <- 'Navarre'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Aragón',]$location <- 'Aragon'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Comunidad de Madrid',]$location <- 'Community of Madrid'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Castilla y León',]$location <- 'Castile and Leon'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Cataluña',]$location <- 'Catalonia'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Comunitat Valenciana',]$location <- 'Valencian Community'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Illes Balears',]$location <- 'Balearic Islands'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Andalucía',]$location <- 'Andalusia'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Región de Murcia',]$location <- 'Region of Murcia'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Ciudad de Melilla',]$location <- 'Melilla'
ESP_case_test_dat.df[ESP_case_test_dat.df$location == 'Canarias',]$location <- 'Canary Islands'

## extract relevant data
ESP_case_test_dat.df <- ESP_case_test_dat.df %>%
  filter(location %in% unique(world_flight_dat.UK_split.Eng.df[
    world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation) &
      week >= as.Date('2021-10-17') & week <= as.Date('2022-02-13'))

## calculate new_cases per capita
ESP_case_test_dat.df <- ESP_case_test_dat.df %>%
  mutate(
    new_cases.p=new_cases/population,
    positivity_rate=positivity_rate/100)

## data for week 2021-12-19 is missing
## interpolate by taking average of nearest values
ESP_20211219_imputation.new_cases.p <- (ESP_case_test_dat.df[ESP_case_test_dat.df$week == as.Date('2021-12-12'),]$new_cases.p +
                                          ESP_case_test_dat.df[ESP_case_test_dat.df$week == as.Date('2021-12-26'),]$new_cases.p)/2
ESP_20211219_imputation.positivity_rate <- (ESP_case_test_dat.df[ESP_case_test_dat.df$week == as.Date('2021-12-12'),]$positivity_rate +
                                              ESP_case_test_dat.df[ESP_case_test_dat.df$week == as.Date('2021-12-26'),]$positivity_rate)/2
ESP_case_test_dat.20211219_imputed.df <- ESP_case_test_dat.df %>%
  select(location, week, new_cases.p, positivity_rate) %>%
  rbind(data.frame(
    location=unique(ESP_case_test_dat.df$location),
    week=rep(as.Date('2021-12-19'), length(unique(ESP_case_test_dat.df$location))),
    new_cases.p=ESP_20211219_imputation.new_cases.p,
    positivity_rate=ESP_20211219_imputation.positivity_rate)) %>%
  arrange(location, week)

##################################################################
## process GISAID metadata
##################################################################

gisaid_dat.df <- read.csv('./data/metadata_20230502_reduced.tsv', sep='\t')
gisaid_dat.df <- gisaid_dat.df %>%
  rename(date=Collection.date)
gisaid_dat.df$date <- as.Date(gisaid_dat.df$date)
gisaid_dat.df$country <- sapply(gisaid_dat.df$Location, function(x) strsplit(x, ' / ')[[1]][2])

## USA -> United States
gisaid_dat.df[gisaid_dat.df$country == 'USA',]$country <- 'United States'

## extract only data for United States and Spain
USA_ESP_gisaid_dat.df <- gisaid_dat.df %>%
  filter(country %in% c('United States', 'Spain', 'Canary Islands')) %>%
  rename(gisaid.location=Location)
USA_ESP_gisaid_dat.df$location <- sapply(USA_ESP_gisaid_dat.df$gisaid.location, function(x) strsplit(x, ' / ')[[1]][3])
USA_ESP_gisaid_dat.df <- USA_ESP_gisaid_dat.df %>%
  mutate(
    location=ifelse(country == 'Canary Islands', 'Canary Islands', location),
    country=ifelse(country == 'Canary Islands', 'Spain', country)
  )
USA_ESP_gisaid_dat.df <- USA_ESP_gisaid_dat.df[complete.cases(USA_ESP_gisaid_dat.df),]

## check names consistency (Spain)
setdiff(unique(USA_ESP_gisaid_dat.df[USA_ESP_gisaid_dat.df$country == 'Spain',]$location),
        unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation))
setdiff(unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation),
        unique(USA_ESP_gisaid_dat.df[USA_ESP_gisaid_dat.df$country == 'Spain',]$location))
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Madrid',
  ]$location <- 'Community of Madrid'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Castilla y Leon',
  ]$location <- 'Castile and Leon'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Balearic Island',
  ]$location <- 'Balearic Islands'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Comunitat Valenciana',
  ]$location <- 'Valencian Community'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Catalunya',
  ]$location <- 'Catalonia'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Logrono',
  ]$location <- 'La Rioja'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Albacete',
  ]$location <- 'Castilla-La Mancha'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Navarra',
  ]$location <- 'Navarre'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Murcia',
  ]$location <- 'Region of Murcia'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Cuenca',
  ]$location <- 'Castilla-La Mancha'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Ceuta',
  ]$location <- 'Ciudad de Ceuta'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Sevilla',
  ]$location <- 'Andalusia'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Toledo',
  ]$location <- 'Castilla-La Mancha'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Castilla–La Mancha',
  ]$location <- 'Castilla-La Mancha'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Barcelona',
  ]$location <- 'Catalonia'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'FUERNGIROLA',
  ]$location <- 'Andalusia'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Comunitat_Valenciana',
  ]$location <- 'Valencian Community'
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'Spain' & USA_ESP_gisaid_dat.df$location == 'Albacet',
  ]$location <- 'Castilla-La Mancha'

## check names consistency (United States)
setdiff(unique(USA_ESP_gisaid_dat.df[USA_ESP_gisaid_dat.df$country == 'United States',]$location),
        unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation))
setdiff(unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation),
        unique(USA_ESP_gisaid_dat.df[USA_ESP_gisaid_dat.df$country == 'United States',]$location))
USA_ESP_gisaid_dat.df[
  USA_ESP_gisaid_dat.df$country == 'United States' & USA_ESP_gisaid_dat.df$location == 'Southwest',
  ]$location <- 'New Mexico'
## remove rows with location = Delaware, Un, and nan
USA_ESP_gisaid_dat.df <- USA_ESP_gisaid_dat.df %>%
  filter(!location %in% c('District of Columbia', 'Delaware', 'Un', 'nan'))

## label BA.1 vs non-BA.1
USA_ESP_gisaid_dat.df <- USA_ESP_gisaid_dat.df %>%
  mutate(BA.1=grepl('BA.1', Pango.lineage, ignore.case = TRUE)) %>%
  select(-Pango.lineage)

## aggregate data
USA_ESP_gisaid_dat.processed.df <- USA_ESP_gisaid_dat.df %>%
  group_by(country, location, date, BA.1) %>%
  summarise(count=n())

## pivot to wide
USA_ESP_gisaid_dat.processed.wide.df <- USA_ESP_gisaid_dat.processed.df %>%
  pivot_wider(names_from=BA.1, values_from=count, values_fill=0) %>%
  rename(
    BA.1=`TRUE`,
    non.BA.1=`FALSE`
  )

## extract only locations with air traffic
USA_ESP_gisaid_dat.processed.wide.df <- USA_ESP_gisaid_dat.processed.wide.df %>%
  filter(
    (country == 'Spain' &
       location %in% unique(world_flight_dat.UK_split.Eng.df[
         world_flight_dat.UK_split.Eng.df$originCountry == 'Spain',]$originLocation)) |
      (country == 'United States' &
         location %in% unique(world_flight_dat.UK_split.Eng.df[
           world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation)))

#### USA
## apply smoothing
USA_gisaid_dat.processed.wide.smoothed.df <- data.frame(
  date=rep(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1),
           length(unique(USA_ESP_gisaid_dat.processed.wide.df[USA_ESP_gisaid_dat.processed.wide.df$country == 'United States',]$location))),
  location=rep(unique(USA_ESP_gisaid_dat.processed.wide.df[USA_ESP_gisaid_dat.processed.wide.df$country == 'United States',]$location),
               each=length(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1)))) %>%
  left_join(USA_ESP_gisaid_dat.processed.wide.df %>%
              filter(country == 'United States') %>%
              ungroup(country) %>%
              select(-country), by=c('location', 'date')) %>%
  mutate(
    non.BA.1=ifelse(is.na(non.BA.1), 0, non.BA.1),
    BA.1=ifelse(is.na(BA.1), 0, BA.1)
  ) %>%
  group_by(location) %>%
  mutate(
    cum_count.non.BA.1=cumsum(non.BA.1),
    cum_count.BA.1=cumsum(BA.1),
    w2_preceding.non.BA.1=cum_count.non.BA.1-lag(cum_count.non.BA.1, 13),
    w2_preceding.BA.1=cum_count.BA.1-lag(cum_count.BA.1, 13),
    day14_rm.non.BA.1=lead(cum_count.non.BA.1, 7) - lag(cum_count.non.BA.1, 7),
    day14_rm.BA.1=lead(cum_count.BA.1, 7) - lag(cum_count.BA.1, 7)
  )

## aggregate at weekly level
USA_gisaid_dat.BA.1.weekly.df <- USA_gisaid_dat.processed.wide.smoothed.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(location, week) %>%
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
USA_gisaid_dat.national.BA.1.weekly.df <- USA_gisaid_dat.processed.wide.smoothed.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    w2_preceding.non.BA.1=sum(w2_preceding.non.BA.1, na.rm=T),
    w2_preceding.BA.1=sum(w2_preceding.BA.1, na.rm=T),
    w2_preceding.BA.1.p=w2_preceding.BA.1/(w2_preceding.BA.1 + w2_preceding.non.BA.1),
    w2_preceding.BA.1.p.lw=ifelse(w2_preceding.BA.1 + w2_preceding.non.BA.1 > 0,
                                  as.numeric(binom.test(w2_preceding.BA.1, w2_preceding.BA.1 + w2_preceding.non.BA.1,
                                                        conf.level=0.95)$conf.int[1]), 0),
    w2_preceding.BA.1.p.up=ifelse(w2_preceding.BA.1 + w2_preceding.non.BA.1 > 0,
                                  as.numeric(binom.test(w2_preceding.BA.1, w2_preceding.BA.1 + w2_preceding.non.BA.1,
                                                        conf.level=0.95)$conf.int[2]), 0),
  )

## make plot for sanity checks
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
  geom_line(dat=USA_gisaid_dat.BA.1.weekly.df,
            aes(x=week, y=w2_preceding.BA.1.p, group=location),
            alpha=0.4, linewidth=0.2, color='#484848') +
  geom_line(dat=USA_gisaid_dat.national.BA.1.weekly.df,
            aes(x=week, y=w2_preceding.BA.1.p),
            alpha=0.95, linewidth=2, color='#484848') +
  labs(x='Week commencing', y='Estimated weekly Omicron BA.1\nrelative prevalence') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
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
ggsave('./figures/USA_BA.1_rel_prevalence.pdf',
       device='pdf', width=7.645, height=4.5)

#### ESP
## calculate national average, apply smoothing
ESP_gisaid_dat.national.wide.df <- data.frame(
  date=seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1)) %>%
  left_join(
    USA_ESP_gisaid_dat.processed.wide.df %>%
      filter(country == 'Spain') %>%
      group_by(date) %>%
      summarise(
        non.BA.1=sum(non.BA.1),
        BA.1=sum(BA.1)
      ), by='date'
  ) %>%
  mutate(
    non.BA.1=ifelse(is.na(non.BA.1), 0, non.BA.1),
    BA.1=ifelse(is.na(BA.1), 0, BA.1)
  ) %>%
  mutate(
    cum_count.non.BA.1=cumsum(non.BA.1),
    cum_count.BA.1=cumsum(BA.1),
    w2_preceding.non.BA.1=cum_count.non.BA.1-lag(cum_count.non.BA.1, 13),
    w2_preceding.BA.1=cum_count.BA.1-lag(cum_count.BA.1, 13),
    day14_rm.non.BA.1=lead(cum_count.non.BA.1, 7) - lag(cum_count.non.BA.1, 7),
    day14_rm.BA.1=lead(cum_count.BA.1, 7) - lag(cum_count.BA.1, 7)
  )
## aggregate at weekly level
ESP_gisaid_dat.national.weekly.df <- ESP_gisaid_dat.national.wide.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
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

## autonomous community-specific BA.1 relative prevalence
ESP_gisaid_dat.CA.wide.df <- data.frame(
  date=rep(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1),
           length(unique((USA_ESP_gisaid_dat.processed.wide.df %>%
                            filter(country == 'Spain' &
                                     !location %in% c('Aragon', 'Asturias', 'Melilla', 'Cantabria',
                                                      'Region of Murcia')))$location))),
  location=rep(unique((USA_ESP_gisaid_dat.processed.wide.df %>%
                         filter(country == 'Spain' &
                                  !location %in% c('Aragon', 'Asturias', 'Melilla', 'Cantabria',
                                                   'Region of Murcia')))$location),
               each=length(seq(as.Date('2021-09-01'), as.Date('2022-03-01'), by=1)))
  ) %>%
  left_join(USA_ESP_gisaid_dat.processed.wide.df %>%
              filter(country == 'Spain') %>%
              ungroup(country) %>%
              select(-country), by=c('location', 'date')) %>%
  mutate(
    non.BA.1=ifelse(is.na(non.BA.1), 0, non.BA.1),
    BA.1=ifelse(is.na(BA.1), 0, BA.1)
  ) %>%
  mutate(
    cum_count.non.BA.1=cumsum(non.BA.1),
    cum_count.BA.1=cumsum(BA.1),
    w2_preceding.non.BA.1=cum_count.non.BA.1-lag(cum_count.non.BA.1, 13),
    w2_preceding.BA.1=cum_count.BA.1-lag(cum_count.BA.1, 13),
    day14_rm.non.BA.1=lead(cum_count.non.BA.1, 7) - lag(cum_count.non.BA.1, 7),
    day14_rm.BA.1=lead(cum_count.BA.1, 7) - lag(cum_count.BA.1, 7)
  )
## aggregate at weekly level
ESP_gisaid_dat.CA.weekly.df <- ESP_gisaid_dat.CA.wide.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(location, week) %>%
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

## make plot for sanity checks
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
  geom_line(dat=ESP_gisaid_dat.CA.weekly.df,
            aes(x=week, y=w2_preceding.BA.1.p, group=location),
            alpha=0.4, linewidth=0.3, color='#484848') +
  geom_line(dat=ESP_gisaid_dat.national.weekly.df,
            aes(x=week, y=w2_preceding.BA.1.p),
            alpha=0.95, linewidth=2, color='#484848') +
  labs(x='Week commencing', y='Estimated weekly Omicron BA.1\nrelative prevalence') +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
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
ggsave('./figures/ESP_BA.1_rel_prevalence.pdf',
       device='pdf', width=7.645, height=4.5)

## impute values for Aragon, Asturias, Melilla, Cantabria, Navarre, Region of Murcia
ESP_gisaid_dat.CA.imputed.weekly.df <- ESP_gisaid_dat.CA.weekly.df %>%
  rbind(
    cbind(location='Aragon', ESP_gisaid_dat.national.weekly.df)
  ) %>%
  rbind(
    cbind(location='Asturias', ESP_gisaid_dat.national.weekly.df)
  ) %>%
  rbind(
    cbind(location='Melilla', ESP_gisaid_dat.national.weekly.df)
  ) %>%
  rbind(
    cbind(location='Cantabria', ESP_gisaid_dat.national.weekly.df)
  ) %>%
  rbind(
    cbind(location='Region of Murcia', ESP_gisaid_dat.national.weekly.df)
  )

##################################################################
## process US state-level case/test data
##################################################################

USA_state_test_dat.df <- read.csv('./data/time_series_covid19_US.csv', sep=',')
USA_state_test_dat.df$date <- as.Date(USA_state_test_dat.df$date, format='%m/%d/%Y')
USA_state_test_dat.df <- USA_state_test_dat.df %>%
  filter(date >= as.Date('2021-10-01') & date <= as.Date('2022-03-01'))

## read in US state abbreviation
USA_state_abbr.df <- read.csv('./data/USA_state_abbreviation.csv', sep=',')
## merge with USA_state_test_dat.df
USA_state_test_dat.df <- USA_state_test_dat.df %>%
  rename(abbr=state) %>%
  left_join(USA_state_abbr.df, by='abbr') %>%
  select(-abbr)

## check name consistency
setdiff(unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation),
        unique(USA_state_test_dat.df$state))
setdiff(unique(USA_state_test_dat.df$state),
        unique(world_flight_dat.UK_split.Eng.df[world_flight_dat.UK_split.Eng.df$originCountry == 'United States',]$originLocation))

## the following do not incoming air traffic: American Samoa, District of Columbia, Delaware, Guam,
## Northern Mariana Islands, Puerto Rico, US Virgin Islands
## remove
USA_state_test_dat.df <- USA_state_test_dat.df %>%
  filter(!state %in% c('American Samoa', 'District of Columbia', 'Delaware', 'Guam',
                       'Northern Mariana Islands', 'Puerto Rico', 'US Virgin Islands'))

## read in US state-level population data (2021: https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html)
USA_pop.df <- read.csv('./data/USA_state_population.csv', sep=',')
USA_pop.df <- USA_pop.df %>%
  select(NAME, POPESTIMATE2021) %>%
  rename(
    state=NAME,
    pop_est=POPESTIMATE2021
  )

## check name consistency
setdiff(unique(USA_state_test_dat.df$state), unique(USA_pop.df$state))
setdiff(unique(USA_pop.df$state), unique(USA_state_test_dat.df$state))

## merge with USA_state_test_dat.df
USA_state_test_dat.df <- USA_state_test_dat.df %>%
  left_join(USA_pop.df, by='state')

# read in US state case data, source: https://github.com/nytimes/covid-19-data
USA_state_case_dat.df <- read.csv(file = './data/us-states_nyt_case_data.csv', header = TRUE)
USA_state_case_dat.df$date <- as.Date(USA_state_case_dat.df$date)

## check name consistency
setdiff(unique(USA_state_test_dat.df$state), unique(USA_state_case_dat.df$state))
setdiff(unique(USA_state_case_dat.df$state), unique(USA_state_test_dat.df$state))

## extract only relevant data
USA_state_case_dat.df <- USA_state_case_dat.df %>%
  filter(date >= as.Date('2021-10-01') & date <= as.Date('2022-03-01') &
           state %in% unique(USA_state_test_dat.df$state))

## calculate daily new cases
USA_state_case_dat.df <- USA_state_case_dat.df %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(new_cases=cases-lag(cases, 1)) %>%
  select(c(state, date, new_cases)) %>%
  filter(!is.na(new_cases))

## find States with anomalous case number
USA_state_case_dat.df[USA_state_case_dat.df$new_cases < 0,]
## Colorado	2022-01-29	-4678
## Pennsylvania	2022-02-08	-4397

## impute "case-recall" data points (where appropriate)
USA_state_case_dat.imputed.df <- USA_state_case_dat.df
date_of_interest <- as.Date('2022-02-08')
state_of_interest <- 'Pennsylvania'
# date_of_interest <- as.Date('2022-01-29')
# state_of_interest <- 'Colorado'
smoothing_window <- 7*3 ## 3 weeks (21 days)
smoothing_dat.df <- USA_state_case_dat.df %>%
  filter(date >= date_of_interest - smoothing_window & date <= date_of_interest + smoothing_window & state == state_of_interest)
## do some plots
ggplot() +
  geom_point(dat=smoothing_dat.df, aes(x=date, y=new_cases)) +
  geom_vline(xintercept=as.numeric(date_of_interest), color='red')
## perform cubic-smoothing
df <- 5
smoothing.fit <- smooth.spline(as.Date((smoothing_dat.df %>% filter(date != date_of_interest))$date),
                               (smoothing_dat.df %>% filter(date != date_of_interest))$new_cases, df=df)
smoothing_fit.df <- data.frame(
  x=smoothing.fit$x,
  y=smoothing.fit$y
)
## do some plots
ggplot() +
  geom_point(dat=smoothing_dat.df, aes(x=as.numeric(date), y=new_cases)) +
  geom_line(dat=smoothing_fit.df, aes(x=x, y=y)) +
  geom_vline(xintercept=as.numeric(date_of_interest), color='red')
## extract interpolated value
fitted_val <- predict(smoothing.fit, as.numeric(date_of_interest))
print(fitted_val)
## impute value
USA_state_case_dat.imputed.df[USA_state_case_dat.imputed.df$date == date_of_interest &
                                USA_state_case_dat.imputed.df$state == state_of_interest,]$new_cases <- as.integer(fitted_val$y)

## calculate 7-day rolling average before weekly aggregation
USA_state_case_dat.imputed.df <- USA_state_case_dat.imputed.df %>%
  group_by(state) %>%
  mutate(new_cases.7day_rm=rollmean(new_cases, 7, fill=NA))

## aggregate at state/weekly level
USA_state_case_dat.imputed.weekly.df <- USA_state_case_dat.imputed.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(state, week) %>%
  summarise(new_cases.weekly=sum(new_cases.7day_rm)) %>%
  filter(week >= as.Date('2021-10-01') & week <= as.Date('2022-02-13')) %>%
  arrange(state, week)

## merge USA_pop.df with USA_state_case_dat.imputed.weekly.df and calculate weekly case per capita
USA_state_case_dat.imputed.weekly.df <- USA_state_case_dat.imputed.weekly.df %>%
  filter(!is.na(new_cases.weekly)) %>%
  left_join(USA_pop.df, by='state') %>%
  mutate(new_cases.weekly.p=new_cases.weekly/pop_est)

## calculate weekly EIIs for USA from case per capita
USA_state_EII.weekly.case_per_capita.df <- world_flight_dat.UK_split.Eng.agg.weekly.df %>%
  filter(originCountry == 'United States' & week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
  select(-originCountry) %>%
  rename(state=originLocation) %>%
  left_join(USA_state_case_dat.imputed.weekly.df, by=c('state', 'week')) %>%
  left_join(USA_gisaid_dat.BA.1.weekly.df %>%
              filter(week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
              select(location, week, w2_preceding.BA.1.p) %>%
              rename(state=location), by=c('state', 'week'))
USA_state_EII.weekly.case_per_capita.df <- USA_state_EII.weekly.case_per_capita.df %>%
  mutate(EII.weekly=totalPassengerVolume.weekly*w2_preceding.BA.1.p*new_cases.weekly.p)

## make plot
ymin <- 0
ymax <- 1000
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
  geom_line(dat=USA_state_EII.weekly.case_per_capita.df,
            aes(x=week, y=EII.weekly, group=state, color=state),
            color='#484848', linetype='dashed', alpha=0.4, linewidth=0.5) +
  geom_line(dat=USA_state_EII.weekly.case_per_capita.df %>%
              group_by(week) %>%
              summarise(EII.weekly=sum(EII.weekly)),
            aes(x=week, y=EII.weekly), linetype='solid', color='#484848', alpha=0.9, linewidth=1.2) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
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
ggsave('./figures/USA_state_BA.1_EII.case_per_capita.pdf',
       device='pdf', width=7.645, height=4.5)

## calculate weekly EIIs for ESP from case per capita
ESP_ca_EII.weekly.case_per_capita.df <- world_flight_dat.UK_split.Eng.agg.weekly.df %>%
  filter(originCountry == 'Spain' & week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
  select(-originCountry) %>%
  rename(ca=originLocation) %>%
  left_join(ESP_case_test_dat.20211219_imputed.df %>%
              rename(ca=location) %>%
              select(ca, week, new_cases.p), by=c('ca', 'week')) %>%
  left_join(ESP_gisaid_dat.CA.imputed.weekly.df %>%
              filter(week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
              select(location, week, w2_preceding.BA.1.p) %>%
              rename(ca=location), by=c('ca', 'week'))
ESP_ca_EII.weekly.case_per_capita.df <- ESP_ca_EII.weekly.case_per_capita.df %>%
  mutate(EII.weekly=totalPassengerVolume.weekly*w2_preceding.BA.1.p*new_cases.p)

## make plot
ymin <- 0
ymax <- 1000
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
  geom_line(dat=ESP_ca_EII.weekly.case_per_capita.df,
            aes(x=week, y=EII.weekly, group=ca, color=ca),
            color='#484848', linetype='dashed', alpha=0.4, linewidth=0.5) +
  geom_line(dat=ESP_ca_EII.weekly.case_per_capita.df %>%
              group_by(week) %>%
              summarise(EII.weekly=sum(EII.weekly)),
            aes(x=week, y=EII.weekly), linetype='solid', color='#484848', alpha=0.9, linewidth=1.2) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
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
ggsave('./figures/ESP_ca_BA.1_EII.case_per_capita.pdf',
       device='pdf', width=7.645, height=4.5)

## calculate weekly EIIs for ESP from positivity rate
ESP_ca_EII.weekly.positivity.df <- world_flight_dat.UK_split.Eng.agg.weekly.df %>%
  filter(originCountry == 'Spain' & week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
  select(-originCountry) %>%
  rename(ca=originLocation) %>%
  left_join(ESP_case_test_dat.20211219_imputed.df %>%
              rename(ca=location) %>%
              select(ca, week, positivity_rate), by=c('ca', 'week')) %>%
  left_join(ESP_gisaid_dat.CA.imputed.weekly.df %>%
              filter(week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
              select(location, week, w2_preceding.BA.1.p) %>%
              rename(ca=location), by=c('ca', 'week'))
ESP_ca_EII.weekly.positivity.df <- ESP_ca_EII.weekly.positivity.df %>%
  mutate(EII.weekly=totalPassengerVolume.weekly*w2_preceding.BA.1.p*positivity_rate)

## make plot
ymin <- 0
ymax <- 27000
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
  geom_line(dat=ESP_ca_EII.weekly.positivity.df,
            aes(x=week, y=EII.weekly, group=ca, color=ca),
            color='#484848', linetype='dashed', alpha=0.4, linewidth=0.5) +
  geom_line(dat=ESP_ca_EII.weekly.positivity.df %>%
              group_by(week) %>%
              summarise(EII.weekly=sum(EII.weekly)),
            aes(x=week, y=EII.weekly), linetype='solid', color='#484848', alpha=0.9, linewidth=1.2) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=seq(0, ymax, by=5000),
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
ggsave('./figures/ESP_ca_BA.1_EII.positivity.pdf',
       device='pdf', width=7.645, height=4.5)

##################################################################
## process US state-level case/test data
##################################################################

## Alaska
Alaska_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Alaska') %>%
  select(date, tests_viral_positive, tests_viral_total)
Alaska_tests_viral_positive_tests_viral_total.df <- Alaska_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Alaska_tests_viral_positive_tests_viral_total.last_val.df <- Alaska_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Alaska_tests_viral_positive_tests_viral_total.weekly.df <- Alaska_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Alaska_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Alabama
Alabama_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Alabama') %>%
  select(date, tests_viral_positive, tests_viral_total)
Alabama_tests_viral_positive_tests_viral_total.df <- Alabama_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Alabama_tests_viral_positive_tests_viral_total.last_val.df <- Alabama_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Alabama_tests_viral_positive_tests_viral_total.weekly.df <- Alabama_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Alabama_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Arkansas
Arkansas_positive_people_test_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Arkansas') %>%
  select(date, people_viral_positive, tests_viral_total)
Arkansas_positive_people_test_viral_total.df <- Arkansas_positive_people_test_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Arkansas_positive_people_test_viral_total.last_val.df <- Arkansas_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Arkansas_positive_people_test_viral_total.weekly.df <- Arkansas_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Arkansas_positive_people_test_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Arizona
Arizona_positive_people_test_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Arizona') %>%
  select(date, people_viral_positive, tests_viral_total)
Arizona_positive_people_test_viral_total.df <- Arizona_positive_people_test_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Arizona_positive_people_test_viral_total.last_val.df <- Arizona_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Arizona_positive_people_test_viral_total.weekly.df <- Arizona_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Arizona_positive_people_test_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## California
California_positive_people_test_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'California') %>%
  select(date, people_viral_positive, tests_viral_total)
California_positive_people_test_viral_total.df <- California_positive_people_test_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
California_positive_people_test_viral_total.last_val.df <- California_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
California_positive_people_test_viral_total.weekly.df <- California_positive_people_test_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(California_positive_people_test_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Colorado
Colorado_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Colorado') %>%
  select(date, people_viral_positive, encounters_viral_total)
Colorado_positive_people_encounters_viral_total.df <- Colorado_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Colorado_positive_people_encounters_viral_total.last_val.df <- Colorado_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Colorado_tests_viral_positive_tests_viral_total.weekly.df <- Colorado_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Colorado_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Connecticut
Connecticut_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Connecticut') %>%
  select(date, tests_viral_positive, tests_viral_total)
Connecticut_tests_viral_positive_tests_viral_total.df <- Connecticut_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Connecticut_tests_viral_positive_tests_viral_total.last_val.df <- Connecticut_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Connecticut_tests_viral_positive_tests_viral_total.weekly.df <- Connecticut_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Connecticut_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Florida
Florida_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Florida') %>%
  select(date, tests_viral_positive, tests_viral_total)
Florida_tests_viral_positive_tests_viral_total.df <- Florida_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Florida_tests_viral_positive_tests_viral_total.last_val.df <- Florida_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Florida_tests_viral_positive_tests_viral_total.weekly.df <- Florida_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Florida_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Georgia
Georgia_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Georgia') %>%
  select(date, tests_viral_positive, tests_viral_total)
Georgia_tests_viral_positive_tests_viral_total.df <- Georgia_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Georgia_tests_viral_positive_tests_viral_total.last_val.df <- Georgia_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Georgia_tests_viral_positive_tests_viral_total.weekly.df <- Georgia_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Georgia_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Hawaii
Hawaii_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Hawaii') %>%
  select(date, people_viral_positive, encounters_viral_total)
Hawaii_positive_people_encounters_viral_total.df <- Hawaii_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Hawaii_positive_people_encounters_viral_total.last_val.df <- Hawaii_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Hawaii_positive_people_encounters_viral_total.weekly.df <- Hawaii_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Hawaii_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Iowa
Iowa_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Iowa') %>%
  select(date, tests_viral_positive, tests_viral_total)
Iowa_tests_viral_positive_tests_viral_total.df <- Iowa_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Iowa_tests_viral_positive_tests_viral_total.last_val.df <- Iowa_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Iowa_tests_viral_positive_tests_viral_total.weekly.df <- Iowa_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Iowa_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Idaho
Idaho_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Idaho') %>%
  select(date, people_viral_positive, tests_viral_total)
Idaho_people_viral_positive_tests_viral_total.df <- Idaho_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Idaho_people_viral_positive_tests_viral_total.last_val.df <- Idaho_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Idaho_people_viral_positive_tests_viral_total.weekly.df <- Idaho_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Idaho_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Illinois
Illinois_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Illinois') %>%
  select(date, people_viral_positive, tests_viral_total)
Illinois_people_viral_positive_tests_viral_total.df <- Illinois_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Illinois_people_viral_positive_tests_viral_total.last_val.df <- Illinois_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Illinois_people_viral_positive_tests_viral_total.weekly.df <- Illinois_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Illinois_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Indiana
Indiana_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Indiana') %>%
  select(date, tests_viral_positive, tests_viral_total)
Indiana_tests_viral_positive_tests_viral_total.df <- Indiana_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Indiana_tests_viral_positive_tests_viral_total.last_val.df <- Indiana_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Indiana_tests_viral_positive_tests_viral_total.weekly.df <- Indiana_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Indiana_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Kansas
Kansas_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Kansas') %>%
  select(date, people_viral_positive, tests_viral_total)
Kansas_people_viral_positive_tests_viral_total.df <- Kansas_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Kansas_people_viral_positive_tests_viral_total.last_val.df <- Kansas_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Kansas_people_viral_positive_tests_viral_total.weekly.df <- Kansas_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Kansas_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Kentucky
Kentucky_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Kentucky') %>%
  select(date, tests_viral_positive, tests_viral_total)
Kentucky_tests_viral_positive_tests_viral_total.df <- Kentucky_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Kentucky_tests_viral_positive_tests_viral_total.last_val.df <- Kentucky_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Kentucky_tests_viral_positive_tests_viral_total.weekly.df <- Kentucky_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Kentucky_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Louisiana
Louisiana_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Louisiana') %>%
  select(date, people_viral_positive, tests_viral_total)
Louisiana_people_viral_positive_tests_viral_total.df <- Louisiana_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Louisiana_people_viral_positive_tests_viral_total.last_val.df <- Louisiana_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Louisiana_people_viral_positive_tests_viral_total.weekly.df <- Louisiana_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Louisiana_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Massachusetts
Massachusetts_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Massachusetts') %>%
  select(date, tests_viral_positive, tests_viral_total)
Massachusetts_tests_viral_positive_tests_viral_total.df <- Massachusetts_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Massachusetts_tests_viral_positive_tests_viral_total.last_val.df <- Massachusetts_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Massachusetts_tests_viral_positive_tests_viral_total.weekly.df <- Massachusetts_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Massachusetts_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Maryland
Maryland_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Maryland') %>%
  select(date, tests_viral_positive, tests_viral_total)
Maryland_tests_viral_positive_tests_viral_total.df <- Maryland_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Maryland_tests_viral_positive_tests_viral_total.last_val.df <- Maryland_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Maryland_tests_viral_positive_tests_viral_total.weekly.df <- Maryland_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Maryland_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Maine
Maine_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Maine') %>%
  select(date, tests_viral_positive, tests_viral_total)
Maine_tests_viral_positive_tests_viral_total.df <- Maine_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Maine_tests_viral_positive_tests_viral_total.last_val.df <- Maine_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Maine_tests_viral_positive_tests_viral_total.weekly.df <- Maine_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Maine_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Michigan
Michigan_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Michigan') %>%
  select(date, tests_viral_positive, tests_viral_total)
Michigan_tests_viral_positive_tests_viral_total.df <- Michigan_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Michigan_tests_viral_positive_tests_viral_total.last_val.df <- Michigan_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Michigan_tests_viral_positive_tests_viral_total.weekly.df <- Michigan_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Michigan_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Minnesota
Minnesota_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Minnesota') %>%
  select(date, people_viral_positive, encounters_viral_total)
Minnesota_positive_people_encounters_viral_total.df <- Minnesota_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Minnesota_positive_people_encounters_viral_total.last_val.df <- Minnesota_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Minnesota_positive_people_encounters_viral_total.weekly.df <- Minnesota_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Minnesota_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Missouri
Missouri_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Missouri') %>%
  select(date, tests_viral_positive, tests_viral_total)
## anomalous data from 2022-02-10
## interpolate assuming constant from 2022-02-09 up to 2022-02-15
Missouri_tests_viral_positive_tests_viral_total.df[
  Missouri_tests_viral_positive_tests_viral_total.df$date >= as.Date('2022-02-10') &
    Missouri_tests_viral_positive_tests_viral_total.df$date <= as.Date('2022-02-15'),
  ]$tests_viral_positive <- Missouri_tests_viral_positive_tests_viral_total.df[
    Missouri_tests_viral_positive_tests_viral_total.df$date == as.Date('2022-02-09'),]$tests_viral_positive
Missouri_tests_viral_positive_tests_viral_total.df <- Missouri_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Missouri_tests_viral_positive_tests_viral_total.last_val.df <- Missouri_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Missouri_tests_viral_positive_tests_viral_total.weekly.df <- Missouri_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Missouri_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Mississippi
Mississippi_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Mississippi') %>%
  select(date, people_viral_positive, tests_viral_total)
Mississippi_people_viral_positive_tests_viral_total.df <- Mississippi_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Mississippi_people_viral_positive_tests_viral_total.last_val.df <- Mississippi_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Mississippi_people_viral_positive_tests_viral_total.weekly.df <- Mississippi_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Mississippi_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Montana
Montana_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Montana') %>%
  select(date, people_viral_positive, tests_viral_total)
Montana_people_viral_positive_tests_viral_total.df <- Montana_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Montana_people_viral_positive_tests_viral_total.last_val.df <- Montana_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Montana_people_viral_positive_tests_viral_total.weekly.df <- Montana_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Montana_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## North Carolina
NorthCarolina_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'North Carolina') %>%
  select(date, tests_viral_positive, tests_viral_total)
NorthCarolina_tests_viral_positive_tests_viral_total.df <- NorthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
NorthCarolina_tests_viral_positive_tests_viral_total.last_val.df <- NorthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NorthCarolina_tests_viral_positive_tests_viral_total.weekly.df <- NorthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(NorthCarolina_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## North Dakota
NorthDakota_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'North Dakota') %>%
  select(date, people_viral_positive, tests_viral_total)
NorthDakota_people_viral_positive_tests_viral_total.df <- NorthDakota_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
NorthDakota_people_viral_positive_tests_viral_total.last_val.df <- NorthDakota_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NorthDakota_people_viral_positive_tests_viral_total.weekly.df <- NorthDakota_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(NorthDakota_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Nebraska
Nebraska_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Nebraska') %>%
  select(date, people_viral_positive, tests_viral_total)
Nebraska_people_viral_positive_tests_viral_total.df <- Nebraska_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Nebraska_people_viral_positive_tests_viral_total.last_val.df <- Nebraska_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Nebraska_people_viral_positive_tests_viral_total.weekly.df <- Nebraska_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Nebraska_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## New Hampshire
NewHampshire_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'New Hampshire') %>%
  select(date, people_viral_positive, tests_viral_total)
NewHampshire_people_viral_positive_tests_viral_total.df <- NewHampshire_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
NewHampshire_people_viral_positive_tests_viral_total.last_val.df <- NewHampshire_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NewHampshire_people_viral_positive_tests_viral_total.weekly.df <- NewHampshire_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(NewHampshire_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## New Jersey
NewJersey_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'New Jersey') %>%
  select(date, people_viral_positive, tests_viral_total)
NewJersey_people_viral_positive_tests_viral_total.df <- NewJersey_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
NewJersey_people_viral_positive_tests_viral_total.last_val.df <- NewJersey_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NewJersey_people_viral_positive_tests_viral_total.weekly.df <- NewJersey_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(NewJersey_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## New Mexico
NewMexico_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'New Mexico') %>%
  select(date, people_viral_positive, tests_viral_total)
NewMexico_people_viral_positive_tests_viral_total.df <- NewMexico_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
NewMexico_people_viral_positive_tests_viral_total.last_val.df <- NewMexico_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NewMexico_people_viral_positive_tests_viral_total.weekly.df <- NewMexico_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(NewMexico_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Nevada
Nevada_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Nevada') %>%
  select(date, people_viral_positive, encounters_viral_total)
Nevada_positive_people_encounters_viral_total.df <- Nevada_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Nevada_positive_people_encounters_viral_total.last_val.df <- Nevada_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Nevada_positive_people_encounters_viral_total.weekly.df <- Nevada_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Nevada_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## New York
NewYork_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'New York') %>%
  select(date, people_viral_positive, encounters_viral_total)
NewYork_positive_people_encounters_viral_total.df <- NewYork_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
NewYork_positive_people_encounters_viral_total.last_val.df <- NewYork_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
NewYork_positive_people_encounters_viral_total.weekly.df <- NewYork_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(NewYork_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Ohio
Ohio_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Ohio') %>%
  select(date, tests_viral_positive, tests_viral_total)
Ohio_tests_viral_positive_tests_viral_total.df <- Ohio_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Ohio_tests_viral_positive_tests_viral_total.last_val.df <- Ohio_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Ohio_tests_viral_positive_tests_viral_total.weekly.df <- Ohio_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Ohio_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Oklahoma
Oklahoma_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Oklahoma') %>%
  select(date, tests_viral_positive, tests_viral_total)
Oklahoma_tests_viral_positive_tests_viral_total.df <- Oklahoma_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Oklahoma_tests_viral_positive_tests_viral_total.last_val.df <- Oklahoma_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Oklahoma_tests_viral_positive_tests_viral_total.weekly.df <- Oklahoma_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Oklahoma_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Oregon
Oregon_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Oregon') %>%
  select(date, tests_viral_positive, tests_viral_total)
Oregon_tests_viral_positive_tests_viral_total.df <- Oregon_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Oregon_tests_viral_positive_tests_viral_total.last_val.df <- Oregon_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Oregon_tests_viral_positive_tests_viral_total.weekly.df <- Oregon_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Oregon_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Pennsylvania
Pennsylvania_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Pennsylvania') %>%
  select(date, people_viral_positive, encounters_viral_total)
Pennsylvania_positive_people_encounters_viral_total.df <- Pennsylvania_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Pennsylvania_positive_people_encounters_viral_total.last_val.df <- Pennsylvania_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Pennsylvania_positive_people_encounters_viral_total.weekly.df <- Pennsylvania_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Pennsylvania_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Rhode Island
RhodeIsland_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Rhode Island') %>%
  select(date, tests_viral_positive, tests_viral_total)
RhodeIsland_tests_viral_positive_tests_viral_total.df <- RhodeIsland_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
RhodeIsland_tests_viral_positive_tests_viral_total.last_val.df <- RhodeIsland_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
RhodeIsland_tests_viral_positive_tests_viral_total.weekly.df <- RhodeIsland_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(RhodeIsland_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## South Carolina
SouthCarolina_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'South Carolina') %>%
  select(date, tests_viral_positive, tests_viral_total)
SouthCarolina_tests_viral_positive_tests_viral_total.df <- SouthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
SouthCarolina_tests_viral_positive_tests_viral_total.last_val.df <- SouthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
SouthCarolina_tests_viral_positive_tests_viral_total.weekly.df <- SouthCarolina_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(SouthCarolina_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## South Dakota
SouthDakota_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'South Dakota') %>%
  select(date, tests_viral_positive, tests_viral_total)
SouthDakota_tests_viral_positive_tests_viral_total.df <- SouthDakota_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
SouthDakota_tests_viral_positive_tests_viral_total.last_val.df <- SouthDakota_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
SouthDakota_tests_viral_positive_tests_viral_total.weekly.df <- SouthDakota_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(SouthDakota_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Tennessee
Tennessee_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Tennessee') %>%
  select(date, tests_viral_positive, tests_viral_total)
Tennessee_tests_viral_positive_tests_viral_total.df <- Tennessee_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Tennessee_tests_viral_positive_tests_viral_total.last_val.df <- Tennessee_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Tennessee_tests_viral_positive_tests_viral_total.weekly.df <- Tennessee_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Tennessee_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Texas
Texas_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Texas') %>%
  select(date, people_viral_positive, tests_viral_total)
Texas_people_viral_positive_tests_viral_total.df <- Texas_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Texas_people_viral_positive_tests_viral_total.last_val.df <- Texas_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Texas_people_viral_positive_tests_viral_total.weekly.df <- Texas_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Texas_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Utah
Utah_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Utah') %>%
  select(date, tests_viral_positive, tests_viral_total)
Utah_tests_viral_positive_tests_viral_total.df <- Utah_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Utah_tests_viral_positive_tests_viral_total.last_val.df <- Utah_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Utah_tests_viral_positive_tests_viral_total.weekly.df <- Utah_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Utah_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## Virginia
Virginia_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Virginia') %>%
  select(date, people_viral_positive, encounters_viral_total)
Virginia_positive_people_encounters_viral_total.df <- Virginia_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Virginia_positive_people_encounters_viral_total.last_val.df <- Virginia_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Virginia_positive_people_encounters_viral_total.weekly.df <- Virginia_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Virginia_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## Vermont
Vermont_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Vermont') %>%
  select(date, people_viral_positive, tests_viral_total)
Vermont_people_viral_positive_tests_viral_total.df <- Vermont_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Vermont_people_viral_positive_tests_viral_total.last_val.df <- Vermont_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Vermont_people_viral_positive_tests_viral_total.weekly.df <- Vermont_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Vermont_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Washington
Washington_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Washington') %>%
  select(date, people_viral_positive, encounters_viral_total)
Washington_positive_people_encounters_viral_total.df <- Washington_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
## issue with WA, problem being that encounters_viral_total/tests_combined_total (denominator)
## remained static for most periods

## Wisconsin
Wisconsin_positive_people_encounters_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Wisconsin') %>%
  select(date, people_viral_positive, encounters_viral_total)
Wisconsin_positive_people_encounters_viral_total.df <- Wisconsin_positive_people_encounters_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    encounters_viral_total_rm=rollmean(encounters_viral_total, 7, fill=NA)
  )
Wisconsin_positive_people_encounters_viral_total.last_val.df <- Wisconsin_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    encounters_viral_total_rm.last=last(encounters_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Wisconsin_positive_people_encounters_viral_total.weekly.df <- Wisconsin_positive_people_encounters_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    encounters_viral_total_weekly=last(encounters_viral_total_rm)
  ) %>%
  left_join(Wisconsin_positive_people_encounters_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    encounters_viral_total_weekly=encounters_viral_total_weekly - encounters_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/encounters_viral_total_weekly
  )

## West Virginia
WestVirginia_people_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'West Virginia') %>%
  select(date, people_viral_positive, tests_viral_total)
WestVirginia_people_viral_positive_tests_viral_total.df <- WestVirginia_people_viral_positive_tests_viral_total.df %>%
  mutate(
    people_viral_positive_rm=rollmean(people_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
WestVirginia_people_viral_positive_tests_viral_total.last_val.df <- WestVirginia_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_rm.last=last(people_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
WestVirginia_people_viral_positive_tests_viral_total.weekly.df <- WestVirginia_people_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    people_viral_positive_weekly=last(people_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(WestVirginia_people_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    people_viral_positive_weekly=people_viral_positive_weekly - people_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=people_viral_positive_weekly/tests_viral_total_weekly
  )

## Wyoming
Wyoming_tests_viral_positive_tests_viral_total.df <- USA_state_test_dat.df %>%
  filter(state == 'Wyoming') %>%
  select(date, tests_viral_positive, tests_viral_total)
Wyoming_tests_viral_positive_tests_viral_total.df <- Wyoming_tests_viral_positive_tests_viral_total.df %>%
  mutate(
    tests_viral_positive_rm=rollmean(tests_viral_positive, 7, fill=NA),
    tests_viral_total_rm=rollmean(tests_viral_total, 7, fill=NA)
  )
Wyoming_tests_viral_positive_tests_viral_total.last_val.df <- Wyoming_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_rm.last=last(tests_viral_positive_rm),
    tests_viral_total_rm.last=last(tests_viral_total_rm)
  ) %>%
  mutate(week=week+7)
Wyoming_tests_viral_positive_tests_viral_total.weekly.df <- Wyoming_tests_viral_positive_tests_viral_total.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(week) %>%
  summarise(
    tests_viral_positive_weekly=last(tests_viral_positive_rm),
    tests_viral_total_weekly=last(tests_viral_total_rm)
  ) %>%
  left_join(Wyoming_tests_viral_positive_tests_viral_total.last_val.df, by='week') %>%
  mutate(
    tests_viral_positive_weekly=tests_viral_positive_weekly - tests_viral_positive_rm.last,
    tests_viral_total_weekly=tests_viral_total_weekly - tests_viral_total_rm.last,
    positive_rate_weekly=tests_viral_positive_weekly/tests_viral_total_weekly
  )

## combine all state-level positivity rates
USA_combined_state.positivity.weekly.df <- Alaska_tests_viral_positive_tests_viral_total.weekly.df %>%
  mutate(state='Alaska') %>%
  select(state, week, positive_rate_weekly) %>%
  rbind(
    Alabama_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Alabama') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Arkansas_positive_people_test_viral_total.weekly.df %>%
      mutate(state='Arkansas') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Arizona_positive_people_test_viral_total.weekly.df %>%
      mutate(state='Arizona') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    California_positive_people_test_viral_total.weekly.df %>%
      mutate(state='California') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Colorado_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Colorado') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Connecticut_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Connecticut') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Florida_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Florida') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Georgia_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Georgia') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Hawaii_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Hawaii') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Iowa_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Iowa') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Idaho_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Idaho') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Illinois_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Illinois') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Indiana_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Indiana') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Kansas_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Kansas') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Kentucky_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Kentucky') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Louisiana_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Louisiana') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Massachusetts_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Massachusetts') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Maryland_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Maryland') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Maine_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Maine') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Michigan_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Michigan') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Minnesota_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Minnesota') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Missouri_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Missouri') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Mississippi_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Mississippi') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Montana_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Montana') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NorthCarolina_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='North Carolina') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NorthDakota_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='North Dakota') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Nebraska_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Nebraska') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NewHampshire_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='New Hampshire') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NewJersey_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='New Jersey') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NewMexico_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='New Mexico') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Nevada_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Nevada') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    NewYork_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='New York') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Ohio_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Ohio') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Oklahoma_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Oklahoma') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Oregon_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Oregon') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Pennsylvania_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Pennsylvania') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    RhodeIsland_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Rhode Island') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    SouthCarolina_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='South Carolina') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    SouthDakota_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='South Dakota') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Tennessee_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Tennessee') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Texas_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Texas') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Utah_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Utah') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Virginia_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Virginia') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Vermont_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Vermont') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Wisconsin_positive_people_encounters_viral_total.weekly.df %>%
      mutate(state='Wisconsin') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    WestVirginia_people_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='West Virginia') %>%
      select(state, week, positive_rate_weekly)
  ) %>%
  rbind(
    Wyoming_tests_viral_positive_tests_viral_total.weekly.df %>%
      mutate(state='Wyoming') %>%
      select(state, week, positive_rate_weekly)
  )

## calculate population-weighted average of positivity rate
USA_combined_state.positivity.weekly.pop_weighted.df <- USA_combined_state.positivity.weekly.df %>%
  left_join(USA_pop.df, by='state') %>%
  filter(week >= as.Date('2021-10-10') & week <= as.Date('2022-02-06')) %>%
  group_by(week) %>%
  summarise(positive_rate_weekly=sum(positive_rate_weekly*pop_est)/sum(pop_est))

## use USA_combined_state.positivity.weekly.pop_weighted.df to impute Washington
USA_combined_state.positivity.weekly.washington_imputed.df <- USA_combined_state.positivity.weekly.df %>%
  rbind(
    USA_combined_state.positivity.weekly.pop_weighted.df %>%
      mutate(state='Washington')
  )
USA_combined_state.positivity.weekly.NewJersey_washington_imputed.df <- USA_combined_state.positivity.weekly.df %>%
  filter(state != 'New Jersey') %>%
  rbind(
    USA_combined_state.positivity.weekly.pop_weighted.df %>%
      mutate(state='Washington')
  ) %>%
  rbind(
    USA_combined_state.positivity.weekly.pop_weighted.df %>%
      mutate(state='New Jersey')
  )

## calculate weekly EIIs for USA at state-level from positivity rate
USA_state_EII.weekly.positivity.df <- world_flight_dat.UK_split.Eng.agg.weekly.df %>%
  filter(originCountry == 'United States' & week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
  select(-originCountry) %>%
  rename(state=originLocation) %>%
  left_join(USA_combined_state.positivity.weekly.washington_imputed.df, by=c('state', 'week')) %>%
  left_join(USA_gisaid_dat.BA.1.weekly.df %>%
              filter(week >= as.Date('2021-10-24') & week <= as.Date('2022-02-13')) %>%
              select(location, week, w2_preceding.BA.1.p) %>%
              rename(state=location), by=c('state', 'week'))
USA_state_EII.weekly.positivity.df <- USA_state_EII.weekly.positivity.df %>%
  mutate(EII.weekly=totalPassengerVolume.weekly*w2_preceding.BA.1.p*positive_rate_weekly)

## make plot
ymin <- 0
ymax <- 14000
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
  geom_line(dat=USA_state_EII.weekly.positivity.df,
            aes(x=week, y=EII.weekly, group=state, color=state),
            color='#484848', linetype='dashed', alpha=0.4, linewidth=0.5) +
  geom_line(dat=USA_state_EII.weekly.positivity.df %>%
              group_by(week) %>%
              summarise(EII.weekly=sum(EII.weekly)),
            aes(x=week, y=EII.weekly), linetype='solid', color='#484848', alpha=0.9, linewidth=1.2) +
  scale_x_date(date_breaks='14 days', date_labels='%d %b',
               limits=c(as.Date('2021-11-07'), as.Date('2022-02-06')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=seq(0, ymax, by=2000),
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
ggsave('./figures/USA_state_BA.1_EII.positivity.pdf',
       device='pdf', width=7.645, height=4.5)

##################################################################
## output EIIs as csv
##################################################################

## ESP AC-level weekly EIIs using case per capita
write.csv(ESP_ca_EII.weekly.case_per_capita.df %>%
            group_by(week) %>%
            summarise(EII.weekly=sum(EII.weekly)),
          file='./ESP_AC_weekly_EIIs_case_per_capita.csv', row.names=FALSE, quote=FALSE)

## ESP AC-level weekly EIIs using positivity rates
write.csv(ESP_ca_EII.weekly.positivity.df %>%
            group_by(week) %>%
            summarise(EII.weekly=sum(EII.weekly)),
          file='./ESP_AC_weekly_EIIs_positivity.csv', row.names=FALSE, quote=FALSE)

## USA state-level weekly EIIs using case per capita
write.csv(USA_state_EII.weekly.case_per_capita.df %>%
            group_by(week) %>%
            summarise(EII.weekly=sum(EII.weekly)),
          file='./USA_state_weekly_EIIs_case_per_capita.csv', row.names=FALSE, quote=FALSE)

## USA state-level weekly EIIs using positivity rates
write.csv(USA_state_EII.weekly.positivity.df %>%
            group_by(week) %>%
            summarise(EII.weekly=sum(EII.weekly)),
          file='./USA_state_weekly_EIIs_positivity.csv', row.names=FALSE, quote=FALSE)


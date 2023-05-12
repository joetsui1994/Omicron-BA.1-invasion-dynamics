library(dplyr)
library(readr)
library(lubridate)
library(stats)
library(ggplot2)
library(zoo)

## read in file mapping English sequences to corresponding LTLAs for each partition
BA1_ltla.dat <- read.csv('./BA.1/tip_ltla.tsv', sep='\t')
BA11_ltla.dat <- read.csv('./BA.1.1/tip_ltla.tsv', sep='\t')
BA115_ltla.dat <- read.csv('./BA.1.15/tip_ltla.tsv', sep='\t')
BA117_ltla.dat <- read.csv('./BA.1.17/tip_ltla.tsv', sep='\t')

## merge all
all_ltla.df <- rbind(BA1_ltla.dat,
                     BA11_ltla.dat,
                     BA115_ltla.dat,
                     BA117_ltla.dat)

## extract sampling period from only these English sequences
all_ltla.df$sample.date <- as.Date(unlist(lapply(strsplit(all_ltla.df$name, '\\|'), function(x) x[length(x)])))
earliest_date <- min(all_ltla.df$sample.date)
latest_date <- max(all_ltla.df$sample.date)

## read in file mapping LTLAs to UTLAs
ltla_utla.df <- read.csv('./LTLA-UTLA_lookup_corrected.csv')
ltla_utla.df <- ltla_utla.df %>%
  rename(
    ltla=LTLA21CD,
    utla=UTLA21CD
  ) %>%
  select(ltla, utla)

## LTLA consistency check
setdiff(unique(all_ltla.df$ltla), unique(ltla_utla.df$ltla))
## note that there were no sequences sampled in Isles of Scilly

## map LTLA to UTLA
all_ltla.df <- all_ltla.df %>%
  left_join(ltla_utla.df, by='ltla')

## aggregate English sequences at weekly level (and UTLA)
all_utla.weekly.df <- all_ltla.df %>%
  mutate(week=floor_date(sample.date, unit='week')) %>%
  group_by(utla, week) %>%
  summarise(count=n())

## read in case data
england_cases_dat.df <- read.csv('./data/master_final_omicron_daily_LTLA_level_2021_09_01-2022_03_01_minimal.csv')
england_cases_dat.df <- england_cases_dat.df %>%
  rename(total_omicron_cases=omicron_cases_confirmed_by_SGTF)
england_cases_dat.df$samples = england_cases_dat.df$total_omicron_cases + england_cases_dat.df$Non_omicron
england_cases_dat.df <- england_cases_dat.df %>%
  filter(LTLA_code != 'E06000053') %>% ## ignore Isles of Scilly
  group_by(LTLA_code, specimen_date) %>%
  summarise(
    Total_number_of_cases = sum(Total_number_of_cases),
    total_omicron_cases = sum(total_omicron_cases),
    Non_omicron = sum(Non_omicron),
    samples = sum(samples),
    est_omicron_cases = Total_number_of_cases*(total_omicron_cases/samples)
  ) %>%
  select(specimen_date, Total_number_of_cases, est_omicron_cases) %>%
  rename(
    ltla=LTLA_code,
    date=specimen_date,
    total=Total_number_of_cases
    ) %>%
  filter(date >= earliest_date & date <= latest_date)
england_cases_dat.df[!complete.cases(england_cases_dat.df),]$est_omicron_cases <- 0
england_cases_dat.df$date <- as.Date(england_cases_dat.df$date)

## map LTLA to UTLA
england_cases_dat.df <- england_cases_dat.df %>%
  left_join(ltla_utla.df, by='ltla')

## aggregate estimated Omicron BA.1 cases at weekly level
england_cases_dat.weekly.df <- england_cases_dat.df %>%
  mutate(week=floor_date(date, unit='week')) %>%
  group_by(utla, week) %>%
  summarise(
    omicron_count=sum(est_omicron_cases),
    case_count=sum(total)
    )

## UTLA/week consistency check
setdiff(unique(all_utla.weekly.df$utla), unique(england_cases_dat.weekly.df$utla))
setdiff(unique(all_utla.weekly.df$week), unique(england_cases_dat.weekly.df$week))

## merge dataframes for LTLA and UTLA
combined.df <- all_utla.weekly.df %>%
  left_join(england_cases_dat.weekly.df, by=c('utla', 'week'))
# 
# ltla_seqs_omicron_cases_weekly_combined_rmNA.df <- ltla_seqs_omicron_cases_weekly_combined.df %>%
#   filter(count > 0 & omicron_count > 0)
# utla_seqs_omicron_cases_weekly_combined.df <- merge(utla_weekly_seqs.dat,
#                                                     utla_weekly_omicron_cases.dat, by=c('utla', 'last_sunday'))
# utla_seqs_omicron_cases_weekly_combined_rmNA.df <- utla_seqs_omicron_cases_weekly_combined.df %>%
#   filter(count > 0 & omicron_count > 0)

## make plots
## specify colour scheme
colors <- c(
  '#313030',
  '#0C4C5F',
  '#1B7883',
  '#947A47',
  '#F8C55D',
  '#E6824C',
  '#D43F3A',
  '#A24243',
  '#6F444B',
  '#3e6949',
  '#755d91'
)
## rank weeks from earliest to latest
combined.filtered.df <- combined.df %>%
  filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')) %>%
  arrange(week) %>%
  mutate(week_rank=as.character(dense_rank(week)))

## make plot
ggplot() +
  geom_point(data=combined.filtered.df %>%
               filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')),
             aes(x=omicron_count, y=count, color=week_rank), size=1.5, alpha=0.9) +
  scale_color_manual(values=colors) +
  geom_smooth(data=combined.filtered.df %>%
                filter(week >= as.Date('2021-11-28') & week <= as.Date('2022-01-23')),
              aes(x=omicron_count, y=count), method='lm', formula=y~x,
              size=1, color='black') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  coord_cartesian(clip = "off") +
  labs(x='Estimated weekly number of new Omicron BA.1 cases per UTLA (log10)',
       y='Weekly number of sequences per UTLA (log10)') +
  theme(
    # legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_line(color='light grey', size=0.2,),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(vjust=2, size=15),
    axis.title.x = element_text(vjust=0, size=15),
    axis.text.x = element_text(face=1, size=13, color='black'),
    axis.text.y = element_text(face=1, size=13, color='black')
  )
## calculate Pearson's r
cor.test(combined.df$count, combined.df$omicron_count, method = "pearson")

## output plot to file
ggsave('../../genome_representativeness/UTLA_sequence_BA.1_case_distribution.pdf',
       device='pdf', width=12, height=7.5)


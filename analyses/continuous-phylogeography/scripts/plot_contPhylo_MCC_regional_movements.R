library(dplyr)
library(readr)
library(lubridate)
library(HDInterval)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(ggpattern)

## read in MCC geo-summaries
## BA.1.15_DTA_700_n713
BA115_DTA_700_n713_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1.15_DTA_700_n713_mcc_node_times.tsv',
                                                delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA115_DTA_700_n713_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1.15_DTA_700_n713_mcc_geo_summary.tsv',
                                         delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA115_DTA_700_n713_mcc_geo <- merge(BA115_DTA_700_n713_mcc_geo, BA115_DTA_700_n713_mcc_node_times,
                                    by=c('head_node', 'tail_node'))
BA115_DTA_700_n713_mcc_geo$tl_lab <- 'BA115_DTA_700_n713'

## BA.1.15_DTA_102_n2967
BA115_DTA_102_n2967_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1.15_DTA_102_n2967_mcc_node_times.tsv',
                                                 delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA115_DTA_102_n2967_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1.15_DTA_102_n2967_mcc_geo_summary.tsv',
                                          delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA115_DTA_102_n2967_mcc_geo <- merge(BA115_DTA_102_n2967_mcc_geo, BA115_DTA_102_n2967_mcc_node_times,
                                     by=c('head_node', 'tail_node'))
BA115_DTA_102_n2967_mcc_geo$tl_lab <- 'BA115_DTA_102_n2967'

## BA.1_DTA_1800_n944
BA1_DTA_1800_n944_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1_DTA_1800_n944_mcc_node_times.tsv',
                                               delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1800_n944_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1_DTA_1800_n944_mcc_geo_summary.tsv',
                                        delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1800_n944_mcc_geo <- merge(BA1_DTA_1800_n944_mcc_geo, BA1_DTA_1800_n944_mcc_node_times,
                                   by=c('head_node', 'tail_node'))
BA1_DTA_1800_n944_mcc_geo$tl_lab <- 'BA1_DTA_1800_n944'

## BA.1_DTA_1944_n1406
BA1_DTA_1944_n1406_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1_DTA_1944_n1406_mcc_node_times.tsv',
                                                delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1944_n1406_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1_DTA_1944_n1406_mcc_geo_summary.tsv',
                                         delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1944_n1406_mcc_geo <- merge(BA1_DTA_1944_n1406_mcc_geo, BA1_DTA_1944_n1406_mcc_node_times,
                                    by=c('head_node', 'tail_node'))
BA1_DTA_1944_n1406_mcc_geo$tl_lab <- 'BA1_DTA_1944_n1406'

## BA.1_DTA_1207_n9727
BA1_DTA_1207_n9727_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1_DTA_1207_n9727_mcc_node_times.tsv',
                                                delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1207_n9727_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1_DTA_1207_n9727_mcc_geo_summary.tsv',
                                         delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA1_DTA_1207_n9727_mcc_geo <- merge(BA1_DTA_1207_n9727_mcc_geo, BA1_DTA_1207_n9727_mcc_node_times,
                                    by=c('head_node', 'tail_node'))
BA1_DTA_1207_n9727_mcc_geo$tl_lab <- 'BA1_DTA_1207_n9727'

## BA.1.1_DTA_1467_n722
BA11_DTA_1467_n722_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1.1_DTA_1467_n722_mcc_node_times.tsv',
                                                delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA11_DTA_1467_n722_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1.1_DTA_1467_n722_mcc_geo_summary.tsv',
                                         delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA11_DTA_1467_n722_mcc_geo <- merge(BA11_DTA_1467_n722_mcc_geo, BA11_DTA_1467_n722_mcc_node_times,
                                    by=c('head_node', 'tail_node'))
BA11_DTA_1467_n722_mcc_geo$tl_lab <- 'BA11_DTA_1467_n722'

## BA.1.1_DTA_1254_n2249
BA11_DTA_1254_n2249_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1.1_DTA_1254_n2249_mcc_node_times.tsv',
                                                 delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA11_DTA_1254_n2249_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1.1_DTA_1254_n2249_mcc_geo_summary.tsv',
                                          delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA11_DTA_1254_n2249_mcc_geo <- merge(BA11_DTA_1254_n2249_mcc_geo, BA11_DTA_1254_n2249_mcc_node_times,
                                     by=c('head_node', 'tail_node'))
BA11_DTA_1254_n2249_mcc_geo$tl_lab <- 'BA11_DTA_1254_n2249'

## BA.1.17_DTA_175_n11351
BA117_DTA_175_n11351_mcc_node_times <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_node_times/BA.1.17_DTA_175_n11351_mcc_node_times.tsv',
                                                  delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA117_DTA_175_n11351_mcc_geo <- read_delim('./contPhylo_process_data/tls_processed_metadata/mcc_geo/BA.1.17_DTA_175_n11351_mcc_geo_summary.tsv',
                                           delim='\t', escape_double=FALSE, trim_ws=TRUE)
BA117_DTA_175_n11351_mcc_geo <- merge(BA117_DTA_175_n11351_mcc_geo, BA117_DTA_175_n11351_mcc_node_times,
                                      by=c('head_node', 'tail_node'))
BA117_DTA_175_n11351_mcc_geo$tl_lab <- 'BA117_DTA_175_n11351'

## small lineages
small_lineages_mcc_node_times_files <- list.files(path='./contPhylo_process_data/tls_processed_metadata/mcc_node_times/small_lineages_nov',
                                                  pattern="*.tsv", full.names=TRUE, recursive=FALSE)
small_lineages_mcc_geo_files <- list.files(path='./contPhylo_process_data/tls_processed_metadata/mcc_geo/small_lineages_nov',
                                           pattern="*.tsv", full.names=TRUE, recursive=FALSE)
small_lineages_mcc_node_times <- data.frame()
for (file in small_lineages_mcc_node_times_files) {
  filename_split <- strsplit(file, '/')[[1]]
  tl_name <- strsplit(filename_split[length(filename_split)], '_mcc_node_times.tsv')[[1]][1]
  tmp <- read_delim(file, delim='\t', escape_double=FALSE, trim_ws=TRUE)
  tmp$tl_lab <- tl_name
  small_lineages_mcc_node_times <- rbind(small_lineages_mcc_node_times, tmp)
}
small_lineages_mcc_geo <- data.frame()
for (file in small_lineages_mcc_geo_files) {
  filename_split <- strsplit(file, '/')[[1]]
  tl_name <- strsplit(filename_split[length(filename_split)], '_geo_summary.tsv')[[1]][1]
  tmp <- read_delim(file, delim='\t', escape_double=FALSE, trim_ws=TRUE)
  tmp$tl_lab <- tl_name
  small_lineages_mcc_geo <- rbind(small_lineages_mcc_geo, tmp)
}
small_lineages_mcc_geo <- merge(small_lineages_mcc_geo, small_lineages_mcc_node_times,
                                by=c('head_node', 'tail_node', 'tl_lab'))

## create combined dataframe
mcc_geo_combined <- rbind(
  BA115_DTA_700_n713_mcc_geo,
  BA115_DTA_102_n2967_mcc_geo,
  BA1_DTA_1800_n944_mcc_geo,
  BA1_DTA_1944_n1406_mcc_geo,
  BA1_DTA_1207_n9727_mcc_geo,
  BA11_DTA_1467_n722_mcc_geo,
  BA11_DTA_1254_n2249_mcc_geo,
  BA117_DTA_175_n11351_mcc_geo,
  small_lineages_mcc_geo
)

## geo_distance bins
dist_bins <- c(0, 50, 300, 9999)
bin_dist_func <- function(dist, bins) {
  for (i in 1:(length(bins)-1)) {
    if (dist >= bins[i] & dist < bins[i+1]) {
      return(c(bins[i], bins[i+1]))
    }
  }
  return(c(bins[length(bins)-1], bins[length(bins)]))
}
mcc_geo_combined_geo_distance_bins <- sapply(mcc_geo_combined$geo_distance, function(x) bin_dist_func(x, dist_bins)[1])
mcc_geo_combined$geo_distance_bin <- mcc_geo_combined_geo_distance_bins
mcc_geo_combined <- mcc_geo_combined %>%
  mutate(
    mid_est = (tail_dec_date + head_dec_date)/2,
    mid_date = as.Date(date_decimal(mid_est))
  )

## label within-region movements
mcc_geo_combined <- mcc_geo_combined %>%
  mutate(
    within_region = head_region == tail_region,
    london_relevant = head_region == 'E12000007' | tail_region == 'E12000007'
  )

## colour-scheme
fill_colors <- c(
  '#3E4855', '#1B7883', '#0C4C5F', '#6F444B', '#A24243', '#D43F3A', '#E6824C', '#F8C55D', '#947A47'
)

## look at cross-region viral lineage movemnts
vMovements_0_50km_y_max <- 3500
start_date <- as.Date('2021-11-07')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
vMovements_0_50km_stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = vMovements_0_50km_y_max
)
vMovements_0_50km_stripes[1, 'xmin'] <- as.Date('2021-11-10')
vMovements_0_50km_stripes[nrow(vMovements_0_50km_stripes), 'xmax'] <- as.Date('2022-02-03')
vMovements_0_50km_plot <- ggplot() +
  geom_rect(data=vMovements_0_50km_stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=vMovements_0_50km_y_max),
            fill='gray80', alpha=0.35) +
  geom_bar(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 0,],
           aes(x=mid_date, fill=london_relevant), size=0.2, width=1, position='stack',
           alpha=0.9, color='#484848') +
  scale_fill_manual(values=c('#403e3e', '#c26a1d')) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, vMovements_0_50km_y_max), expand=c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(0, 30, 0, -3.4)
  )

vMovements_50_300km_y_max <- 700
start_date <- as.Date('2021-11-07')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
vMovements_50_300km_stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = vMovements_50_300km_y_max
)
vMovements_50_300km_stripes[1, 'xmin'] <- as.Date('2021-11-10')
vMovements_50_300km_stripes[nrow(vMovements_50_300km_stripes), 'xmax'] <- as.Date('2022-02-03')
vMovements_50_300km_plot <- ggplot() +
  geom_rect(data=vMovements_50_300km_stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=vMovements_50_300km_y_max),
            fill='gray80', alpha=0.35) +
  geom_bar(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 50,],
           aes(x=mid_date, fill=london_relevant), size=0.2, width=1, position='stack',
           alpha=0.9, color='#484848') +
  scale_fill_manual(values=c('#403e3e', '#c26a1d')) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, vMovements_50_300km_y_max), expand=c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(0, 30, 0, 3.3)
  )

vMovements_300km_y_max <- 38
start_date <- as.Date('2021-11-07')
end_date <- as.Date('2022-02-08')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
vMovements_300km_stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = 0,
  ymax = vMovements_300km_y_max
)
vMovements_300km_stripes[1, 'xmin'] <- as.Date('2021-11-10')
vMovements_300km_stripes[nrow(vMovements_300km_stripes), 'xmax'] <- as.Date('2022-02-03')
vMovements_300km_plot <- ggplot() +
  geom_rect(data=vMovements_300km_stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=vMovements_300km_y_max),
            fill='gray80', alpha=0.35) +
  geom_bar(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 300,],
           aes(x=mid_date, fill=london_relevant), size=0.2, width=1, position='stack',
           alpha=0.9, color='#484848') +
  scale_fill_manual(values=c('#403e3e', '#c26a1d')) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, vMovements_300km_y_max), expand=c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(0, 30, 0, 10)
  )

## combine plots
combined_freq_plots <- arrangeGrob(
  vMovements_0_50km_plot,
  vMovements_50_300km_plot,
  vMovements_300km_plot,
  nrow = 3,
  top = ''
)

ggsave('./figures/vMovements_combined_freq_plot_v2.pdf',
       combined_freq_plots, width=7, height=7)

## plot proportion by region
f <- function(..count.., ..x..) tapply(..count.., factor(..x..), sum)[factor(..x..)]

regional_vMovements_outflow_0_50km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 0 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=head_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 0 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../400), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*400, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 30, 0, 0)
  )

regional_vMovements_inflow_0_50km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 0 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=tail_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 0 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../400), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*400, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 30, 0, 0)
  )

regional_vMovements_outflow_50_300km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 50 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=head_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 50 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../600), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*600, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 30, 0, 0)
  )

regional_vMovements_inflow_50_300km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 50 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=tail_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 50 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../600), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*600, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 30, 0, 0)
  )

regional_vMovements_outflow_300km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 300 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=head_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 300 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../50), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*50, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 36.7, 0, 0)
  )

regional_vMovements_inflow_300km_propr <- ggplot() +
  geom_rect_pattern(aes(xmin=as.Date('2021-11-10'), xmax=as.Date('2022-02-03'), ymin=0, ymax=1),
                    pattern='stripe', pattern_density=0.01, pattern_size=0.05,
                    pattern_spacing=0.03, pattern_angle=45,
                    fill='gray80', alpha=0.25) +
  geom_histogram(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 300 &
                                         mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
                 aes(x=mid_date, fill=tail_region, alpha=f(..count.., ..x..) > 9),
                 size=0.1, binwidth=2, position='fill', color='#484848') +
  geom_line(data=mcc_geo_combined[mcc_geo_combined$geo_distance_bin == 300 &
                                    mcc_geo_combined$head_region != mcc_geo_combined$tail_region,],
            aes(x=mid_date, y=..count../50), stat='bin', binwidth=1, color='black', size=0.7, alpha=0.7) +
  scale_alpha_manual(values=c(0.4, 1)) +
  scale_fill_manual(values=fill_colors) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-10'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0),
                     sec.axis=sec_axis(trans=~.*50, name='')) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(vjust=2),
    axis.title.y.right = element_text(vjust=2),
    axis.text.x = element_text(face=1, size=11.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=11.5, color='black'),
    plot.margin = margin(10, 36.7, 0, 0)
  )

## create combined plot
combined_outflow_propr_plots <- arrangeGrob(
  regional_vMovements_outflow_0_50km_propr,
  regional_vMovements_outflow_50_300km_propr,
  regional_vMovements_outflow_300km_propr,
  nrow = 3,
  top = ''
)

## output plot to file
ggsave('./figures/regional_vMovements_outflow_combined_propr_plot.pdf',
       combined_outflow_propr_plots, width=8, height=7)

## create combined plot
combined_inflow_propr_plots <- arrangeGrob(
  regional_vMovements_inflow_0_50km_propr,
  regional_vMovements_inflow_50_300km_propr,
  regional_vMovements_inflow_300km_propr,
  nrow = 3,
  top = ''
)

## output plot to file
ggsave('./figures/regional_vMovements_inflow_combined_propr_plot.pdf',
       combined_inflow_propr_plots, width=8, height=7)




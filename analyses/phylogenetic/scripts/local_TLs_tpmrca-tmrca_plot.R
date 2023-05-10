library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)

## read in importation events identified from the MCC tree for each subtype
## BA.1.15
BA115_mcc_tls.df <- read.csv('./mcc_tls/BA.1.15_mcc_tls.tsv', sep='\t')
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
BA117_mcc_tls.df <- read.csv('./mcc_tls/BA.1.17_mcc_tls.tsv', sep='\t')
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
BA11_mcc_tls.df <- read.csv('./mcc_tls/BA.1.1_mcc_tls.tsv', sep='\t')
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
BA1_mcc_tls.df <- read.csv('./mcc_tls/BA.1_mcc_tls.tsv', sep='\t')
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
all_mcc_tls.df <- all_mcc_tls.df %>%
  arrange(import_est) %>%
  mutate(
    index=row_number()-1,
    ntaxa_grp = ifelse(ntaxa == 1, 'singleton',
                       ifelse(ntaxa < 5, 'small',
                              ifelse(ntaxa >= 5 & ntaxa <= 50, 'medium', 'large')))
  )

## create alternatingly shaded background (by every two weeks)
start_date <- as.Date('2021-10-31')
end_date <- as.Date('2022-01-31')
all_dates <- seq(start_date, end_date, by = 'day')
selected_dates <- all_dates[weekdays(all_dates) == 'Sunday'][c(TRUE, FALSE)]
stripes <- data.frame(
  xmin = selected_dates,
  xmax = selected_dates + 7,
  ymin = -50,
  ymax = nrow(all_mcc_tls.df) + 50
)
stripes[1, 'xmin'] <- '2021-11-03'
tl_ntaxa_line.colors <- c('#b52e2a', '#d66324', '#78555b', '#d99b1e')
travel_ban <- data.frame(
  xmin = c(as.Date('2021-11-26')),
  xmax = c(as.Date('2021-12-15')),
  ymin = 0,
  ymax = nrow(all_mcc_tls.df) + 50
)
secondary_axis_factor <- 45/nrow(all_mcc_tls.df)
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='gray80', alpha=0.4) +
  geom_rect(data=travel_ban, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill='#3E4855', alpha=0.25) +
  geom_errorbarh(data=all_mcc_tls.df[all_mcc_tls.df$ntaxa_grp == 'singleton',],
                 aes(y=index, xmax=tmrca_date, xmin=ptmrca_date, color=ntaxa_grp),
                 size=0.5, height=90, alpha=0.5) +
  geom_errorbarh(data=all_mcc_tls.df[all_mcc_tls.df$ntaxa_grp == 'small',],
                 aes(y=index, xmax=tmrca_date, xmin=ptmrca_date, color=ntaxa_grp),
                 size=0.5, height=90, alpha=0.5) +
  geom_errorbarh(data=all_mcc_tls.df[all_mcc_tls.df$ntaxa_grp == 'medium',],
                 aes(y=index, xmax=tmrca_date, xmin=ptmrca_date, color=ntaxa_grp),
                 size=0.5, height=90, alpha=0.5) +
  geom_errorbarh(data=all_mcc_tls.df[all_mcc_tls.df$ntaxa_grp == 'large',],
                 aes(y=index, xmax=tmrca_date, xmin=ptmrca_date, color=ntaxa_grp),
                 size=0.5, height=90, alpha=0.5) +
  scale_x_date(date_breaks='14 days', date_labels='%b %d',
               limits=c(as.Date('2021-11-03'), as.Date('2022-02-03')), expand=c(0, 0)) +
  scale_y_continuous(
    name = 'Importation (from earliest to latest)',
    expand=c(0, 0)
  ) +
  scale_color_manual(values=tl_ntaxa_line.colors) +
  scale_fill_manual(values=tl_ntaxa_line.colors) +
  coord_cartesian(clip = "off") +
  labs(x='', y='') +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y.left = element_text(size=15.5, vjust=2),
    axis.text.x = element_text(face=1, size=14.5, vjust=-0.8, color='black'),
    axis.text.y = element_text(face=1, size=14.5, color='black'),
    plot.margin = margin(10, 25, 10, 10)
  )

## output plot to file
ggsave('./ptmrca_tmrca_distr.pdf',
       device='pdf', width=7.35, height=7)


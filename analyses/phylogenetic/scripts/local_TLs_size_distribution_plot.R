library(dplyr)
library(readr)
library(ggplot2)
library(bayestestR)

## read in posterior TLs and bin by ntaxa
## BA.1.15
BA115_plts.df <- read.csv('./posterior_tls/BA.1.15_posterior_tls.tsv', sep='\t')
BA115_plts.df <- BA115_plts.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  select(ntaxa, p_lab)
## BA.1.17
BA117_plts.df <- read.csv('./posterior_tls/BA.1.17_posterior_tls.tsv', sep='\t')
BA117_plts.df <- BA117_plts.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  select(ntaxa, p_lab)
## BA.1.1
BA11_plts.df <- read.csv('./posterior_tls/BA.1.1_posterior_tls.tsv', sep='\t')
BA11_plts.df <- BA11_plts.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  select(ntaxa, p_lab)
## BA.1
BA1_plts.df <- read.csv('./posterior_tls/BA.1_posterior_tls.tsv', sep='\t')
BA1_plts.df <- BA1_plts.df %>%
  filter(source == 'nonENG' & ntaxa > 0) %>%
  select(ntaxa, p_lab)

## bind all
all_plts.df <- rbind(
  BA115_plts.df,
  BA117_plts.df,
  BA11_plts.df,
  BA1_plts.df
)

## bin by size
max_ntaxa <- 23
all_plts.binned.df <- all_plts.df %>%
  mutate(capped_ntaxa=ifelse(ntaxa > max_ntaxa, max_ntaxa+1, ntaxa)) %>%
  group_by(capped_ntaxa, p_lab) %>%
  summarise(count=n()) %>%
  group_by(capped_ntaxa) %>%
  summarise(
    count.median=median(count),
    count.lw=hdi(count)[[1]],
    count.up=hdi(count)[[2]]
  )

## calculate cumulative number of BA.1 genomes contained in TLs
total_n <- 48748
all_plts.cum_n.df <- all_plts.df %>%
  # filter(ntaxa <= max_ntaxa) %>%
  group_by(ntaxa, p_lab) %>%
  summarise(count=n()) %>%
  group_by(p_lab) %>%
  mutate(
    cum_n=cumsum(ntaxa*count)/total_n
  ) %>%
  group_by(ntaxa) %>%
  summarise(
    cum_n.median=median(cum_n),
    cum_n.lw=hdi(cum_n)[[1]],
    cum_n.up=hdi(cum_n)[[2]]
  )
  
## make plots
cum_n_scale_factor <- 0.0005
ggplot() +
  geom_line(data=all_plts.cum_n.df[all_plts.cum_n.df$ntaxa < max_ntaxa,],
            aes(x=ntaxa, y=cum_n.median/cum_n_scale_factor),
            color='#0C4C5F', size=0.6, alpha=0.8) +
  geom_ribbon(data=all_plts.cum_n.df[all_plts.cum_n.df$ntaxa < max_ntaxa,],
              aes(x=ntaxa, ymin=cum_n.lw/cum_n_scale_factor, ymax=cum_n.up/cum_n_scale_factor),
              fill='#0C4C5F', alpha=0.3) +
  geom_bar(data=all_plts.binned.df %>%
             filter(capped_ntaxa > 1), 
           aes(x=capped_ntaxa, y=count.median), stat='identity',
           fill=c(rep('#3E4855', max_ntaxa-1), 'lightgrey'),
           color=c(rep('black', max_ntaxa-1), 'darkgrey'), alpha=0.8, width=0.8) +
  geom_errorbar(data=all_plts.binned.df %>%
                  filter(capped_ntaxa > 1), 
                aes(x=capped_ntaxa, ymin=count.lw, ymax=count.up),
                width=0.4, color='#D43F3A') +
  coord_cartesian(clip='off') +
  scale_y_continuous(
    expand=c(0, 0),
    limits=c(0, 1000),
    name='Number of transmission lineages',
    sec.axis = sec_axis(trans=~.*cum_n_scale_factor, name='Cumulative proportion of total genome samples')
  ) +
  scale_x_discrete(limits=c(seq(2, max_ntaxa)[lapply(seq(2, max_ntaxa), "%%", 2) == 0], max_ntaxa+1),
                   labels=c(seq(2, max_ntaxa)[lapply(seq(2, max_ntaxa), "%%", 2) == 0],
                            sprintf('>%d', max_ntaxa))) +
  labs(x='Tranmission lineage size (number of English BA.1 genomes)',
       y='Number of transmission lineages') +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    axis.title.y.left = element_text(vjust=0),
    axis.title.y.right = element_text(vjust=2),
    axis.text=element_text(size=11),
    axis.title=element_text(size=13)
  )

## output plot to file
ggsave('./all_tl_size_distr.pdf',
       device='pdf', width=7.5, height=4.5)


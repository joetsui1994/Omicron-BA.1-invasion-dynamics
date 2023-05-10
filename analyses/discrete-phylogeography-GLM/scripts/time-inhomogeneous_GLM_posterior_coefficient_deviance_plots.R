library(ggplot2)
library(dplyr)
library(HDInterval)
library(tidyr)

## function for removing burn-in
remove.burnin <- function(log, prop) {
  end = length(log$state)
  start = floor(prop*end)+1
  return(log[start:end,])
}

## read in log files
## BA.1.17_DTA_175_n11351 (does not require burn-in removal)
BA_1_17.epoch = read.table(
  "./logs/BA.1.17_DTA_175_n11351/BA.1.17_DTA_175_n11351.log", header=T)
## BA.1_DTA_1207_n9727
BA_1_n9727.epoch = read.table(
  "./logs/BA.1_DTA_1207_n9727/BA.1_DTA_1207_n9727.log", header=T)
BA_1_n9727.epoch <- remove.burnin(BA_1_n9727.epoch, 20000/max(BA_1_n9727.epoch$state))
## BA.1.15_DTA_102_n2975
BA_1_15_n2975.epoch = read.table(
  "./logs/BA.1.15_DTA_102_n2975/BA.1.15_DTA_102_n2975.log", header=T)
BA_1_15_n2975.epoch <- remove.burnin(BA_1_15_n2975.epoch, 130000/max(BA_1_15_n2975.epoch$state))
## combined5
joint_TLs.epoch = read.table(
  "./logs/combined5/combined5.log", header=T)

## read in deviance measure ranks
posterior_ranking.df <- read.csv('./results/GLM_posterior_deviance_ranking.csv', sep=',')
posterior_ranking.ranks.df <- posterior_ranking.df %>%
  select(name, covariate, n11351_rank, n9727_rank, n2975_rank, combined_rank) %>%
  pivot_longer(c('n11351_rank', 'n9727_rank', 'n2975_rank', 'combined_rank'), names_to='TL', values_to='rank') %>%
  mutate(
    TL=ifelse(startsWith(TL, 'n11351'), 'n11351',
              ifelse(startsWith(TL, 'n9727'), 'n9727',
                     ifelse(startsWith(TL, 'n2975'), 'n2975', 'combined'))),
    epoch=as.integer(ifelse(grepl('epoch1', name), 2, 1))
  )
covariates_order <- c(
  'geo distance',
  'Origin pop size',
  'Destination pop size',
  'Mobility matrix',
  'Mobility community structure overlap origin',
  'Mobility community structure overlap destination',
  'Greater London origin',
  'Greater London Destination',
  'Peak time origin',
  'Peak time destination',
  'Residual sample size /case count origin',
  'Residual sample size /case count destination'
)
posterior_ranking.ranks.df$covariate_order <- sapply(
  posterior_ranking.ranks.df$covariate, function(x) which(covariates_order == x))
posterior_ranking.ranks.df$rank <- as.integer(posterior_ranking.ranks.df$rank)

## BA.1.17 n11351: EPOCH 1
BA_1_17.epoch1.geo = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch21"]
BA_1_17.epoch1.popori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch22"]
BA_1_17.epoch1.popdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch23"]
BA_1_17.epoch1.Ldnori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch24"]
BA_1_17.epoch1.Ldndes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch25"]
BA_1_17.epoch1.peakori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch26"]
BA_1_17.epoch1.peakdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch27"]
BA_1_17.epoch1.resori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch28"]
BA_1_17.epoch1.resdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch29"]
BA_1_17.epoch1.mob = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch210"]
BA_1_17.epoch1.comoverori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch211"]
BA_1_17.epoch1.comoverdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch212"]

## BA.1.17 n11351: EPOCH 2
BA_1_17.epoch2.geo = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch11"]
BA_1_17.epoch2.popori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch12"]
BA_1_17.epoch2.popdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch13"]
BA_1_17.epoch2.Ldnori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch14"]
BA_1_17.epoch2.Ldndes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch15"]
BA_1_17.epoch2.peakori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch16"]
BA_1_17.epoch2.peakdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch17"]
BA_1_17.epoch2.resori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch18"]
BA_1_17.epoch2.resdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch19"]
BA_1_17.epoch2.mob = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch110"]
BA_1_17.epoch2.comoverori = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch111"]
BA_1_17.epoch2.comoverdes = BA_1_17.epoch[,"ltla.coefficientsTimesIndicators.epoch112"]

## BA.1 n9727 EPOCH 1
BA_1_n9727.epoch1.geo = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch21"]
BA_1_n9727.epoch1.popori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch22"]
BA_1_n9727.epoch1.popdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch23"]
BA_1_n9727.epoch1.Ldnori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch24"]
BA_1_n9727.epoch1.Ldndes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch25"]
BA_1_n9727.epoch1.peakori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch26"]
BA_1_n9727.epoch1.peakdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch27"]
BA_1_n9727.epoch1.resori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch28"]
BA_1_n9727.epoch1.resdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch29"]
BA_1_n9727.epoch1.mob = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch210"]
BA_1_n9727.epoch1.comoverori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch211"]
BA_1_n9727.epoch1.comoverdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch212"]

## BA.1 n9727 EPOCH 2
BA_1_n9727.epoch2.geo = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch11"]
BA_1_n9727.epoch2.popori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch12"]
BA_1_n9727.epoch2.popdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch13"]
BA_1_n9727.epoch2.Ldnori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch14"]
BA_1_n9727.epoch2.Ldndes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch15"]
BA_1_n9727.epoch2.peakori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch16"]
BA_1_n9727.epoch2.peakdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch17"]
BA_1_n9727.epoch2.resori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch18"]
BA_1_n9727.epoch2.resdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch19"]
BA_1_n9727.epoch2.mob = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch110"]
BA_1_n9727.epoch2.comoverori = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch111"]
BA_1_n9727.epoch2.comoverdes = BA_1_n9727.epoch[,"ltla.coefficientsTimesIndicators.epoch112"]

## BA.1.15 n2975 EPOCH 1
BA_1_15_n2975.epoch1.geo = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch21"]
BA_1_15_n2975.epoch1.popori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch22"]
BA_1_15_n2975.epoch1.popdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch23"]
BA_1_15_n2975.epoch1.mob = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch24"]
BA_1_15_n2975.epoch1.comoverori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch25"]
BA_1_15_n2975.epoch1.comoverdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch26"]
BA_1_15_n2975.epoch1.Ldnori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch27"]
BA_1_15_n2975.epoch1.Ldndes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch28"]
BA_1_15_n2975.epoch1.peakori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch29"]
BA_1_15_n2975.epoch1.peakdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch210"]
BA_1_15_n2975.epoch1.resori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch211"]
BA_1_15_n2975.epoch1.resdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch212"]

## BA.1.15 n2975 EPOCH 2
BA_1_15_n2975.epoch2.geo = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch11"]
BA_1_15_n2975.epoch2.popori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch12"]
BA_1_15_n2975.epoch2.popdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch13"]
BA_1_15_n2975.epoch2.mob = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch14"]
BA_1_15_n2975.epoch2.comoverori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch15"]
BA_1_15_n2975.epoch2.comoverdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch16"]
BA_1_15_n2975.epoch2.Ldnori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch17"]
BA_1_15_n2975.epoch2.Ldndes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch18"]
BA_1_15_n2975.epoch2.peakori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch19"]
BA_1_15_n2975.epoch2.peakdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch110"]
BA_1_15_n2975.epoch2.resori = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch111"]
BA_1_15_n2975.epoch2.resdes = BA_1_15_n2975.epoch[,"ltla.coefficientsTimesIndicators.epoch112"]

## joint-TL EPOCH 1
joint.epoch1.geo = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch21"]
joint.epoch1.popori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch22"]
joint.epoch1.popdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch23"]
joint.epoch1.Ldnori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch24"]
joint.epoch1.Ldndes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch25"]
joint.epoch1.peakori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch26"]
joint.epoch1.peakdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch27"]
joint.epoch1.resori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch28"]
joint.epoch1.resdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch29"]
joint.epoch1.mob = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch210"]
joint.epoch1.comoverori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch211"]
joint.epoch1.comoverdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch212"]

## joint-TL EPOCH 2
joint.epoch2.geo = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch11"]
joint.epoch2.popori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch12"]
joint.epoch2.popdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch13"]
joint.epoch2.Ldnori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch14"]
joint.epoch2.Ldndes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch15"]
joint.epoch2.peakori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch16"]
joint.epoch2.peakdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch17"]
joint.epoch2.resori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch18"]
joint.epoch2.resdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch19"]
joint.epoch2.mob = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch110"]
joint.epoch2.comoverori = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch111"]
joint.epoch2.comoverdes = joint_TLs.epoch[,"ltla.coefficientsTimesIndicators.epoch112"]

## create combined dataframe
## specify colour scheme
colors <- c(
  '#1B7883',
  '#313030',
  '#855f66',
  '#D43F3A',
  '#E6824C',
  '#F8C55D'
)

## BA_1_17 combined dataframe
BA_1_17.combined.df <- data.frame(
  index = rep(seq(1:24),
              rep(c(length(BA_1_17.epoch1.geo),
                    length(BA_1_17.epoch2.geo)), 12)),
  vals = c(
    BA_1_17.epoch1.geo, BA_1_17.epoch2.geo,
    BA_1_17.epoch1.popori, BA_1_17.epoch2.popori,
    BA_1_17.epoch1.popdes, BA_1_17.epoch2.popdes,
    BA_1_17.epoch1.mob, BA_1_17.epoch2.mob,
    BA_1_17.epoch1.comoverori, BA_1_17.epoch2.comoverori,
    BA_1_17.epoch1.comoverdes, BA_1_17.epoch2.comoverdes,
    BA_1_17.epoch1.Ldnori, BA_1_17.epoch2.Ldnori,
    BA_1_17.epoch1.Ldndes, BA_1_17.epoch2.Ldndes,
    BA_1_17.epoch1.peakori, BA_1_17.epoch2.peakori,
    BA_1_17.epoch1.peakdes, BA_1_17.epoch2.peakdes,
    BA_1_17.epoch1.resori, BA_1_17.epoch2.resori,
    BA_1_17.epoch1.resdes, BA_1_17.epoch2.resdes
  )
)
BA_1_17.combined.df <- BA_1_17.combined.df %>%
  mutate(
    color = ifelse(index >= 1 & index <= 2, colors[1],
                   ifelse(index >= 3 & index <= 6, colors[2],
                          ifelse(index >= 7 & index <= 12, colors[3],
                                 ifelse(index >= 13 & index <= 16, colors[4],
                                        ifelse(index >= 17 & index <= 20, colors[5], colors[6]))))),
    index = index + floor((index+1)/2) - 1
  )

## BA_1_n9727 combined dataframe
BA_1_n9727.combined.df <- data.frame(
  index = rep(seq(1:24),
              rep(c(length(BA_1_n9727.epoch1.geo),
                    length(BA_1_n9727.epoch2.geo)), 12)),
  vals = c(
    BA_1_n9727.epoch1.geo, BA_1_n9727.epoch2.geo,
    BA_1_n9727.epoch1.popori, BA_1_n9727.epoch2.popori,
    BA_1_n9727.epoch1.popdes, BA_1_n9727.epoch2.popdes,
    BA_1_n9727.epoch1.mob, BA_1_n9727.epoch2.mob,
    BA_1_n9727.epoch1.comoverori, BA_1_n9727.epoch2.comoverori,
    BA_1_n9727.epoch1.comoverdes, BA_1_n9727.epoch2.comoverdes,
    BA_1_n9727.epoch1.Ldnori, BA_1_n9727.epoch2.Ldnori,
    BA_1_n9727.epoch1.Ldndes, BA_1_n9727.epoch2.Ldndes,
    BA_1_n9727.epoch1.peakori, BA_1_n9727.epoch2.peakori,
    BA_1_n9727.epoch1.peakdes, BA_1_n9727.epoch2.peakdes,
    BA_1_n9727.epoch1.resori, BA_1_n9727.epoch2.resori,
    BA_1_n9727.epoch1.resdes, BA_1_n9727.epoch2.resdes
  )
)
BA_1_n9727.combined.df <- BA_1_n9727.combined.df %>%
  mutate(
    color = ifelse(index >= 1 & index <= 2, colors[1],
                   ifelse(index >= 3 & index <= 6, colors[2],
                          ifelse(index >= 7 & index <= 12, colors[3],
                                 ifelse(index >= 13 & index <= 16, colors[4],
                                        ifelse(index >= 17 & index <= 20, colors[5], colors[6]))))),
    index = index + floor((index+1)/2) - 1
  )

## BA_1_15_n2975 combined dataframe
BA_1_15.combined.df <- data.frame(
  index = rep(seq(1:24),
              rep(c(length(BA_1_15_n2975.epoch1.geo),
                    length(BA_1_15_n2975.epoch2.geo)), 12)),
  vals = c(
    BA_1_15_n2975.epoch1.geo, BA_1_15_n2975.epoch2.geo,
    BA_1_15_n2975.epoch1.popori, BA_1_15_n2975.epoch2.popori,
    BA_1_15_n2975.epoch1.popdes, BA_1_15_n2975.epoch2.popdes,
    BA_1_15_n2975.epoch1.mob, BA_1_15_n2975.epoch2.mob,
    BA_1_15_n2975.epoch1.comoverori, BA_1_15_n2975.epoch2.comoverori,
    BA_1_15_n2975.epoch1.comoverdes, BA_1_15_n2975.epoch2.comoverdes,
    BA_1_15_n2975.epoch1.Ldnori, BA_1_15_n2975.epoch2.Ldnori,
    BA_1_15_n2975.epoch1.Ldndes, BA_1_15_n2975.epoch2.Ldndes,
    BA_1_15_n2975.epoch1.peakori, BA_1_15_n2975.epoch2.peakori,
    BA_1_15_n2975.epoch1.peakdes, BA_1_15_n2975.epoch2.peakdes,
    BA_1_15_n2975.epoch1.resori, BA_1_15_n2975.epoch2.resori,
    BA_1_15_n2975.epoch1.resdes, BA_1_15_n2975.epoch2.resdes
  )
)
BA_1_15.combined.df <- BA_1_15.combined.df %>%
  mutate(
    color = ifelse(index >= 1 & index <= 2, 'a',
                   ifelse(index >= 3 & index <= 6, 'b',
                          ifelse(index >= 7 & index <= 12, 'c',
                                 ifelse(index >= 13 & index <= 16, 'd',
                                        ifelse(index >= 17 & index <= 20, 'e', 'f'))))),
    index = index + floor((index+1)/2) - 1
  )

## joint-TL combined dataframe
joint.combined.df <- data.frame(
  index = rep(seq(1:24),
              rep(c(length(joint.epoch1.geo),
                    length(joint.epoch2.geo)), 12)),
  vals = c(
    joint.epoch1.geo, joint.epoch2.geo,
    joint.epoch1.popori, joint.epoch2.popori,
    joint.epoch1.popdes, joint.epoch2.popdes,
    joint.epoch1.mob, joint.epoch2.mob,
    joint.epoch1.comoverori, joint.epoch2.comoverori,
    joint.epoch1.comoverdes, joint.epoch2.comoverdes,
    joint.epoch1.Ldnori, joint.epoch2.Ldnori,
    joint.epoch1.Ldndes, joint.epoch2.Ldndes,
    joint.epoch1.peakori, joint.epoch2.peakori,
    joint.epoch1.peakdes, joint.epoch2.peakdes,
    joint.epoch1.resori, joint.epoch2.resori,
    joint.epoch1.resdes, joint.epoch2.resdes
  )
)
joint.combined.df <- joint.combined.df %>%
  mutate(
    color = ifelse(index >= 1 & index <= 2, 'a',
                   ifelse(index >= 3 & index <= 6, 'b',
                          ifelse(index >= 7 & index <= 12, 'c',
                                 ifelse(index >= 13 & index <= 16, 'd',
                                        ifelse(index >= 17 & index <= 20, 'e', 'f'))))),
    index = index + floor((index+1)/2) - 1
  )

## Plot for BA_1_17.combined.df
rank_margins <- c(0.45, 0.15)
rank_box.height <- 0.283
rank_box.width <- 2
hdi_95 <- function(x) {
  r_95 <- hdi(x, credMass=0.95)
  r_50 <- hdi(x, credMass=0.5)
  r_m <- median(x)
  r <- c(r_95[1], r_50[1], r_m, r_50[2], r_95[2])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(BA_1_17.combined.df$vals)) + 0.1
ymin <- min(c(BA_1_17.combined.df$vals)) - sum(rank_margins) - 2*rank_box.height
stripes <- data.frame(
  xmin = c(0, 6, 12, 18, 24, 30, 36),
  xmax = c(3, 9, 15, 21, 27, 33, 37),
  ymin = ymin,
  ymax = ymax
)
rank_box.df <- data.frame(
  xmin = seq(0, 34.5, by=1.5),
  xmax = seq(1.5, 36, by=1.5),
  ymin = rep(c(min(c(BA_1_17.combined.df$vals)) - rank_margins[1] - rank_box.height,
               min(c(BA_1_17.combined.df$vals)) - rank_margins[1] - 2*rank_box.height),
             as.integer(length(unique(BA_1_17.combined.df$index))/2)),
  ymax = rep(c(min(c(BA_1_17.combined.df$vals)) - rank_margins[1],
               min(c(BA_1_17.combined.df$vals)) - rank_margins[1] - rank_box.height),
             as.integer(length(unique(BA_1_17.combined.df$index))/2)),
  epoch = rep(c(1, 2), as.integer(length(unique(BA_1_17.combined.df$index))/2))
)
rank_box.df <- rank_box.df %>%
  mutate(index=floor((row_number() + 1)/2)) %>%
  left_join(posterior_ranking.ranks.df %>%
              filter(TL == 'n11351') %>%
              select(covariate, covariate_order, epoch, rank) %>%
              rename(index=covariate_order), by=c('index', 'epoch'))
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='#484848', fill='white', linewidth=0, alpha=1) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha=rank),
            color='#484848', fill='#bf551b', linewidth=0.2) +
  geom_text(dat=rank_box.df,
            aes(x=xmin+(xmax-xmin)/2,
                y=ymin+(ymax-ymin)/2, label=rank), size=3.3) +
  stat_summary(data=BA_1_17.combined.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_alpha_continuous(range=c(0.75, 0.05)) +
  scale_x_continuous(limits=c(-1, 37),
                     breaks=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5),
                     labels=c('geo distance', 'pop size ori', 'pop size dest',
                              'mobility mat', 'comm overlap l1', 'comm overlap l2',
                              'gr LDN ori', 'gr LDN dest', 'peak time ori',
                              'peak time dest', 'sample res ori', 'sample res dest'),
                     expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2),
                     expand=c(0, 0)) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_hline(yintercept=min(c(BA_1_17.combined.df$vals)) - rank_margins[1] + rank_margins[2],
             linetype='dashed', color='black', size=0.5, alpha=0.3) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.text.x = element_text(face=1, size=11, vjust=0.4, hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=9.5, color='black'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

## output plot to file
ggsave('./figures/BA_1_17.combined.pdf',
       device='pdf', width=8, height=5.34)

## Plot for BA_1_n9727.combined.df
rank_margins <- c(0.45, 0.15)
rank_box.height <- 0.32
rank_box.width <- 2
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(BA_1_n9727.combined.df$vals)) + 0.1
ymin <- min(c(BA_1_n9727.combined.df$vals)) - sum(rank_margins) - 2*rank_box.height
stripes <- data.frame(
  xmin = c(0, 6, 12, 18, 24, 30, 36),
  xmax = c(3, 9, 15, 21, 27, 33, 37),
  ymin = ymin,
  ymax = ymax
)
rank_box.df <- data.frame(
  xmin = seq(0, 34.5, by=1.5),
  xmax = seq(1.5, 36, by=1.5),
  ymin = rep(c(min(c(BA_1_n9727.combined.df$vals)) - rank_margins[1] - rank_box.height,
               min(c(BA_1_n9727.combined.df$vals)) - rank_margins[1] - 2*rank_box.height),
             as.integer(length(unique(BA_1_n9727.combined.df$index))/2)),
  ymax = rep(c(min(c(BA_1_n9727.combined.df$vals)) - rank_margins[1],
               min(c(BA_1_n9727.combined.df$vals)) - rank_margins[1] - rank_box.height),
             as.integer(length(unique(BA_1_n9727.combined.df$index))/2)),
  epoch = rep(c(1, 2), as.integer(length(unique(BA_1_n9727.combined.df$index))/2))
)
rank_box.df <- rank_box.df %>%
  mutate(index=floor((row_number() + 1)/2)) %>%
  left_join(posterior_ranking.ranks.df %>%
              filter(TL == 'n9727') %>%
              select(covariate, covariate_order, epoch, rank) %>%
              rename(index=covariate_order), by=c('index', 'epoch'))
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='#484848', fill='white', linewidth=0, alpha=1) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha=rank),
            color='#484848', fill='#bf551b', linewidth=0.2) +
  geom_text(dat=rank_box.df,
            aes(x=xmin+(xmax-xmin)/2,
                y=ymin+(ymax-ymin)/2, label=rank), size=3.3) +
  stat_summary(data=BA_1_n9727.combined.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_alpha_continuous(range=c(0.75, 0.05)) +
  scale_x_continuous(limits=c(-1, 37),
                     breaks=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5),
                     labels=c('geo distance', 'pop size ori', 'pop size dest',
                              'mobility mat', 'comm overlap l1', 'comm overlap l2',
                              'gr LDN ori', 'gr LDN dest', 'peak time ori',
                              'peak time dest', 'sample res ori', 'sample res dest'),
                     expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2),
                     expand=c(0, 0)) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_hline(yintercept=min(c(BA_1_n9727.combined.df$vals)) - rank_margins[1] + rank_margins[2],
             linetype='dashed', color='black', size=0.5, alpha=0.3) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.text.x = element_text(face=1, size=11, vjust=0.4, hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=9.5, color='black'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

## output plot to file
ggsave('./BA_1.combined.pdf',
       device='pdf', width=8, height=5.34)

## Plot for BA_1_15.combined.df
rank_margins <- c(0.45, 0.15)
rank_box.height <- 0.37
rank_box.width <- 2
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(BA_1_15.combined.df$vals)) + 0.1
ymin <- min(c(BA_1_15.combined.df$vals)) - sum(rank_margins) - 2*rank_box.height
stripes <- data.frame(
  xmin = c(0, 6, 12, 18, 24, 30, 36),
  xmax = c(3, 9, 15, 21, 27, 33, 37),
  ymin = ymin,
  ymax = ymax
)
rank_box.df <- data.frame(
  xmin = seq(0, 34.5, by=1.5),
  xmax = seq(1.5, 36, by=1.5),
  ymin = rep(c(min(c(BA_1_15.combined.df$vals)) - rank_margins[1] - rank_box.height,
               min(c(BA_1_15.combined.df$vals)) - rank_margins[1] - 2*rank_box.height),
             as.integer(length(unique(BA_1_15.combined.df$index))/2)),
  ymax = rep(c(min(c(BA_1_15.combined.df$vals)) - rank_margins[1],
               min(c(BA_1_15.combined.df$vals)) - rank_margins[1] - rank_box.height),
             as.integer(length(unique(BA_1_15.combined.df$index))/2)),
  epoch = rep(c(1, 2), as.integer(length(unique(BA_1_15.combined.df$index))/2))
)
rank_box.df <- rank_box.df %>%
  mutate(index=floor((row_number() + 1)/2)) %>%
  left_join(posterior_ranking.ranks.df %>%
              filter(TL == 'n2975') %>%
              select(covariate, covariate_order, epoch, rank) %>%
              rename(index=covariate_order), by=c('index', 'epoch'))
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='#484848', fill='white', linewidth=0, alpha=1) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha=rank),
            color='#484848', fill='#bf551b', linewidth=0.2) +
  geom_text(dat=rank_box.df,
            aes(x=xmin+(xmax-xmin)/2,
                y=ymin+(ymax-ymin)/2, label=rank), size=3.3) +
  stat_summary(data=BA_1_15.combined.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_alpha_continuous(range=c(0.75, 0.05)) +
  scale_x_continuous(limits=c(-1, 37),
                     breaks=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5),
                     labels=c('geo distance', 'pop size ori', 'pop size dest',
                              'mobility mat', 'comm overlap l1', 'comm overlap l2',
                              'gr LDN ori', 'gr LDN dest', 'peak time ori',
                              'peak time dest', 'sample res ori', 'sample res dest'),
                     expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5),
                     expand=c(0, 0)) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_hline(yintercept=min(c(BA_1_15.combined.df$vals)) - rank_margins[1] + rank_margins[2],
             linetype='dashed', color='black', size=0.5, alpha=0.3) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.text.x = element_text(face=1, size=11, vjust=0.4, hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=9.5, color='black'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

## output plot to file
ggsave('./BA_1_15.combined.pdf',
       device='pdf', width=8, height=5.34)

## Plot for joint.combined.df
rank_margins <- c(0.45, 0.15)
rank_box.height <- 0.37
rank_box.width <- 2
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(joint.combined.df$vals)) + 0.1
ymin <- min(c(joint.combined.df$vals)) - sum(rank_margins) - 2*rank_box.height
stripes <- data.frame(
  xmin = c(0, 6, 12, 18, 24, 30, 36),
  xmax = c(3, 9, 15, 21, 27, 33, 37),
  ymin = ymin,
  ymax = ymax
)
rank_box.df <- data.frame(
  xmin = seq(0, 34.5, by=1.5),
  xmax = seq(1.5, 36, by=1.5),
  ymin = rep(c(min(c(joint.combined.df$vals)) - rank_margins[1] - rank_box.height,
               min(c(joint.combined.df$vals)) - rank_margins[1] - 2*rank_box.height),
             as.integer(length(unique(joint.combined.df$index))/2)),
  ymax = rep(c(min(c(joint.combined.df$vals)) - rank_margins[1],
               min(c(joint.combined.df$vals)) - rank_margins[1] - rank_box.height),
             as.integer(length(unique(joint.combined.df$index))/2)),
  epoch = rep(c(1, 2), as.integer(length(unique(joint.combined.df$index))/2))
)
rank_box.df <- rank_box.df %>%
  mutate(index=floor((row_number() + 1)/2)) %>%
  left_join(posterior_ranking.ranks.df %>%
              filter(TL == 'combined') %>%
              select(covariate, covariate_order, epoch, rank) %>%
              rename(index=covariate_order), by=c('index', 'epoch'))
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color='#484848', fill='white', linewidth=0, alpha=1) +
  geom_rect(dat=rank_box.df,
            mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, alpha=rank),
            color='#484848', fill='#bf551b', linewidth=0.2) +
  geom_text(dat=rank_box.df,
            aes(x=xmin+(xmax-xmin)/2,
                y=ymin+(ymax-ymin)/2, label=rank), size=3.3) +
  stat_summary(data=joint.combined.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_alpha_continuous(range=c(0.75, 0.05)) +
  scale_x_continuous(limits=c(-1, 37),
                     breaks=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5),
                     labels=c('geo distance', 'pop size ori', 'pop size dest',
                              'mobility mat', 'comm overlap l1', 'comm overlap l2',
                              'gr LDN ori', 'gr LDN dest', 'peak time ori',
                              'peak time dest', 'sample res ori', 'sample res dest'),
                     expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5),
                     expand=c(0, 0)) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_hline(yintercept=min(c(joint.combined.df$vals)) - rank_margins[1] + rank_margins[2],
             linetype='dashed', color='black', size=0.5, alpha=0.3) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.text.x = element_text(face=1, size=11, vjust=0.4, hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=9.5, color='black'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

## output plot to file
ggsave('./figures/joint-TL.combined.pdf',
       device='pdf', width=8, height=5.34)


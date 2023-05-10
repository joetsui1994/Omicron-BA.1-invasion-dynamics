library(ggplot2)
library(dplyr)
library(HDInterval)

## function for removing burn-in
remove.burnin <- function(log, prop) {
  end = length(log$state)
  start = floor(prop*end)+1
  return(log[start:end,])
}

## read in log files
## BA.1.17_DTA_175_n11351 (does not require burn-in removal)
BA_1_17.boosters.epoch <- read.table('./results/BA.1.17_DTA_175_n11351.log', header=T)

## specify order of covariates
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

## BA.1.17 n11351: EPOCH 1
BA_1_17.boosters.epoch1.geo = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch21"]
BA_1_17.boosters.epoch1.popori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch22"]
BA_1_17.boosters.epoch1.popdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch23"]
BA_1_17.boosters.epoch1.Ldnori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch24"]
BA_1_17.boosters.epoch1.Ldndes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch25"]
BA_1_17.boosters.epoch1.peakori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch26"]
BA_1_17.boosters.epoch1.peakdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch27"]
BA_1_17.boosters.epoch1.resori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch28"]
BA_1_17.boosters.epoch1.resdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch29"]
BA_1_17.boosters.epoch1.mob = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch210"]
BA_1_17.boosters.epoch1.boosterori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch211"]
BA_1_17.boosters.epoch1.boosterdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch212"]
BA_1_17.boosters.epoch1.comoverori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch213"]
BA_1_17.boosters.epoch1.comoverdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch214"]

## BA.1.17 n11351: EPOCH 2
BA_1_17.boosters.epoch2.geo = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch11"]
BA_1_17.boosters.epoch2.popori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch12"]
BA_1_17.boosters.epoch2.popdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch13"]
BA_1_17.boosters.epoch2.Ldnori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch14"]
BA_1_17.boosters.epoch2.Ldndes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch15"]
BA_1_17.boosters.epoch2.peakori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch16"]
BA_1_17.boosters.epoch2.peakdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch17"]
BA_1_17.boosters.epoch2.resori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch18"]
BA_1_17.boosters.epoch2.resdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch19"]
BA_1_17.boosters.epoch2.mob = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch110"]
BA_1_17.boosters.epoch2.boosterori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch111"]
BA_1_17.boosters.epoch2.boosterdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch112"]
BA_1_17.boosters.epoch2.comoverori = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch113"]
BA_1_17.boosters.epoch2.comoverdes = BA_1_17.boosters.epoch[,"ltla.coefficientsTimesIndicators.epoch114"]

## create combined dataframe
## specify colour scheme
colors <- c(
  '#1B7883',
  '#313030',
  '#855f66',
  '#D43F3A',
  '#E6824C',
  '#F8C55D',
  '#947A47'
)

## BA_1_17 combined dataframe
BA_1_17.boosters.combined.df <- data.frame(
  index = rep(seq(1:28),
              rep(c(length(BA_1_17.boosters.epoch1.geo),
                    length(BA_1_17.boosters.epoch2.geo)), 14)),
  vals = c(
    BA_1_17.boosters.epoch1.geo, BA_1_17.boosters.epoch2.geo,
    BA_1_17.boosters.epoch1.popori, BA_1_17.boosters.epoch2.popori,
    BA_1_17.boosters.epoch1.popdes, BA_1_17.boosters.epoch2.popdes,
    BA_1_17.boosters.epoch1.mob, BA_1_17.boosters.epoch2.mob,
    BA_1_17.boosters.epoch1.comoverori, BA_1_17.boosters.epoch2.comoverori,
    BA_1_17.boosters.epoch1.comoverdes, BA_1_17.boosters.epoch2.comoverdes,
    BA_1_17.boosters.epoch1.Ldnori, BA_1_17.boosters.epoch2.Ldnori,
    BA_1_17.boosters.epoch1.Ldndes, BA_1_17.boosters.epoch2.Ldndes,
    BA_1_17.boosters.epoch1.peakori, BA_1_17.boosters.epoch2.peakori,
    BA_1_17.boosters.epoch1.peakdes, BA_1_17.boosters.epoch2.peakdes,
    BA_1_17.boosters.epoch1.resori, BA_1_17.boosters.epoch2.resori,
    BA_1_17.boosters.epoch1.resdes, BA_1_17.boosters.epoch2.resdes,
    BA_1_17.boosters.epoch1.boosterori, BA_1_17.boosters.epoch2.boosterori,
    BA_1_17.boosters.epoch1.boosterdes, BA_1_17.boosters.epoch2.boosterdes
  )
)
BA_1_17.boosters.combined.df <- BA_1_17.boosters.combined.df %>%
  mutate(
    color = ifelse(index >= 1 & index <= 2, 'a',
                   ifelse(index >= 3 & index <= 6, 'b',
                          ifelse(index >= 7 & index <= 12, 'c',
                                 ifelse(index >= 13 & index <= 16, 'd',
                                        ifelse(index >= 17 & index <= 20, 'e',
                                               ifelse(index >= 21 & index <= 24, 'f', 'g')))))),
    index = index + floor((index+1)/2) - 1
  )

## Plot for BA_1_17.combined.df
hdi_95 <- function(x) {
  r_95 <- hdi(x, credMass=0.95)
  r_50 <- hdi(x, credMass=0.5)
  r_m <- median(x)
  r <- c(r_95[1], r_50[1], r_m, r_50[2], r_95[2])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(BA_1_17.boosters.combined.df$vals)) + 0.1
ymin <- min(c(BA_1_17.boosters.combined.df$vals)) - 0.1
stripes <- data.frame(
  xmin = c(0, 6, 12, 18, 24, 30, 36, 42),
  xmax = c(3, 9, 15, 21, 27, 33, 39, 43),
  ymin = ymin,
  ymax = ymax
)
ggplot() +
  geom_rect(dat=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  stat_summary(data=BA_1_17.boosters.combined.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_x_continuous(limits=c(-1, 43),
                     breaks=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5, 37.5, 40.5),
                     labels=c('geo distance', 'pop size ori', 'pop size dest',
                              'mobility mat', 'comm overlap l1', 'comm overlap l2',
                              'gr LDN ori', 'gr LDN dest', 'peak time ori',
                              'peak time dest', 'sample res ori', 'sample res dest',
                              'booster uptake ori', 'booster uptake dest'),
                     expand=c(0, 0)) +
  scale_y_continuous(limits=c(ymin, ymax),
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2),
                     expand=c(0, 0)) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.text.x = element_text(face=1, size=11, vjust=0.6, hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=9.5, color='black'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

## output plot to file
ggsave('./figures/BA_1_17.combined_Apr_updated_v1.pdf',
       device='pdf', width=8.5, height=5.5)


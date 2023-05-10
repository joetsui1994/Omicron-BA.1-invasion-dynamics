library(ggplot2)
library(dplyr)
library(HDInterval)

## function for removing burn-in
remove.burnin <- function(log,prop) {
  end = length(log$state)
  start = floor(prop * end)
  return(log[start:end,])
}

## reading in estimates
## BA.1.17_DTA_175_n11351
BA_1_17.homo = read.table("./BA.1.17_DTA_175_n11351.log", header=T)
## BA.1.15_DTA_102_n2975
BA_1_15_n2975.homo = read.table("./BA.1.15_DTA_102_n2975.log", header=T)
BA_1_15_n2975.homo <- remove.burnin(BA_1_15_n2975.homo, 0.20)
## BA.1_DTA_1207_n9727
BA_1_n9727.homo = read.table("./BA.1_DTA_1207_n9727.log", header=T)
BA_1_n9727.homo <- remove.burnin(BA_1_n9727.homo, 0.25)
## combined-TL
combined.homo = read.table("./combined5.log", header=T)
combined.homo <- remove.burnin(combined.homo, 0.25)

## BA.1.17 (n11351; time-homogeneous)
BA_1_17.homo.geo = BA_1_17.homo[,"ltla.coefficientsTimesIndicators1"]
BA_1_17.homo.popori = BA_1_17.homo[,"ltla.coefficientsTimesIndicators2"]
BA_1_17.homo.popdes = BA_1_17.homo[,"ltla.coefficientsTimesIndicators3"]
BA_1_17.homo.Ldnori = BA_1_17.homo[,"ltla.coefficientsTimesIndicators4"]
BA_1_17.homo.Ldndes = BA_1_17.homo[,"ltla.coefficientsTimesIndicators5"]
BA_1_17.homo.peakori = BA_1_17.homo[,"ltla.coefficientsTimesIndicators6"]
BA_1_17.homo.peakdes = BA_1_17.homo[,"ltla.coefficientsTimesIndicators7"]
BA_1_17.homo.resori = BA_1_17.homo[,"ltla.coefficientsTimesIndicators8"]
BA_1_17.homo.resdes = BA_1_17.homo[,"ltla.coefficientsTimesIndicators9"]
BA_1_17.homo.mob = BA_1_17.homo[,"ltla.coefficientsTimesIndicators10"]
BA_1_17.homo.comoverori = BA_1_17.homo[,"ltla.coefficientsTimesIndicators11"]
BA_1_17.homo.comoverdes = BA_1_17.homo[,"ltla.coefficientsTimesIndicators12"]

## BA.1 (n9727; time-homogeneous)
BA_1_n9727.homo.geo = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators1"]
BA_1_n9727.homo.popori = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators2"]
BA_1_n9727.homo.popdes = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators3"]
BA_1_n9727.homo.Ldnori = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators7"]
BA_1_n9727.homo.Ldndes = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators8"]
BA_1_n9727.homo.peakori = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators9"]
BA_1_n9727.homo.peakdes = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators10"]
BA_1_n9727.homo.resori = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators11"]
BA_1_n9727.homo.resdes = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators12"]
BA_1_n9727.homo.mob = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators4"]
BA_1_n9727.homo.comoverori = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators5"]
BA_1_n9727.homo.comoverdes = BA_1_n9727.homo[,"ltla.coefficientsTimesIndicators6"]

## BA.1.15 (n2975; time-homogeneous)
BA_1_15_n2975.homo.geo = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators1"]
BA_1_15_n2975.homo.popori = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators2"]
BA_1_15_n2975.homo.popdes = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators3"]
BA_1_15_n2975.homo.Ldnori = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators7"]
BA_1_15_n2975.homo.Ldndes = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators8"]
BA_1_15_n2975.homo.peakori = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators9"]
BA_1_15_n2975.homo.peakdes = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators10"]
BA_1_15_n2975.homo.resori = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators11"]
BA_1_15_n2975.homo.resdes = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators12"]
BA_1_15_n2975.homo.mob = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators4"]
BA_1_15_n2975.homo.comoverori = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators5"]
BA_1_15_n2975.homo.comoverdes = BA_1_15_n2975.homo[,"ltla.coefficientsTimesIndicators6"]

## combined-TL (time-homogeneous)
combined.homo.geo = combined.homo[,"ltla.coefficientsTimesIndicators1"]
combined.homo.popori = combined.homo[,"ltla.coefficientsTimesIndicators2"]
combined.homo.popdes = combined.homo[,"ltla.coefficientsTimesIndicators3"]
combined.homo.Ldnori = combined.homo[,"ltla.coefficientsTimesIndicators4"]
combined.homo.Ldndes = combined.homo[,"ltla.coefficientsTimesIndicators5"]
combined.homo.peakori = combined.homo[,"ltla.coefficientsTimesIndicators6"]
combined.homo.peakdes = combined.homo[,"ltla.coefficientsTimesIndicators7"]
combined.homo.resori = combined.homo[,"ltla.coefficientsTimesIndicators8"]
combined.homo.resdes = combined.homo[,"ltla.coefficientsTimesIndicators9"]
combined.homo.mob = combined.homo[,"ltla.coefficientsTimesIndicators10"]
combined.homo.comoverori = combined.homo[,"ltla.coefficientsTimesIndicators11"]
combined.homo.comoverdes = combined.homo[,"ltla.coefficientsTimesIndicators12"]

## create combined dataframe
## specify colour scheme
colors <- c(
  '#313030',
  '#1B7883',
  '#855f66',
  '#D43F3A',
  '#E6824C',
  '#F8C55D'
)

## BA_1_17 combined dataframe
BA_1_17.homo.df <- data.frame(
  index = rep(seq(1:12), rep(length(BA_1_17.homo.geo), 12)),
  vals = c(
    BA_1_17.homo.geo,
    BA_1_17.homo.popori,
    BA_1_17.homo.popdes,
    BA_1_17.homo.mob,
    BA_1_17.homo.comoverori,
    BA_1_17.homo.comoverdes,
    BA_1_17.homo.Ldnori,
    BA_1_17.homo.Ldndes,
    BA_1_17.homo.peakori,
    BA_1_17.homo.peakdes,
    BA_1_17.homo.resori,
    BA_1_17.homo.resdes
  )
)
BA_1_17.homo.df <- BA_1_17.homo.df %>%
  mutate(
    color = ifelse(index == 1, colors[1],
                   ifelse(index >= 2 & index <= 3, colors[2],
                          ifelse(index >= 4 & index <= 6, colors[3],
                                 ifelse(index >= 7 & index <= 8, colors[4],
                                        ifelse(index >= 9 & index <= 10, colors[5], colors[6]))))),
    index = index*2,
  )

## BA_1_n9727 combined dataframe
BA_1_n9727.homo.df <- data.frame(
  index = rep(seq(1:12), rep(length(BA_1_n9727.homo.geo), 12)),
  vals = c(
    BA_1_n9727.homo.geo,
    BA_1_n9727.homo.popori,
    BA_1_n9727.homo.popdes,
    BA_1_n9727.homo.mob,
    BA_1_n9727.homo.comoverori,
    BA_1_n9727.homo.comoverdes,
    BA_1_n9727.homo.Ldnori,
    BA_1_n9727.homo.Ldndes,
    BA_1_n9727.homo.peakori,
    BA_1_n9727.homo.peakdes,
    BA_1_n9727.homo.resori,
    BA_1_n9727.homo.resdes
  )
)
BA_1_n9727.homo.df <- BA_1_n9727.homo.df %>%
  mutate(
    color = ifelse(index == 1, colors[1],
                   ifelse(index >= 2 & index <= 3, colors[2],
                          ifelse(index >= 4 & index <= 6, colors[3],
                                 ifelse(index >= 7 & index <= 8, colors[4],
                                        ifelse(index >= 9 & index <= 10, colors[5], colors[6]))))),
    index = index*2,
  )

## BA_1_15_n2975 combined dataframe
BA_1_15_n2975.homo.df <- data.frame(
  index = rep(seq(1:12), rep(length(BA_1_15_n2975.homo.geo), 12)),
  vals = c(
    BA_1_15_n2975.homo.geo,
    BA_1_15_n2975.homo.popori,
    BA_1_15_n2975.homo.popdes,
    BA_1_15_n2975.homo.mob,
    BA_1_15_n2975.homo.comoverori,
    BA_1_15_n2975.homo.comoverdes,
    BA_1_15_n2975.homo.Ldnori,
    BA_1_15_n2975.homo.Ldndes,
    BA_1_15_n2975.homo.peakori,
    BA_1_15_n2975.homo.peakdes,
    BA_1_15_n2975.homo.resori,
    BA_1_15_n2975.homo.resdes
  )
)
BA_1_15_n2975.homo.df <- BA_1_15_n2975.homo.df %>%
  mutate(
    color = ifelse(index == 1, colors[1],
                   ifelse(index >= 2 & index <= 3, colors[2],
                          ifelse(index >= 4 & index <= 6, colors[3],
                                 ifelse(index >= 7 & index <= 8, colors[4],
                                        ifelse(index >= 9 & index <= 10, colors[5], colors[6]))))),
    index = index*2,
  )

## combined-TL combined dataframe
combined.homo.df <- data.frame(
  index = rep(seq(1:12), rep(length(combined.homo.geo), 12)),
  vals = c(
    combined.homo.geo,
    combined.homo.popori,
    combined.homo.popdes,
    combined.homo.mob,
    combined.homo.comoverori,
    combined.homo.comoverdes,
    combined.homo.Ldnori,
    combined.homo.Ldndes,
    combined.homo.peakori,
    combined.homo.peakdes,
    combined.homo.resori,
    combined.homo.resdes
  )
)
combined.homo.df <- combined.homo.df %>%
  mutate(
    color = ifelse(index == 1, colors[1],
                   ifelse(index >= 2 & index <= 3, colors[2],
                          ifelse(index >= 4 & index <= 6, colors[3],
                                 ifelse(index >= 7 & index <= 8, colors[4],
                                        ifelse(index >= 9 & index <= 10, colors[5], colors[6]))))),
    index = index*2,
  )

## make plots
hdi_95 <- function(x) {
  r_95 <- hdi(x, credMass=0.95)
  r_50 <- hdi(x, credMass=0.5)
  r_m <- median(x)
  r <- c(r_95[1], r_50[1], r_m, r_50[2], r_95[2])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ymax <- max(c(
  BA_1_17.homo.df$vals,
  BA_1_n9727.homo.df$vals,
  BA_1_15_n2975.homo.df$vals,
  combined.homo.df$vals
)) + 0.1
ymin <- min(c(
  BA_1_17.homo.df$vals,
  BA_1_n9727.homo.df$vals,
  BA_1_15_n2975.homo.df$vals,
  combined.homo.df$vals
)) - 0.1
stripes <- data.frame(
  xmin = c(1, 5, 9, 13, 17, 21, 25),
  xmax = c(3, 7, 11, 15, 19, 23, 26),
  ymin = ymin,
  ymax = ymax
)

## BA_1_17.combined.df
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  stat_summary(data=BA_1_17.homo.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_x_continuous(limits=c(0, 26),
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
  scale_alpha_manual(values=c(1, 0)) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(face=1, size=15, vjust=1., hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=13, color='black')
  )

## output plot to file
ggsave('./BA_1_17.homo_v1.pdf',
       device='pdf', width=8, height=5.5)

## BA_1_n9727.combined.df
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  stat_summary(data=BA_1_n9727.homo.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_x_continuous(limits=c(0, 26),
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
  scale_alpha_manual(values=c(1, 0)) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(face=1, size=15, vjust=1., hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=13, color='black')
  )

## output plot to file
ggsave('./BA_1_n9727.homo_v1.pdf',
       device='pdf', width=8, height=5.5)

## BA_1_15_n2975.combined.df
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  stat_summary(data=BA_1_15_n2975.homo.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_x_continuous(limits=c(0, 26),
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
  scale_alpha_manual(values=c(1, 0)) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(face=1, size=15, vjust=1., hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=13, color='black')
  )

## output plot to file
ggsave('./BA_1_15_n2975.homo_v1.pdf',
       device='pdf', width=8, height=5.5)

## combined.df
ggplot() +
  geom_rect(data=stripes, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill='gray80', alpha=0.4) +
  stat_summary(data=combined.homo.df, aes(x=index, y=vals, group=index, color=color, fill=color),
               width=0.7, size=0.5, outlier.shape=NA, geom='boxplot',
               fun.data=hdi_95) +
  scale_x_continuous(limits=c(0, 26),
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
  scale_alpha_manual(values=c(1, 0)) +
  geom_hline(yintercept=0, linetype='solid', color='black', size=0.5, alpha=0.3) +
  labs(x='', y='Coefficient * Inclusion probability') +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_rect(color='black', size=0.4, fill=NA),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(face=1, size=15, vjust=1., hjust=1, color='black', angle=50),
    axis.text.y = element_text(face=1, size=13, color='black')
  )

## output plot to file
ggsave('./combined.homo_v1.pdf',
       device='pdf', width=8, height=5.5)

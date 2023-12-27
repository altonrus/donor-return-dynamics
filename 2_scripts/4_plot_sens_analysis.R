library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyverse)
library(ggforestplot)
library(ggforce)
library(cowplot)
library(gridExtra)

t_dat <- read_csv("../3_output/coefs_fm_sa_sens.csv") |>
  data.table()

t_dat[, covariate := factor(covariate, levels = t_dat$covariate)]

# Wide to long transformation
t_dat <- melt(t_dat, id.vars = c("covariate"), variable.name = "concat")

t_dat[, c("Measure", "Time", "Fixed", "DonType") := tstrsplit(as.character(concat), '_')]
#t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]

t_dat[, Time := fifelse(Time=="pre", "Pre-COVID19", "Intra-COVID19")]
t_dat[, Fixed := fifelse(Fixed=="f", "Fixed",'Mobile')]
t_dat[, DonType := fifelse(DonType=="a", "Only Whole Blood Returns",'Returns of all Donation Types')]

t_dat <- dcast(t_dat, covariate + Time + Fixed + DonType~ Measure, value.var = "value")
t_dat$sig <- ifelse((t_dat$`exp(coef)lower` <= 1 & t_dat$`exp(coef)upper` >= 1), FALSE, TRUE)

#grouping covariates
t_dat$group<- ifelse(grepl("After", t_dat$covariate), "Donation Outcomes", NA)
t_dat$group<- ifelse(grepl("Drives", t_dat$covariate), "Mobile Drives", t_dat$group)
t_dat$group[is.na(t_dat$group)] <- "Donor Characteristics"

#create diff datasets for diff groups

d1<- subset(t_dat, group=='Donor Characteristics')
d2<- subset(t_dat, group=='Donation Outcomes')
d3<- subset(t_dat, group=='Mobile Drives')

#d1

#d1
d1$cov_group<- ifelse(grepl("vs O+", d1$covariate), "Blood type", NA)
d1$cov_group<- ifelse(grepl("vs White", d1$covariate), "race", d1$cov_group)
d1$cov_group[is.na(d1$cov_group)] <- "Other"

d1 <- d1 %>%
  group_by(covariate, cov_group) %>%
  mutate(mean_hr = mean(`exp(coef)`)) %>%
  ungroup() %>%
  arrange(cov_group, mean_hr) %>%
  mutate(covariate = factor(covariate, levels = unique(covariate)))
#arrow 
d1$OR_u <- ifelse(d1$`exp(coef)upper` > 1.55, 1.55, NA_real_) # for the limit x=1.5

#d2
mean_hr <- d2[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d2$covariate <- factor(d2$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

#d3
mean_hr <- d3[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d3$covariate <- factor(d3$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

g1<-ggplot(data = d1, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`DonType`, alpha=sig))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_text(
    size = 16), axis.text.y = element_text(size = 14, color = "black"), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    axis.text.x = element_text(size = 12, color = "black"))+
  coord_cartesian(xlim = c(0.5, 1.5))+
  ggtitle("Donor Characteristics")+
  geom_segment(aes(x =`exp(coef)`, xend = OR_u, y = covariate, yend = covariate), arrow = arrow(length = unit(0.3, "cm")))



g2<-ggplot(data = d2, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`DonType`,alpha=sig))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_blank(), 
        axis.text.y = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.5, 1.5))+
  ggtitle("Donation Outcomes")

g3<-ggplot(data = d3, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=DonType))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "bottom", legend.spacing.x = unit(1, 'cm'), legend.title=element_blank(), legend.text = element_text(size = 14, color = "black"),
        strip.text = element_blank(), axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.5, 2.2))+
  ggtitle("Mobile Drives")

grid_plot<-plot_grid( g1, g2, g3, ncol=1, rel_heights= c(0.7, 0.2, 0.25), 
                      scale = c(0.97, .97, .97), align='v')

grid_plot + theme(panel.background = element_blank())


ggsave("../4_figures/SA_COX_sens.png",
       width = 16,
       height = 12,
       unit="in", bg='#ffffff')

######################################################US######################################################
t_dat <- read_csv("../3_output/coefs_fm_us_sens.csv") |>
  data.table()

t_dat[, covariate := factor(covariate, levels = t_dat$covariate)]

# Wide to long transformation
t_dat <- melt(t_dat, id.vars = c("covariate"), variable.name = "concat")

t_dat[, c("Measure", "Time", "Fixed", "DonType") := tstrsplit(as.character(concat), '_')]
#t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]

t_dat[, Time := fifelse(Time=="pre", "Pre-COVID19", "Intra-COVID19")]
t_dat[, Fixed := fifelse(Fixed=="f", "Fixed",'Mobile')]
t_dat[, DonType := fifelse(DonType=="a", "Only Whole Blood Returns",'Returns of all Donation Types')]

t_dat <- dcast(t_dat, covariate + Time + Fixed + DonType~ Measure, value.var = "value")
t_dat$sig <- ifelse((t_dat$`exp(coef)lower` <= 1 & t_dat$`exp(coef)upper` >= 1), FALSE, TRUE)
#grouping covariates
t_dat$group<- ifelse(grepl("After", t_dat$covariate), "Donation Outcomes", NA)
t_dat$group<- ifelse(grepl("Drives", t_dat$covariate), "Mobile Drives", t_dat$group)
t_dat$group[is.na(t_dat$group)] <- "Donor Characteristics"

#create diff datasets for diff groups

d1<- subset(t_dat, group=='Donor Characteristics')
d2<- subset(t_dat, group=='Donation Outcomes')
d3<- subset(t_dat, group=='Mobile Drives')

#d1

#d1
d1$cov_group<- ifelse(grepl("vs O+", d1$covariate), "Blood type", NA)
d1$cov_group<- ifelse(grepl("vs White", d1$covariate), "race", d1$cov_group)
d1$cov_group[is.na(d1$cov_group)] <- "Other"

d1 <- d1 %>%
  group_by(covariate, cov_group) %>%
  mutate(mean_hr = mean(`exp(coef)`)) %>%
  ungroup() %>%
  arrange(cov_group, mean_hr) %>%
  mutate(covariate = factor(covariate, levels = unique(covariate)))
#arrow 
d1$OR_u <- ifelse(d1$`exp(coef)upper` > 1.55, 1.55, NA_real_) # for the limit x=1.5

#d2
mean_hr <- d2[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d2$covariate <- factor(d2$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

#d3
mean_hr <- d3[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d3$covariate <- factor(d3$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

g1<-ggplot(data = d1, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`DonType`, alpha=sig))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=0.75)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_text(
    size = 16), axis.text.y = element_text(size = 14, color = "black"), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    axis.text.x = element_text(size = 12, color = "black"))+
  coord_cartesian(xlim = c(0.35, 1.4))+
  ggtitle("Donor Characteristics")

g2<-ggplot(data = d2, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`DonType`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=0.75)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  #scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_blank(), 
        axis.text.y = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.35, 1.4))+
  ggtitle("Donation Outcomes")

g3<-ggplot(data = d3, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`DonType`, alpha=sig))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=0.75)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "bottom", 
        legend.spacing.x = unit(1.0, 'cm'),
        legend.title=element_blank(), 
        legend.text = element_text(size = 14, color = "black"),
        strip.text = element_blank(), axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.7, 1.8))+
  ggtitle("Mobile Drives")

grid_plot<-plot_grid( g1, g2, g3, ncol=1, rel_heights= c(0.90, 0.18, 0.28), 
                      scale = c(0.96, .965, .965), align='v')

grid_plot + theme(panel.background = element_blank())

ggsave("../4_figures/US_COX_sens.png",
       width = 16,
       height = 12,
       unit="in", bg='#ffffff')





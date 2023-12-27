library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyverse)
library(ggforestplot)
library(ggforce)
library(cowplot)
library(gridExtra)

t_dat <- read_csv("../3_output/coefs_fm_sa.csv") |>
  data.table()

t_dat[, covariate := factor(covariate, levels = t_dat$covariate)]

# Wide to long transformation
t_dat <- melt(t_dat, id.vars = c("covariate"), variable.name = "concat")

t_dat[, c("Measure", "Time", "Fixed", "Adjusted") := tstrsplit(as.character(concat), '_')]
#t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]

t_dat[, Time := fifelse(Time=="pre", "Pre-COVID19", "Intra-COVID19")]
t_dat[, Fixed := fifelse(Fixed=="f", "Fixed",'Mobile')]
t_dat[, Adjusted := fifelse(Adjusted=="a", "Adjusted",'Unadjusted')]
t_dat <- dcast(t_dat, covariate + Time + Fixed + Adjusted ~ Measure, value.var = "value")

t_dat$sig <- ifelse((t_dat$`exp(coef)lower` <= 1 & t_dat$`exp(coef)upper` >= 1), FALSE, TRUE)

#grouping covariates
t_dat$group<- ifelse(grepl("After", t_dat$covariate), "Donation Outcomes", NA)
t_dat$group<- ifelse(grepl("Drives", t_dat$covariate), "Mobile Drives", t_dat$group)
t_dat$group[is.na(t_dat$group)] <- "Donor Characteristics"

#create diff datasets for diff groups

d1s<- subset(t_dat, group=='Donor Characteristics')
d2s<- subset(t_dat, group=='Donation Outcomes')
d3s<- subset(t_dat, group=='Mobile Drives')

#d1

#d1
d1s$cov_group<- ifelse(grepl("vs O+", d1s$covariate), "Blood type", NA)
d1s$cov_group<- ifelse(grepl("vs White", d1s$covariate), "race", d1s$cov_group)
d1s$cov_group[is.na(d1s$cov_group)] <- "Other"

d1s <- d1s %>%
  group_by(covariate, cov_group) %>%
  mutate(mean_hr = mean(`exp(coef)`)) %>%
  ungroup() %>%
  arrange(cov_group, mean_hr) %>%
  mutate(covariate = factor(covariate, levels = unique(covariate)))
#arrow 
d1s$OR_u <- ifelse(d1s$`exp(coef)upper` > 1.5, 1.55, NA_real_) # for the limit x=1.5

#d2s
mean_hr <- d2s[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d2s$covariate <- factor(d2s$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

#d3
mean_hr <- d3s[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
d3s$covariate <- factor(d3s$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

g1s<-ggplot(data = d1s, 
            aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
                y = covariate, shape=`Adjusted`, alpha=sig))+
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
  coord_cartesian(xlim = c(0.5, 1.52))+
  ggtitle("Donor Characteristics")+
  geom_segment(aes(x =`exp(coef)`, xend = OR_u, y = covariate, yend = covariate), arrow = arrow(length = unit(0.3, "cm")))



g2s<-ggplot(data = d2s, 
            aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
                y = covariate, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  #scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_blank(), 
        axis.text.y = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.5, 1.51))+
  ggtitle("Donation Outcomes")

g3s<-ggplot(data = d3s, 
            aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
                y = covariate, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  #scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "bottom", 
        legend.spacing.x = unit(1.0, 'cm'),
        legend.title=element_blank(), 
        legend.text = element_text(size = 14, color = "black"),
        strip.text = element_blank(), axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.5, 2.2))+
  ggtitle("Mobile Drives")

grid_plot2<-plot_grid( g1s, g2s, g3s, ncol=1, rel_heights= c(0.8, 0.20, 0.28), 
                       scale = c(0.97, .97, .97), align='v')

grid_plot2 + theme(panel.background = element_blank())


ggsave("../4_figures/SA_COX_Unadjusted.png",
       width = 16,
       height = 12,
       unit="in", bg='#ffffff')

##############################################US############################################

t_dat2 <- read_csv("../3_output/coefs_fm_us.csv") |>
  data.table()

t_dat2[, covariate := factor(covariate, levels = t_dat2$covariate)]

# Wide to long transformation
t_dat2 <- melt(t_dat2, id.vars = c("covariate"), variable.name = "concat")

t_dat2[, c("Measure", "Time", "Fixed", "Adjusted") := tstrsplit(as.character(concat), '_')]

t_dat2[, Time := fifelse(Time=="pre", "Pre-COVID19", "Intra-COVID19")]
t_dat2[, Fixed := fifelse(Fixed=="f", "Fixed",'Mobile')]
t_dat2[, Adjusted := fifelse(Adjusted=="a", "Adjusted",'Unadjusted')]

t_dat2 <- dcast(t_dat2, covariate + Time + Fixed + Adjusted~ Measure, value.var = "value")

#grouping covariates
t_dat2$group<- ifelse(grepl("After", t_dat2$covariate), "Donation Outcomes", NA)
t_dat2$group<- ifelse(grepl("Drives", t_dat2$covariate), "Mobile Drives", t_dat2$group)
t_dat2$group[is.na(t_dat2$group)] <- "Donor Characteristics"

t_dat2$sig <- ifelse((t_dat2$`exp(coef)lower` <= 1 & t_dat2$`exp(coef)upper` >= 1), FALSE, TRUE)

#create diff datasets for diff groups

d1<- subset(t_dat2, group=='Donor Characteristics')
d2<- subset(t_dat2, group=='Donation Outcomes')
d3<- subset(t_dat2, group=='Mobile Drives')


#d1
d1$cov_group<- ifelse(grepl("vs O+", d1$covariate), "Blood type", NA)
d1$cov_group<- ifelse(grepl("vs White", d1$covariate), "race", d1$cov_group)
d1$cov_group<- ifelse(grepl("vs Post", d1$covariate), "Edu", d1$cov_group)
d1$cov_group[is.na(d1$cov_group)] <- "Other"

d1 <- d1 %>%
  group_by(covariate, cov_group) %>%
  mutate(mean_hr = mean(`exp(coef)`)) %>%
  ungroup() %>%
  arrange(cov_group, mean_hr) %>%
  mutate(covariate = factor(covariate, levels = unique(covariate)))

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
               y = covariate, shape=`Adjusted`, alpha=sig))+
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
  coord_cartesian(xlim = c(0.35, 1.4))+
  ggtitle("Donor Characteristics")

g2<-ggplot(data = d2, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
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
               y = covariate, shape=`Adjusted`, alpha=sig))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
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

ggsave("../4_figures/US_COX_unadjusted.png",
       width = 16,
       height = 12,
       unit="in", bg='#ffffff')

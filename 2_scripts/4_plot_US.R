library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyverse)
library(ggforestplot)
library(ggforce)
library(cowplot)
library(gridExtra)

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

#create diff datasets for diff groups

d1<- subset(t_dat2, group=='Donor Characteristics')
d2<- subset(t_dat2, group=='Donation Outcomes')
d3<- subset(t_dat2, group=='Mobile Drives')

#d1
mean_hr <- d1[`exp(coef)` != 0] %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))
# Reorder the levels of the covariate factor based on the mean HR
d1$covariate <- factor(d1$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])



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
               y = covariate, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  #scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "none", legend.title=element_blank(),strip.text = element_text(
    size = 16), axis.text.y = element_text(size = 16, color = "black"), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text.x = element_text(size = 10, color = "black"))+
  #scale_y_discrete(labels = function(y) str_wrap(y, width = 30))+
  coord_cartesian(xlim = c(0.35, 1.7))+
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
        axis.text.y = element_text(size = 16, color = "black"), 
        axis.text.x = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.35, 1.7))+
  ggtitle("Donation Outcomes")

g3<-ggplot(data = d3, 
           aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
               y = covariate, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(size=1)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  #scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="")+
  theme(legend.position = "bottom", legend.title=element_blank(), legend.text = element_text(size = 18, color = "black"),
        strip.text = element_blank(), axis.text.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0))+
  coord_cartesian(xlim = c(0.35, 1.7))+
  ggtitle("Mobile Drives")


grid_plot<-plot_grid( g1, g2, g3, ncol=1, rel_heights= c(0.9, 0.18, 0.25), 
                      scale = c(0.955, .96, .96), align='v')

grid_plot + theme(panel.background = element_blank())

ggsave("../4_figures/US_COX.png",
       width = 18,
       height = 14,
       unit="in", bg='#ffffff')

library(readxl)
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyverse)
library(ggforestplot)

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

t_dat <- dcast(t_dat, covariate + Time + Fixed + Adjusted~ Measure, value.var = "value")

#t_dat[, significant := fifelse('exp(coef)lower'> 1 | 'exp(coef)upper'<1, T, F) ]

mean_hr <- t_dat %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))

# Reorder the levels of the covariate factor based on the mean HR
t_dat$covariate <- factor(t_dat$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

t_dat$Group <- rep(1:(nrow(t_dat) / 8), each = 8)
t_dat$Binary_Group <- as.integer(t_dat$Group %% 2 == 1)

odd_numbers <- seq(1, 110, 8)

ggplot(data = t_dat[`exp(coef)` != 1], 
       aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
           y = covariate, color = `Time`, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(position = position_dodge(width=.7), size =2)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="Hazard Ratio")+
  theme(legend.position = "bottom", legend.title=element_blank(),strip.text = element_text(
    size = 20), axis.text = element_text(size = 18, color = "black"), legend.text = element_text(size = 18, color = "black"), plot.title = element_text(size = 20, face = "bold"))+
  coord_cartesian(xlim = c(0, 1.75))+
  annotate("text", label="Higher\nerror",x = 6, y = 6, color = "grey27")+
  annotate("text", label="Lower\nerror",x = -6, y = 6, color = "grey27")+
  ggtitle("Hazard Ratios: South Africa")
  

ggsave("../4_figures/SA_COX.png",
       width = 18,
       height = 14,
      unit="in")


##########################################################################################################################
t_dat2 <- read_csv("../3_output/coefs_fm_us.csv") |>
  data.table()

t_dat2[, covariate := factor(covariate, levels = t_dat2$covariate)]

# Wide to long transformation
t_dat2 <- melt(t_dat2, id.vars = c("covariate"), variable.name = "concat")

t_dat2[, c("Measure", "Time", "Fixed", "Adjusted") := tstrsplit(as.character(concat), '_')]
#t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]

t_dat2[, Time := fifelse(Time=="pre", "Pre-COVID19", "Intra-COVID19")]
t_dat2[, Fixed := fifelse(Fixed=="f", "Fixed",'Mobile')]
t_dat2[, Adjusted := fifelse(Adjusted=="a", "Adjusted",'Unadjusted')]

t_dat2 <- dcast(t_dat2, covariate + Time + Fixed + Adjusted~ Measure, value.var = "value")

#t_dat2[, significant := fifelse('exp(coef)lower'> 1 | 'exp(coef)upper'<1, T, F) ]

mean_hr <- t_dat2 %>%
  group_by(covariate) %>%
  summarize(mean_hr = mean(`exp(coef)`))

# Reorder the levels of the covariate factor based on the mean HR
t_dat2$covariate <- factor(t_dat2$covariate, levels = mean_hr$covariate[order(mean_hr$mean_hr)])

t_dat2$Group <- rep(1:(nrow(t_dat2) / 8), each = 8)
t_dat2$Binary_Group <- as.integer(t_dat2$Group %% 2 == 1)

odd_numbers <- seq(1, 110, 8)

ggplot(data = t_dat2[`exp(coef)` != 1], 
       aes(x = `exp(coef)`, xmin = `exp(coef)lower`, xmax = `exp(coef)upper`, 
           y = covariate, color = `Time`, shape=`Adjusted`))+
  facet_grid(cols = vars(Fixed, factor(Time, levels=c('Pre-COVID19', 'Intra-COVID19'))))+
  geom_pointrange(position = position_dodge(width=.7), size =2)+
  scale_shape_manual(values=c( 16, 1))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none') +
  labs(y="", x="Hazard Ratio")+
  theme(legend.position = "bottom", legend.title=element_blank(),strip.text = element_text(
    size = 20), axis.text = element_text(size = 18, color = "black"), legend.text = element_text(size = 18, color = "black"), plot.title = element_text(size = 20, face = "bold"))+
  coord_cartesian(xlim = c(0, 1.75))+
  annotate("text", label="Higher\nerror",x = 6, y = 6, color = "grey27")+
  annotate("text", label="Lower\nerror",x = -6, y = 6, color = "grey27")+
  ggtitle("Hazard Ratios: United States")


ggsave("../4_figures/US_COX.png",
       width = 18,
       height = 14,
       unit="in")




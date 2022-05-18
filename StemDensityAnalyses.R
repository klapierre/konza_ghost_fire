setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
theme_set(theme_bw())

sd<-read.csv("StemDensity_2014-2019.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

trts<-read.csv("GF_PlotList_Trts.csv") %>% 
  select(-plot_id) %>% 
  rename(Burn=Burn.Trt2)%>%
  select(-Burn.Trt)

sd2<-sd %>% 
  left_join(trts)

###analysis of total stems
total=sd2 %>% 
  group_by(Year, Watershed, Block, Plot, Litter, Nutrient, treatment, Burn) %>% 
  summarise(total=sum(stems))

fit <- lmer(total ~  Burn*Litter*Nutrient*Year,  random = ~1|Watershed/Block, data = total)
anova(fit)

#Just trt yrs
fit <- lmer(total ~  Burn.Trt2*Litter*Nutrient+(1|Block), data = subset(ALL, Year!=2014))
Anova(fit)
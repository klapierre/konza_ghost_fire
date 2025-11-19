library(tidyverse)
library(codyn)
library(stringr)
library(vegan)
library(emmeans)
theme_set(theme_bw(20))

setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\")

trts<-read.csv("Compiled data\\GF_PlotList_Trts.csv") %>% 
  separate(Plot, into = c('blockb', 'Plot'), sep = 1) %>% 
  mutate(Plot=as.numeric(Plot))

myc2014<-read.csv('GhostFire2014_Data\\Mycorr\\MycoCounts_GF_2014.csv') %>% 
  mutate(Year=2014) %>% 
  select(Year, Watershed, Block, Plot, Average) %>% 
  rename(Colonization=Average) 
myc2019<-read.csv('GhostFire2019_Data\\Mycorr\\GF_MycorrhizalCounts_2019.csv') %>% 
  separate(Sample, into=c('Block', 'Plot'), sep=1, remove = F) %>% 
  select(Year, Watershed, Block, Plot, Colonization) %>% 
  mutate(Plot=as.numeric(Plot)) 
myc2024<-read.csv('GhostFire2024_Data\\Mycorr\\GF_Mycorrhizal_2024.csv') %>% 
  separate(Sample, into=c('Block', 'Plot'), sep=1, remove = F) %>% 
  select(Year, Watershed, Block, Plot, Colonization) %>% 
  mutate(Plot=as.numeric(Plot)) 

alldat<-myc2014 %>% 
  bind_rows(myc2019, myc2024) %>% 
  group_by(Year, Watershed, Block, Plot) %>% 
  summarise(Colonization=mean(Colonization)) %>% 
  pivot_wider(names_from = 'Year', names_prefix = 'Y', values_from = 'Colonization', values_fill =NA) %>% 
  left_join(trts)

alldat3<-myc2014 %>% 
  bind_rows(myc2019, myc2024) %>% 
  group_by(Year, Watershed, Block, Plot) %>% 
  summarise(Colonization=mean(Colonization)) %>% 
  left_join(trts) %>% 
  group_by(Year, Burn.Trt, Litter, Nutrient) %>% 
  summarise(n=length(Colonization))


setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
theme_set(theme_bw(20))

trts<-read.csv("GF_PlotList_Trts.csv") 

light<-read.csv("Light_2014_2019.CSV")%>%
  filter(Season=="Early")%>%
  rename(plot=Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))
light2<-light %>% 
  left_join(trts)

l2014=light2 %>% 
  filter(Year==2014|Year==2015) %>% 
  rename(LightReduction=CanopyEffect) %>% 
  select(Year, Watershed, Block, plot, BurnFreq, Plot, LightReduction, Litter, Nutrient, treatment)

ltrtyrs<-light2 %>% 
  filter(Year!=2014&Year!=2015) %>% 
  mutate(LightReduction=ifelse(Litter=="A", CanopyEffect, ifelse(Litter=="P", LitterCanopyEffect, 999))) %>% 
  select(Year, Watershed, Block, plot, BurnFreq, Plot, LightReduction, Litter, Nutrient, treatment)

light3<-l2014 %>% 
  bind_rows(ltrtyrs)

anpp<-read.csv("ANPP_2014-2019.CSV") %>% 
  rename(plot=Plot) %>% 
  mutate(Plot=paste(Block, plot, sep="")) %>% 
  select(-plot)

sp<-read.csv("SpComp_2014-2019.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

sp2<-sp %>% 
  left_join(trts)

richeven<-community_structure(sp2, time.var="Year", replicate.var = "plot_id", abundance.var = "pcover")

sd<-read.csv("StemDensity_2014-2019.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

total=sd %>% 
  group_by(Year, Watershed, Block, Plot) %>% 
  summarise(stems=sum(stems))

#putting it all into one file.

data<-light3 %>% 
  left_join(trts) %>% 
  left_join(anpp) %>% 
  left_join(richeven) %>% 
  left_join(total) %>% 
  select(Year, Watershed, Plot, plot_id, Burn.Trt, Litter, Nutrient, LightReduction, stems, total, richness)

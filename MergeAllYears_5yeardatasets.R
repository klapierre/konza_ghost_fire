setwd("~/Dropbox/Ghost Fire/DATA")
setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Data\\")

library(tidyverse)
library(writexl)

##Lots of the data we read in add odd things to the first column, use this code in the read.csv line to not have that
# fileEncoding="UTF-8-BOM"

#read in treatment
trts<-read.csv('GF_PlotList.csv')

##Mycorrhizal data
myc2014<-read.csv('GhostFire2014_Data\\Mycorr\\MycoCounts_GF_2014.csv') %>% 
  mutate(Year=2014) %>% 
  select(Year, Watershed, Block, Plot, Average) %>% 
  mutate(Colonization=Average/100) |> 
  select(-Average)
myc2019<-read.csv('GhostFire2019_Data\\Mycorr\\GF_MycorrhizalCounts_2019.csv') %>% 
  separate(Sample, into=c('Block', 'Plot'), sep=1, remove = F) %>% 
  select(Year, Watershed, Block, Plot, Colonization) %>% 
  mutate(Plot=as.numeric(Plot)) 
myc2024<-read.csv('GhostFire2024_Data\\Mycorr\\GF_Mycorrhizal_2024.csv') %>% 
  separate(Sample, into=c('Block', 'Plot'), sep=1, remove = F) %>% 
  select(Year, Watershed, Block, Plot, Colonization) %>% 
  mutate(Plot=as.numeric(Plot)) 

mycAll<-myc2014 %>% 
  bind_rows(myc2019, myc2024) %>% 
  group_by(Year, Watershed, Block, Plot) %>% 
  summarise(Colonization=mean(Colonization)) %>% 
  left_join(trts) |> 
  mutate(LogCol=log(Colonization))

hist(log(mycAll$Colonization))

##we are missing a lot of data and some plots never have data
# alldat3<-myc2014 %>% 
#   bind_rows(myc2019, myc2024) %>% 
#   group_by(Year, Watershed, Block, Plot) %>% 
#   summarise(Colonization=mean(Colonization)) %>% 
#   left_join(trts) %>% 
#   group_by(Year, Burn.Trt, Litter, Nutrient) %>% 
#   summarise(n=length(Colonization))


write.csv(mycAll, "Compiled data/Myc_2014_2019_2024.csv", row.names = F)
write_xlsx(mycAll, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Myc_2014_2019_2024.csv.xlsx')


##Soil resins
#check for outliers when read in
resin2014<-read.csv('GhostFire2014_Data\\soil\\GhostFire_resin bags_2014_v2.csv') |> 
  rename(Year=year,
         Watershed=watershed,
         Block=block,
         Plot=plot) |> 
  select(Year, Watershed, Block, Plot, nitrate, ammonium) |> 
  group_by(Year, Watershed, Block, Plot) |> 
  summarize_all(mean) |> 
  left_join(trts) |> 
  mutate(logNit=log(nitrate))

resin2014<-read.csv('GhostFire2014_Data\\soil\\GhostFire_resin bags_2014_v2.csv') |> 
  rename(Year=year,
         Watershed=watershed,
         Block=block,
         Plot=plot) |> 
  select(Year, Watershed, Block, Plot, nitrate, ammonium) |> 
  group_by(Year, Watershed, Block, Plot) |> 
  summarize_all(mean) |> 
  left_join(trts) |> 
  mutate(logNit=log(nitrate))


write.csv(mycAll, "Compiled data/Myc_2014_2019_2024.csv", row.names = F)
write_xlsx(mycAll, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Myc_2014_2019_2024.csv.xlsx')


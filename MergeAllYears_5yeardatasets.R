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
  left_join(trts) 

resin2019_raw<-read.csv('GhostFire2019_Data\\Resins\\GF_N_compiled_raw.csv') |> 
  filter(problem==0) |> #this drops negative values and one that need to be diluted
  mutate(dil2=ifelse(dil==0, 1, dil), 
         ppm=conc*dil2)

#duplicates - not sure what is going on here, looks like samples were run multiple times over serveral days. Just going to average them.
resin2019<-resin2019_raw |> 
  select(sample, Ntype, ppm) |> 
  group_by(sample, Ntype) |> 
  summarize(n=length(ppm))

resin2019<-resin2019_raw |> 
  select(sample, Ntype, ppm) |> 
  group_by(sample, Ntype) |> 
  summarize(mppm=mean(ppm)) |> 
  pivot_wider(names_from = 'Ntype', values_from = 'mppm') |> 
  rename(nitrate=`KCL NO3_NO2 2`,
         ammonium = `KCl Ammonia 10`)|> 
  separate(sample, into=c('Watershed', 'Block', 'plotrep'), sep = " ") |> 
  separate(plotrep, into=c('Plot', 'rep'), sep = "-") |> 
  mutate(Year=2019,
         Block=toupper(Block),
         Watershed=ifelse(Watershed=='ID', '1D', Watershed),
         Plot=as.integer(Plot)) |> 
    group_by(Year, Watershed, Block, Plot) |> 
  summarize_all(mean, na.rm=T) |> 
  left_join(trts) |> 
  select(-rep)


write.csv(mycAll, "Compiled data/Myc_2014_2019_2024.csv", row.names = F)
write_xlsx(mycAll, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Myc_2014_2019_2024.csv.xlsx')


######Standing root biomass
srb2014<-read.csv('GhostFire2014_Data\\Root standing crop\\GF_StandingCrop_2014_SKcleaned_v1.csv')



###BNPP


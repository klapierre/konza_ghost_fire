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
write_xlsx(mycAll, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Myc_2014_2019_2024.xlsx')


##Soil resins
#check for outliers when read in
resin2014<-read.csv('GhostFire2014_Data\\soil\\GhostFire_resin bags_2014_v2.csv') |> 
  rename(Year=year,
         Watershed=watershed,
         Block=block,
         Plot=plot) |> 
  select(Year, Watershed, Block, Plot, nitrate, ammonium) |> 
  group_by(Year, Watershed, Block, Plot) |> 
  summarize_all(mean) 

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
  select(-rep)

resin2024<-read.csv('GhostFire2024_Data\\Resins\\ghostfire_2024_nitrate_ammonia.csv') |> 
  select(year, watershed, block, plot, rep, ammonia_ppm, nitrate_ppm) |> 
  filter(watershed!="") |> 
  rename(Year=year,
         Watershed=watershed,
         Block=block,
         Plot=plot,
         ammonium=ammonia_ppm,
         nitrate=nitrate_ppm) |> 
  group_by(Year, Watershed, Block, Plot) |> 
  summarize_all(mean, rm.na=T) |> 
  mutate(Plot=as.integer(Plot)) |> 
  select(-rep) |> 
  mutate(Watershed=ifelse(Watershed=='SPB', 'SpB', Watershed))


resinall<-resin2014 |> 
  bind_rows(resin2019, resin2024) |> 
  left_join(trts) |> 
  mutate(logNit=log(nitrate),
         logAmm=log(ammonium))

hist(log(resinall$ammonium))
hist(log(resinall$nitrate))

write.csv(resinall, "Compiled data/Resin_2014_2019_2024.csv", row.names = F)
write_xlsx(resinall, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Resin_2014_2019_2024.xlsx')


######Standing root biomass
srb2014<-read.csv('GhostFire2014_Data\\Root standing crop\\GF_StandingCrop_2014_SKcleaned_v1.csv') |> 
  select(Year, Watershed, Block, Plot, Dry.mass..g., AFDM) |> 
  rename(drymass=Dry.mass..g.)

srb2019<-read.csv('GhostFire2019_Data\\standing roots biomass\\GF_Biomass.csv') |> 
  mutate(drymass=Dead.roots+Live.roots) |> 
  select(-Dead.roots, -SOM, -Live.roots, -X, -X.1)

srb2024<-read.csv('GhostFire2024_Data\\Roots\\GhostFire_standingroot_2024.csv') |> 
  select(Year, Watershed, Block, Plot, Dry_mass, AFDM) |> 
  rename(drymass=Dry_mass)

allSRC<-srb2014 |> 
  bind_rows(srb2019, srb2024)


summary(lm(AFDM~drymass, data=allSRC))
# m = 0.60423
# b = 0.04203
# r2= 0.985

plot(allSRC$drymass, allSRC$AFDM)

allSRC2<-allSRC |> 
  mutate(AFDM2=ifelse(is.na(AFDM), 0.60423*drymass+0.04203, AFDM)) |> 
  mutate(SRC=AFDM2/0.001963) |> 
  select(-AFDM, -drymass, -AFDM2) |> 
  left_join(trts) |> 
  mutate(logSRC=log(SRC))

hist(log(allSRC2$SRC))

write.csv(allSRC2, "Compiled data/SRC_2014_2019_2024.csv", row.names = F)
write_xlsx(allSRC2, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\SRC_2014_2019_2024.xlsx')


###BNPP

bnpp2014<-read.csv('GhostFire2014_Data\\BNPP\\GhostFire_BNPP_2014_v2.csv') |> 
  select(Year, Watershed, Block, Plot, Rep, DryMass, AFDM) |> 
  rename(Dry_mass=DryMass)

bnpp2019<-read.csv('GhostFire2019_Data\\Roots\\GhostFire BNPP_2019.csv') |> 
  select(Year, Watershed, Block, Plot, Rep, Dry_mass, AFDM) |> 
  mutate(Rep=ifelse(Rep=='A', 1, 2))

bnpp2024<-read.csv('GhostFire2024_Data\\Roots\\GhostFire_BNPP_2024.csv') |> 
  select(Year, Watershed, Block, Plot, Rep, Dry_mass, AFDM) 

bnappall<-bnpp2014 |> 
  bind_rows(bnpp2019, bnpp2024)

summary(lm(AFDM~Dry_mass, data=bnappall))
# m = 0.60423
# b = 0.04203
# r2= 0.985

plot(bnappall$Dry_mass, bnappall$AFDM)

bnappall2<-bnappall |> 
  #mutate(AFDM2=ifelse(is.na(AFDM), 0.60423*drymass+0.04203, AFDM)) |> 
  select(-Dry_mass) |> 
  filter(!is.na(AFDM)) |> 
  group_by(Year, Watershed, Block, Plot) |> 
  summarise(mAFDM=mean(AFDM, na.rm=T)) |> 
  left_join(trts) |> 
  mutate(logBNPP=log(mAFDM)) 
  
hist(log(bnappall2$mAFDM))
  
write.csv(bnappall2, "Compiled data/BNPP_2014_2019_2024.csv", row.names = F)
write_xlsx(bnappall2, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\BNPP_2014_2019_2024.xlsx')
  


####inverts
invertcom<-read.csv('Compiled data\\invertMetricsSAS.csv') |> 
  select(Year, Watershed, Block, plot, richness, Evar, abundance, ln_abund) |> 
  rename(Plot=plot) |> 
  left_join(trts)

invertbio<-read.csv('Compiled data\\GF_invertBiomass.csv') |> 
  select(-burn_trt, -month) |> 
  rename(Year=year,
         Watershed=watershed,
         Block=block,
         Plot=plot) |> 
  left_join(trts)

write_xlsx(invertcom, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\InvertDiv2014_2019_2024.xlsx')
write_xlsx(invertbio, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\InvertBio2014_2019_2024.xlsx')


###soil and microbial enzymes
soil<-read.csv('SoilExtracellularEnzymeData\\GhostFire_Y0Y1Y4Y10_SoilEnzymeData.csv') |> 
  filter(XptYear %in% c('Y0', 'Y4', 'Y10')) |> 
  select(-GFCode, -SAMPLE, -Month.Year, -BurnTrt, -LitterTrt, -Ntrt, -BurnLitter, -XptTimePt, -XptTimePt_BurnTrt, -VectorX, -VectorY, -VectorLength, -VectorAngle, -pH, -POXC) |> 
  mutate(Year=case_when(
    XptYear=='Y0' ~ 2014,
    XptYear=='Y4' ~ 2018,
    XptYear=='Y10' ~2024,
    .default = 999)) |> 
  select(-XptYear) |> 
  left_join(trts)

write.csv(soil, "Compiled data/SoilEnzymeProp_2014_2018_2024.csv", row.names = F)
write_xlsx(soil, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\SoilEnzymeProp_2014_2018_2024.xlsx')

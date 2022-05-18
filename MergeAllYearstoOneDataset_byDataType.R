setwd("~/Dropbox/Ghost Fire/DATA")
setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Data")
library(tidyverse)

######################################################################################
######################################################################################
######################################################################################
####import all DPM data (We are missing data from 2014 and 2017), calculate mean across four pseudo replicates
DPM2015<-read.csv("GhostFire2015_Data/DiscPasture/GhostFire_Disc Pasture_2015.csv")%>%
  select(-disc)%>%
  gather(quad, height, Quad.A:Quad.D)%>%
  group_by(Year, Wateshed, Plot, Rep) %>% 
  summarise(meanheight=mean(height, na.rm=T))%>%
  filter(Year!="NA")
DPM2016<-read.csv("GhostFire2016_Data/DiscPasture/GhostFire_Disc Pasture_2016.csv")%>%
  select(-disc)%>%
  gather(quad, height, Quad.A:Quad.D)%>%
  group_by(Year, Wateshed, Plot, Rep) %>% 
  summarise(meanheight=mean(height, na.rm=T))%>%
  filter(Year!="NA")
DPM2018<-read.csv("GhostFire2018_Data/DiscPasture/GhostFire_Disc Pasture_2018.csv")%>%
  gather(quad, height, Quad.A:Quad.D)%>%
  group_by(Year, Wateshed, Plot, Rep) %>% 
  summarise(meanheight=mean(height, na.rm=T))%>%
  filter(Year!="NA")

#merge all years of DPM together with column names Year, Wateshed, Plot, Rep, meanheight
#not watershe is spelled wrong
#removed Rep because they were all a 1? Not sure what this column meant
DPM_AllYears<-bind_rows(DPM2015, DPM2016, DPM2018)%>%
  select(-Rep)
#write.csv(DPM_AllYears, "DPM_2015_2016_2018.csv", row.names = F)

######################################################################################
######################################################################################
######################################################################################
#import all light data, calculate mean across four pseudo replicates
Light2014Early<-read.csv("GhostFire2014_Data/Light/GhostFire_Light_May2014.csv")%>%
  select(-B_Average, -Light)%>%
  filter(Block!="x") %>% 
  gather(Rep, Below, Below.1:Below.2)%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  summarise(meanBelow=mean(Below, na.rm=T))%>%
  mutate(CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>% 
  mutate(LitterCanopyEffect=NA) %>% 
  mutate(meanBelowLitter=NA) %>% 
  ungroup() %>% 
  mutate(Plot=as.integer(Plot))%>%
  rename(burn=BurnFreq) %>% 
  mutate(BurnFreq=ifelse(burn=="Annual", 1, 20)) %>% 
  mutate(BurnFreq=as.factor(BurnFreq))%>%
  select(-burn)

Light2014Late<-read.csv("GhostFire2014_Data/Light/GhostFire_Light_August2014.csv")%>%
  select(-B_average, -Light)%>%
  filter(Block!="x") %>% 
  gather(Rep, Below, Below.1:Below.2)%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  summarise(meanBelow=mean(Below, na.rm=T))%>%
  mutate(CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Late")%>% 
  mutate(LitterCanopyEffect=NA) %>% 
  mutate(meanBelowLitter=NA)%>%
  rename(burn=BurnFreq) %>% 
  mutate(BurnFreq=ifelse(burn=="Annual", 1, 20)) %>% 
  ungroup() %>% 
  mutate(BurnFreq=as.factor(BurnFreq))%>%
  select(-burn)

Light2015<-read.csv("GhostFire2015_Data/Light/GhostFire_Light_June2015.csv")%>%
  select(-B_Average, -Light)%>%
  filter(Block!="x") %>% 
  gather(Rep, Below, Below.1:Below.2)%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  summarise(meanBelow=mean(Below, na.rm=T))%>%
  mutate(CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early") %>% 
  mutate(LitterCanopyEffect=NA) %>% 
  mutate(meanBelowLitter=NA)%>%
  mutate(BurnFreq=as.factor(BurnFreq))

Light2016<-read.csv("GhostFire2016_Data/Light/GhostFire_Light_June2016.csv")%>%
  select(-B_Average, -Light, -AboveLitter_Average)%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.1, Below.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.1, -Below.2, -Above.Litter.1, -Above.Litter.2) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))

Light2017<-read.csv("GhostFire2017_Data/Light/GhostFire_Light_May2017.csv")%>%
  select(-B_Average, -Light, -AboveLitter_Average)%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.1, Below.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.1, -Below.2, -Above.Litter.1, -Above.Litter.2) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))

Light2018<-read.csv("GhostFire2018_Data/Light/GhostFire_Light_June2018.csv")%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.1, Below.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.1, -Below.2, -Above.Litter.1, -Above.Litter.2) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))

Light2019<-read.csv("GhostFire2019_Data/Light/GhostFire_Light_2019.csv")%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.Litter.1, Below.Litter.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.Litter.1, -Below.Litter.2, -Above.Litter.1, -Above.Litter.2) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))%>%
  select(-Month)
#str(Light2018)

#merge all years of Light together with column names Year, BurnFreq, Watershed, Block, Plot, Above, meanBelowLitter, meanBelow, LitterCanopyEffect, CanopyEffect
#note watershes is spelled correct here
#note 2014 has both early and late season data

Light_AllYears<-bind_rows(Light2014Early, Light2014Late, Light2015, Light2016, Light2017, Light2018, Light2019)
write.csv(Light_AllYears, "Compiled data\\Light_2014_2019.csv", row.names = F)


######################################################################################
######################################################################################
######################################################################################
####import all Species Comp data
SC2014<-read.csv("GhostFire2014_Data/Species Comp/GhostFire_SpComp_2014.csv")%>%
  select(-Experiment, -Site) %>% 
  group_by(ï..Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2015<-read.csv("GhostFire2015_Data/SpeciesComp/GhostFire_SpComp_2015.csv")%>%
  select(-Experiment, -Site, -ï..Entry, -Max.cov, -X.1, -X.2, -X.3, -X.4, -X.5, -X) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2016<-read.csv("GhostFire2016_Data/SpeciesComp/GhostFire_SpComp_2016.csv")%>%
  select(-Experiment, -Site, -ï..Entry, -Max.cov, -X.1, -X.2, -X.3, -X.4, -X.5, -X) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2017<-read.csv("GhostFire2017_Data/SpeciesComp/GhostFire_SpComp_2017.csv")%>%
  select(-Experiment, -Site, -ï..Entry, -Max.cov, -X.1, -X.2, -X, -Comments) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2018<-read.csv("GhostFire2018_Data/SpeciesComp/GhostFire_SpComp_2018.csv")%>%
  select(-Experiment, -Site, -X, -Comments) %>% 
  group_by(ï..Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2019<-read.csv("GhostFire2010_Data/SpeciesComp/GhostFire_SpComp_2019.csv")%>%
  select(-Experiment, -Site, -X, -Comments) %>% 
  group_by(ï..Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

#merge all years of SpComp together with column names Year, Burn.Trt, Block, Plot, spnum, Species, Season, cover

SpComp_AllYears<-bind_rows(SC2014, SC2015, SC2016, SC2017, SC2018)
# bring in sp list
SpList<-read.csv("~/Dropbox/Ghost Fire/Data/GhostFire_Konza_spplist.csv")
SpComp_AllYears2<-SpComp_AllYears %>% 
  right_join(SpList) %>% 
  filter(Year!="NA")
### Check to make sure species names, numbers, and cleaned names match up. 
#SK checked 2014-2018 and found multiple errors (>15)
#SK cleaned sp comp and sp list through 2018 on May 21, 2019
write.csv(SpComp_AllYears2, "SpComp_2014-2018.csv", row.names = F)





######################################################################################
######################################################################################
######################################################################################
####import all Stem Density data
SD2014<-read.csv("GhostFire2014_Data/Stem Density/GhostFire_SpringStemD_2014.csv")%>%
  rename(Year=ï..Year)%>%
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p_1:p_6)%>%
  separate(Plot, into=c("letter", "plot"), sep="_")%>%
  mutate(Plot=paste(letter, plot, sep="")) %>% 
  select(-letter, -plot)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")%>%
  rename(burn=Burn)%>%
  mutate(Burn=ifelse(burn=="Unburned", 20, 1))%>%
  ungroup() %>% 
  select(-burn)


SD2015<-read.csv("GhostFire2015_Data/StemDensity/GhostFire_SpringStemD_2015.csv")%>%
  select(-ï..Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0) %>% 
  filter(Year!="NA")
SD2016<-read.csv("GhostFire2016_Data/StemDensity/GhostFire_SpringStemD_2016_v2.csv")%>%
  select(-ï..Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2017<-read.csv("GhostFire2017_Data/StemDensity/GhostFire_SpringStemD_2017.csv")%>%
  select(-ï..Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2018<-read.csv("GhostFire2018_Data/StemDensity/GhostFire_SpringStemD_2018.csv")%>%
  select(-ï..Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2019<-read.csv("GhostFire2019_Data/StemDensity/GhostFire_SpringStemD_2019.csv")%>%
  select(-ï..Format.ID, -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")

#merge all years of StemDensity together with column names Year, Burn, Watershed, Block, Plot, spnum, Species, stems

StemDensity_AllYears<-bind_rows(SD2014, SD2015, SD2016, SD2017, SD2018, SD2019)
# bring in sp list
SpList<-read.csv("GhostFire_Konza_spplist.csv") %>% 
  rename(spnum=ï..spnum)

StemDensity_AllYears2<-StemDensity_AllYears %>% 
  right_join(SpList) %>% 
  filter(Year!="NA")
### Check to make sure species names, numbers, and cleaned names match up. 
#SK checked 2014-2018 and found multiple errors (>15)
#SK cleaned sp comp and sp list through 2018 on May 21, 2019
write.csv(StemDensity_AllYears2, "Compiled Data/StemDensity_2014-2019.csv", row.names = F)


######################################################################################
######################################################################################
######################################################################################
#import all biomass data, calculate mean across two pseudo replicates
ANPP2014<-read.csv("~/Dropbox/Ghost Fire/Data/GhostFire2014_Data/Biomass/GhostFire_Biomass_2014.csv")%>%
  select(-Notes)%>%
  mutate_at(c(7:10), ~replace(., is.na(.), 0))
ANPP2015<-read.csv("~/Dropbox/Ghost Fire/Data/GhostFire2015_Data/Biomass/GhostFire_Biomass_2015.csv")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot))
ANPP2016<-read.csv("~/Dropbox/Ghost Fire/Data/GhostFire2016_Data/Biomass/GhostFire_Biomass_2016_formatted.csv")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot))
#merge all years of StemDensity together with column names Year, Burn, Watershed, Block, Plot, spnum, Species, stems
ANPP_AllYears<-bind_rows(ANPP2014, ANPP2015, ANPP2016)
#write.csv(ANPP_AllYears, "ANPP_2014-2016.csv", row.names = F)


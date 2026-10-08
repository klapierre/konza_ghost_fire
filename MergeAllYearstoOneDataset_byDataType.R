setwd("~/Dropbox/Ghost Fire/DATA")
setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Data")

library(tidyverse)

##Lots of the data we read in add odd things to the first column, use this code in the read.csv line to not have that
# fileEncoding="UTF-8-BOM"

#read in treatment
trts<-read.csv('GF_PlotList.csv')

# bring in sp list
SpList<-read.csv("GhostFire_Konza_spplist.csv")|> 
  filter(spnum!='NA')

######################################################################################
######################################################################################
######################################################################################

#Not focusing in DMP b/c we have ANPP Oct 2026

####import all DPM data (We are missing data from 2014 and 2017), calculate mean across four pseudo replicates
# DPM2015<-read.csv("GhostFire2015_Data/DiscPasture/GhostFire_Disc Pasture_2015.csv")%>%
#   select(-disc)%>%
#   gather(quad, height, Quad.A:Quad.D)%>%
#   group_by(Year, Wateshed, Plot, Rep) %>% 
#   summarise(meanheight=mean(height, na.rm=T))%>%
#   filter(Year!="NA")
# DPM2016<-read.csv("GhostFire2016_Data/DiscPasture/GhostFire_Disc Pasture_2016.csv")%>%
#   select(-disc)%>%
#   gather(quad, height, Quad.A:Quad.D)%>%
#   group_by(Year, Wateshed, Plot, Rep) %>% 
#   summarise(meanheight=mean(height, na.rm=T))%>%
#   filter(Year!="NA")
# #No 2017 DPM Data
# DPM2018<-read.csv("GhostFire2018_Data/DiscPasture/GhostFire_Disc Pasture_2018.csv")%>%
#   gather(quad, height, Quad.A:Quad.D)%>%
#   group_by(Year, Wateshed, Plot, Rep) %>% 
#   summarise(meanheight=mean(height, na.rm=T))%>%
#   filter(Year!="NA")
# #No 2019 DPM Data
# #No 2020 DPM Data
# DPM2021<-read.csv("GhostFire2021_Data/DiscPasture/enterred/GhostFire_Disc Pasture_2021.csv")%>%
#   gather(quad, height, Quad.A:Quad.D)%>%
#   group_by(Year, Wateshed, Plot, Rep) %>% 
#   summarise(meanheight=mean(height, na.rm=T))%>%
#   filter(Year!="NA")
# DPM2022<-read.csv("GhostFire2022_Data/DiscPasture/GhostFire_Disc Pasture_2022.csv")%>%
#   mutate(Rep=1) %>% 
#   gather(quad, height, Quad.A:Quad.D)%>%
#   group_by(Year, Wateshed, Plot, Rep) %>% 
#   summarise(meanheight=mean(height, na.rm=T))%>%
#   filter(Year!="NA")
# 
# 
# #merge all years of DPM together with column names Year, Wateshed, Plot, Rep, meanheight
# #not watershe is spelled wrong
# #removed Rep because they were all a 1? Not sure what this column meant
# DPM_AllYears<-bind_rows(DPM2015, DPM2016, DPM2018, DPM2021, DPM2022)%>%
#   select(-Rep)
# write.csv(DPM_AllYears, "Compiled data/DPM_2015_2022.csv", row.names = F)

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

# Light2014Late<-read.csv("GhostFire2014_Data/Light/GhostFire_Light_August2014.csv")%>%
#   select(-B_average, -Light)%>%
#   filter(Block!="x") %>% 
#   gather(Rep, Below, Below.1:Below.2)%>%
#   group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
#   summarise(meanBelow=mean(Below, na.rm=T))%>%
#   mutate(CanopyEffect=((meanBelow/Above)*100))%>%
#   mutate(Season="Late")%>% 
#   mutate(LitterCanopyEffect=NA) %>% 
#   mutate(meanBelowLitter=NA)%>%
#   rename(burn=BurnFreq) %>% 
#   mutate(BurnFreq=ifelse(burn=="Annual", 1, 20)) %>% 
#   ungroup() %>% 
#   mutate(BurnFreq=as.factor(BurnFreq))%>%
#   select(-burn)

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
#We have 2016 light for middle and late season too, but this is JUST early season

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

#No Light2020
#No Light2021 but Sally should look for it in raw data sheets

Light2022<-read.csv("GhostFire2022_Data/Light/GhostFire_Light_2022.csv")%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.Litter.1, Below.Litter.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.Litter.1, -Below.Litter.2, -Above.Litter.1, -Above.Litter.2, -X) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))%>%
  select(-Month)

Light2023<-read.csv("GhostFire2023_Data/Light/GhostFire_Light_2023.csv")%>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, Above) %>% 
  mutate(meanBelowLitter=mean(c(Below.Litter.1, Below.Litter.2), na.rm=T), meanBelow=mean(c(Above.Litter.1, Above.Litter.2), na.rm=T))%>%
  select(-Below.Litter.1, -Below.Litter.2, -Above.Litter.1, -Above.Litter.2, -oldplot) %>% 
  mutate(LitterCanopyEffect=((meanBelowLitter/Above)*100), CanopyEffect=((meanBelow/Above)*100))%>%
  mutate(Season="Early")%>%
  mutate(BurnFreq=as.factor(BurnFreq))%>%
  select(-Month)


#merge all years of Light together with column names Year, BurnFreq, Watershed, Block, Plot, Above, meanBelowLitter, meanBelow, LitterCanopyEffect, CanopyEffect
#note watershes is spelled correct here
#note 2014 has both early and late season data

Light_AllYears<-bind_rows(Light2014Early, Light2015, Light2016, Light2017, Light2018, Light2019, Light2022, Light2023) |> 
  left_join(trts)

write.csv(Light_AllYears, "Compiled data/Light_2014_2023.csv", row.names = F)
write_xlsx(Light_AllYears, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\Light.xlsx')


######################################################################################
######################################################################################
######################################################################################
####import all Species Comp data
SC2014<-read.csv("GhostFire2014_Data/Species Comp/GhostFire_SpComp_2014.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Species) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2015<-read.csv("GhostFire2015_Data/SpeciesComp/GhostFire_SpComp_2015.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Entry, -Max.cov, -X.1, -X.2, -X.3, -X.4, -X.5, -X) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2016<-read.csv("GhostFire2016_Data/SpeciesComp/GhostFire_SpComp_2016.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Entry, -Max.cov, -X.1, -X.2, -X.3, -X.4, -X.5, -X) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum, Species) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2017<-read.csv("GhostFire2017_Data/SpeciesComp/GhostFire_SpComp_2017.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Entry, -Max.cov, -X.1, -X.2, -X, -Comments, -Species) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0) |> 
  left_join(trts, by=c('Block', 'Plot', 'Burn.Trt')) |> 
  select(-plot, -Litter, -Nutrient)

SC2018<-read.csv("GhostFire2018_Data/SpeciesComp/GhostFire_SpComp_2018.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -X, -Comments, -Species) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0) |> 
  left_join(trts, by=c('Block', 'Plot', 'Burn.Trt')) |> 
  select(-plot, -Litter, -Nutrient)


SC2019<-read.csv("GhostFire2019_Data/SpeciesComp/GhostFire_SpComp_2019.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Comments) %>% 
  filter(spnum!="litter") %>%# there was a lot of litter in 2019
  mutate(spnum=as.integer(spnum)) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0) |> 
  left_join(trts, by=c('Block', 'Plot', 'Burn.Trt')) |> 
  select(-plot, -Litter, -Nutrient)


#No SC2020

SC2021<-read.csv("GhostFire2021_Data/SpeciesComp/GhostFire_SpComp_2021.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Comments) %>% 
  filter(spnum!="litter") %>%# there was a lot of litter in 2019
  mutate(spnum=as.integer(spnum)) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0) |> 
  left_join(trts, by=c('Block', 'Plot', 'Burn.Trt')) |> 
  select(-plot, -Litter, -Nutrient)


SC2022<-read.csv("GhostFire2022_Data/SpeciesComp/GhostFire_SpComp_2022.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Comments) %>% 
  filter(spnum!="litter") %>%# there was a lot of litter in 2019
  mutate(spnum=as.integer(spnum)) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2023<-read.csv("GhostFire2023_Data/SpeciesComp/GhostFire_SpComp_2023.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Comments) %>% 
  filter(spnum!="litter") %>%# there was a lot of litter in 2019
  mutate(spnum=as.integer(spnum)) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)

SC2024<-read.csv("GhostFire2024_Data/SpeciesComp/GhostFire_SpComp_2024.csv",fileEncoding="UTF-8-BOM")%>%
  select(-Experiment, -Site, -Comments) %>% 
  filter(spnum!="litter") %>%# there was a lot of litter in 2019
  mutate(spnum=as.integer(spnum)) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  gather(Season, cover, June:August)%>%
  filter(cover!=0)


#merge all years of SpComp together with column names Year, Burn.Trt, Block, Plot, spnum, Species, Season, cover
#Now also getting the max cover at this dataset.

SpComp_AllYears<-bind_rows(SC2014, SC2015, SC2016, SC2017, SC2018, SC2019, SC2021, SC2022, SC2023, SC2024) %>% 
  ungroup() |> 
  select(-Species) %>% 
  group_by(Year, Burn.Trt, Block, Plot, spnum) %>% 
  summarize(pcover=max(cover)) |> 
  mutate(spnum2=case_when(
    spnum==112 ~ 111,
    spnum==80 ~ 300,
    spnum==253 ~ 228,
   .default= spnum
  )) |> 
  select(-spnum) |> 
  rename(spnum=spnum2)

ggplot(data=SpComp_AllYears, aes(x=pcover))+
  geom_histogram()+
  facet_wrap(~spnum, scales='free')
# 
# test<-SpComp_AllYears %>% 
#   left_join(SpList) |> 
#   left_join(trts) |> 
#   mutate(species=paste(genus, species, sep="_")) |> 
#   select(Year, PlotID, species, pcover) |> 
#   pivot_wider(names_from = 'Year', values_from = 'pcover', values_fill = 0)
# 
# 
# write.csv(test, 'Compiled data/SpComp_check.csv', row.names = F )

SpComp_AllYears2<-SpComp_AllYears %>% 
  left_join(SpList) |> 
  left_join(trts)
### Check to make sure species names, numbers, and cleaned names match up. 
#SK checked 2014-2018 and found multiple errors (>15)
#SK cleaned sp comp and sp list through 2018 on May 21, 2019

write.csv(SpComp_AllYears2, "Compiled data/SpComp_2014-2024.csv", row.names = F)





######################################################################################
######################################################################################
######################################################################################
####import all Stem Density data
SD2014<-read.csv("GhostFire2014_Data/Stem Density/GhostFire_SpringStemD_2014.csv")%>%
  #rename(Year=ï..Year)%>%
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
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0) %>% 
  filter(Year!="NA") |> 
  group_by(Year, Burn, Watershed, Block, spnum, Species, Plot) |> 
  summarize(stems=sum(stems))

SD2016<-read.csv("GhostFire2016_Data/StemDensity/GhostFire_SpringStemD_2016_v2.csv")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2017<-read.csv("GhostFire2017_Data/StemDensity/GhostFire_SpringStemD_2017.csv")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2018<-read.csv("GhostFire2018_Data/StemDensity/GhostFire_SpringStemD_2018.csv")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2019<-read.csv("GhostFire2019_Data/StemDensity/GhostFire_SpringStemD_2019.csv")%>%
  select(-Format.ID, -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
#No SD2020
SD2021<-read.csv("GhostFire2021_Data/StemDensity/GhostFire_SpringStemD_2021.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Format.ID, -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11, -X.12) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2022<-read.csv("GhostFire2022_Data/StemDensity/GhostFire_SpringStemD_2022.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2023<-read.csv("GhostFire2023_Data/StemDensity/GhostFire_SpringStemD_2023.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")
SD2024<-read.csv("GhostFire2024_Data/StemDensity/GhostFire_SpringStemDensity_2024.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Format.ID) %>% 
  group_by(Year, Burn, Watershed, Block, spnum, Species) %>% 
  gather(Plot, stems, p1:p6)%>%
  filter(stems!=0)%>% 
  filter(Year!="NA")


#merge all years of StemDensity together with column names Year, Burn, Watershed, Block, Plot, spnum, Species, stems

StemDensity_AllYears<-bind_rows(SD2014, SD2015, SD2016, SD2017, SD2018, SD2019, SD2021, SD2022, SD2023, SD2024)


StemDensity_AllYears2<-StemDensity_AllYears %>% 
  right_join(SpList) %>% 
  filter(Year!="NA") |> 
  separate(Plot, into=c('p', 'Plot'), 1) |> 
  mutate(Plot=as.integer(Plot)) |> 
  select(-p) |> 
  left_join(trts)

# test1<-StemDensity_AllYears2 |> 
#   group_by(Year, PlotID, spnum, Species) |> 
#   summarize(n=length(stems))
# there is sorg in 15 2x, once for 6 stems and once for 1. I am adding those in the 2015 line code

#there does not seem to be an outlier
ggplot(data=StemDensity_AllYears2, aes(x=Year, y=stems, color=species))+
  geom_line()+
  facet_wrap(~PlotID)+
  theme(legend.position = 'none')

### Check to make sure species names, numbers, and cleaned names match up. 
#SK checked 2014-2018 and found multiple errors (>15)
#SK cleaned sp comp and sp list through 2018 on May 21, 2019
write.csv(StemDensity_AllYears2, "Compiled Data/StemDensity_2014-2024.csv", row.names = F)


######################################################################################
######################################################################################
######################################################################################
#import all biomass data, calculate mean across two pseudo replicates
ANPP2014<-read.csv("GhostFire2014_Data/Biomass/GhostFire_Biomass_2014.csv")%>%
  select(-Notes)%>%
  mutate_at(c(7:10), ~replace(., is.na(.), 0))%>%
  rename(Burn=BurnFreq)%>%
  mutate(BurnFreq=ifelse(Burn=="Annual", 1, 20)) %>% 
  select(-Burn) 

ANPP2015<-read.csv("GhostFire2015_Data/Biomass/GhostFire_Biomass_2015.csv")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot))

ANPP2016<-read.csv("GhostFire2016_Data/Biomass/GhostFire_Biomass_2016_formatted.csv")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot))

#No ANPP2017 but still hoping Kevin finds it
#ANPP2017<-read.csv("GhostFire2016_Data/Biomass/GhostFire_Biomass_2016_formatted.csv")%>%
 # select(-Notes)%>%
 # separate(Plot, c("Block", "Plot"), sep=1) %>% 
 # mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
 # mutate(Plot=as.integer(Plot))%>% 
 #   rename(Year=ï..Year)
  
ANPP2018<-read.csv("GhostFire2018_Data/Biomass/GhostFire_Biomass_2018.csv")%>%
  na.omit() %>% 
    mutate(Plot=as.integer(Plot))

ANPP2019<-read.csv("GhostFire2019_Data/Biomass/GhostFire_Biomass_DataEntry2019.csv")%>%
    select(-Notes, -WhoWeighed, -DateWeighed)%>%
    mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
    rename(Watershed=Wateshed)

ANPP2020<-read.csv("GhostFire2020_Data/Biomass/GhostFire_Biomass_2020.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot)) %>% 
  rename(Watershed=Wateshed)

ANPP2021<-read.csv("GhostFire2021_Data/Biomass/GhostFire_Biomass_Data_2021.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Notes)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot)) %>% 
  rename(Watershed=Wateshed)

ANPP2022<-read.csv("GhostFire2022_Data/Biomass/GhostFire_Biomass_Data_2022.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Notes, -QC_flag)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot)) %>% 
  rename(Watershed=Wateshed)

ANPP2023<-read.csv("GhostFire2023_Data/Biomass/GhostFire_Biomass_Data_2023.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Notes, -QC_flag, -Initial_key, -Initial_weighed, -Date_weighed)%>%
  separate(Plot, c("Block", "Plot"), sep=1) %>% 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot)) 

ANPP2024<-read.csv("GhostFire2024_Data/Biomass/GhostFire_ANPP_2024.csv", fileEncoding="UTF-8-BOM")%>%
  select(-Comments) |> 
  mutate_at(c(7:10), ~replace(., is.na(.), 0)) %>% 
  mutate(Plot=as.integer(Plot)) |> 
  rename(Replicate=Rep, P.Dead=Pdead)


#merge all years of ANPP together with column names Year, BurnFreq, Watershed, Block, Plot, Replicate, Grass, Forb, Woody, P.Dead Species, stems
ANPP_AllYears<-bind_rows(ANPP2014, ANPP2015, ANPP2016, ANPP2018, ANPP2019, ANPP2020, ANPP2021, ANPP2022, ANPP2023, ANPP2024) %>% 
  group_by(Year, BurnFreq, Watershed, Block, Plot) %>% 
  summarize_all(mean) %>% 
  select(-Replicate) 

hist(ANPP_AllYears$Forb)

write.csv(ANPP_AllYears, "Compiled Data/ANPP_2014-2024_GFPW.csv", row.names = F)

#merge all years of ANPP together with column names Year, BurnFreq, Watershed, Block, Plot, Replicate, Grass, Forb, Woody, P.Dead Species, stems
ANPP_AllYears_Total<-ANPP_AllYears |> 
  pivot_longer(Grass:Woody, names_to = "Type", values_to = "Biomass") %>% 
  group_by(Year, BurnFreq, Watershed, Block, Plot) %>% 
  summarise(Total=sum(Biomass)*10) |> 
  left_join(trts)

hist(log(ANPP_AllYears_Total$total))
#unlogged looks better, do not log.

write.csv(ANPP_AllYears_Total, "Compiled Data/ANPP_2014-2024_TotalANPP.csv", row.names = F)
write_xlsx(ANPP_AllYears_Total, 'C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\Analyses in SAS\\ANPP_2014-2024_TotalANPP.xlsx')

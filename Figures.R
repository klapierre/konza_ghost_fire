library(tidyverse)
theme_set(theme_bw(12))

setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data")

#ANPP
#want three figures, one of litter by burn over time and one of n over time. Also the full way litter, nutrient over time even though P - 0.091.
anpp<-read.csv('ANPP_2014-2024_TotalANPP.csv')

anpp.litter<-anpp |> 
  group_by(Year, BurnFreq, Litter) |> 
  summarise(manpp=mean(Total), sd=sd(Total), n=length(Total)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(BurnFreq, Litter, sep = "_"))
anpp.n<-anpp |> 
  group_by(Year, Nutrient) |> 
  summarise(manpp=mean(Total), sd=sd(Total), n=length(Total)) |> 
  mutate(se=sd/sqrt(n))
anpp.litter.n<-anpp |> 
  group_by(Year, Nutrient, Litter) |> 
  summarise(manpp=mean(Total), sd=sd(Total), n=length(Total)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Nutrient, Litter, sep = "_"))

ProdLit<-
  ggplot(data=anpp.litter, aes(x=Year, y=manpp, color=Litter, group=group))+
  geom_point(size=3, aes(shape=as.factor(BurnFreq)))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=manpp-se, ymax=manpp+se), width=0.2)+
  ylab('ANPP')
ProdLit


ProdN<-
  ggplot(data=anpp.n, aes(x=Year, y=manpp, color=Nutrient))+
  geom_point(size=3)+
  geom_line()+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=manpp-se, ymax=manpp+se), width=0.2)+
  ylab('ANPP')
ProdN

ProdLitN<-
  ggplot(data=anpp.litter.n, aes(x=Year, y=manpp, color=Nutrient, group=group))+
  geom_point(size=3, aes(shape=as.factor(Litter)))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='Litter')+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=manpp-se, ymax=manpp+se), width=0.2)+
  ylab('ANPP')+
  facet_wrap(~Nutrient, ncol=1)
ProdLitN


###Richness
rich<-read.csv('PlantDiversity.csv')

rich.litter<-rich |> 
  group_by(Year, Burn.Trt, Litter) |> 
  summarise(mrich=mean(richness), sd=sd(richness), n=length(richness)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Burn.Trt, Litter, sep = "_"))

RichLit<-
  ggplot(data=rich.litter, aes(x=Year, y=mrich, color=Litter, group=group))+
  geom_point(size=3, aes(shape=Burn.Trt))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.2)+
  ylab('Richness')
RichLit

###Stem density
stems<-read.csv('StemTotals.csv')

stem.litter<-stems |> 
  group_by(Year, Burn.Trt, Litter) |> 
  summarise(mstems=mean(TotalStems), sd=sd(TotalStems), n=length(TotalStems)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Burn.Trt, Litter, sep = "_"))

StemsLit<-
  ggplot(data=stem.litter, aes(x=Year, y=mstems, color=Litter, group=group))+
  geom_point(size=3, aes(shape=Burn.Trt))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mstems-se, ymax=mstems+se), width=0.2)+
  ylab('Stem Density')
StemsLit

###Mycorr
myc<-read.csv('Myc_2014_2019_2024.csv')

myc.litter<-myc |> 
  group_by(Year, Litter) |> 
  summarise(mcol=mean(Colonization), sd=sd(Colonization), n=length(Colonization)) |> 
  mutate(se=sd/sqrt(n)) 
MycLit<-
  ggplot(data=myc.litter, aes(x=Year, y=mcol, color=Litter))+
  geom_point(size=3)+
  geom_line()+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mcol-se, ymax=mcol+se), width=0.2)+
  ylab('Mycorrhizal Rt Colonization %')
MycLit

###SRC
SRC<-read.csv('SRC_2014_2019_2024.csv')

src.lit<-SRC |> 
  group_by(Year, Burn.Trt, Litter) |> 
  summarise(msrc=mean(SRC, na.rm = T), sd=sd(SRC,na.rm = T), n=length(SRC)) |>
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Burn.Trt, Litter, sep = "_"))

SRCLit<-
  ggplot(data=src.lit, aes(x=Year, y=msrc, color=Litter, group=group))+
  geom_point(size=3, aes(shape=Burn.Trt))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=msrc-se, ymax=msrc+se), width=0.2)+
  ylab('Standing Crop Root Biomass')
SRCLit


###BNPP
bnpp<-read.csv('BNPP_2014_2019_2024.csv')

bnpp.litter<-bnpp |> 
  group_by(Year, Litter) |> 
  summarise(mbnpp=mean(AFDMgm), sd=sd(AFDMgm), n=length(AFDMgm)) |> 
  mutate(se=sd/sqrt(n)) 

BNPPLit<-
  ggplot(data=bnpp.litter, aes(x=Year, y=mbnpp, color=Litter))+
  geom_point(size=3)+
  geom_line()+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mbnpp-se, ymax=mbnpp+se), width=0.2)+
  ylab('BNPP')
BNPPLit

##SOM
Enzyme<-read.csv('SoilEnzymeProp_2014_2018_2024.csv') |> 
  select(Watershed, Block, Plot, Litter, Nutrient, SOM_LOI, BG, NAG, CBH, POX, PHX)  |> 
  pivot_longer(SOM_LOI:PHX, names_to = 'enzyme', values_to = 'value')

enz.n<-Enzyme |> 
  group_by(Nutrient, enzyme) |> 
  summarise(mval=mean(value, na.rm=T), sd=sd(value, na.rm = T), n=length(value)) |> 
  mutate(se=sd/sqrt(n)) 

EnzN<-
  ggplot(data=enz.n, aes(x=Nutrient, y=mval, color=Nutrient))+
  geom_point(size=3)+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=mval-se, ymax=mval+se), width=0.2)+
  facet_wrap(~enzyme, scales='free')+
  ylab("Soil Enzyme Activity or Property")
EnzN




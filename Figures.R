library(tidyverse)
theme_set(theme_bw(12))
library(ggpubr)

setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data")

#ANPP
#want three figures, one of litter by burn over time and one of n over time. Also the full way litter, nutrient over time even though P - 0.091.
anpp<-read.csv('ANPP_2014-2024_TotalANPP.csv')

anpp.litter<-anpp |> 
  filter(Year!=2014) |> 
  group_by(Year, Burn.Trt, Litter) |> 
  summarise(manpp=mean(Total), sd=sd(Total), n=length(Total)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Burn.Trt, Litter, sep = "_"))
anpp.n<-anpp |> 
  filter(Year!=2014) |> 
  group_by(Burn.Trt, Nutrient) |> 
  summarise(manpp=mean(Total), sd=sd(Total), n=length(Total)) |> 
  mutate(se=sd/sqrt(n))

ProdLit<-
  ggplot(data=anpp.litter, aes(x=Year, y=manpp, color=Litter, group=group))+
  geom_point(size=3, aes(shape=as.factor(Burn.Trt)))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=manpp-se, ymax=manpp+se), width=0.2)+
  ylab('ANPP')+
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ProdLit


ProdN<-
  ggplot(data=anpp.n, aes(x=Burn.Trt, y=manpp, color=Nutrient))+
  geom_point(size=3)+
  geom_line()+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=manpp-se, ymax=manpp+se), width=0.2)+
  ylab('ANPP')+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ProdN


###Richness
rich<-read.csv('PlantDiversity.csv')

rich.litter<-rich |> 
  filter(Year!=2014) |> 
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
  ylab('Richness')+
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
RichLit

###Stem density
stems<-read.csv('StemTotals.csv')

stem.litter<-stems |> 
  filter(Year!=2014) |> 
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
  ylab('Stem Density')+
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
StemsLit

###Mycorr
myc<-read.csv('Myc_2014_2019_2024.csv')

myc.litter<-myc |>
  filter(Year!=2014) |> 
  group_by(Year, Litter) |> 
  summarise(mcol=mean(Colonization), sd=sd(Colonization), n=length(Colonization)) |> 
  mutate(se=sd/sqrt(n)) 
myc.n<-myc |>
  filter(Year!=2014) |> 
  group_by(Nutrient) |> 
  summarise(mcol=mean(Colonization), sd=sd(Colonization), n=length(Colonization)) |> 
  mutate(se=sd/sqrt(n)) 
MycLit<-
  ggplot(data=myc.litter, aes(x=Year, y=mcol, color=Litter))+
  geom_point(size=3)+
  geom_line()+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mcol-se, ymax=mcol+se), width=0.2)+
  ylab('Mycorrhizal Rt Colonization %')+
  scale_x_continuous(breaks = c(2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
MycLit

MycN<-
  ggplot(data=myc.n, aes(x=Nutrient, y=mcol, color=Nutrient))+
  geom_point(size=3)+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=mcol-se, ymax=mcol+se), width=0.2)+
  ylab('Mycorrhizal Rt Colonization %')+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
MycN

ggarrange(MycLit, MycN)

###SRC
SRC<-read.csv('SRC_2014_2019_2024.csv')

src.lit<-SRC |> 
  group_by(Year, Burn.Trt, Litter) |> 
  filter(Year!=2014) |> 
  summarise(msrc=mean(SRC, na.rm = T), sd=sd(SRC,na.rm = T), n=length(SRC)) |>
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Burn.Trt, Litter, sep = "_"))
src.n<-SRC |> 
  group_by(Nutrient) |> 
  filter(Year!=2014) |> 
  summarise(msrc=mean(SRC, na.rm = T), sd=sd(SRC,na.rm = T), n=length(SRC)) |>
  mutate(se=sd/sqrt(n)) 

SRCLit<-
  ggplot(data=src.lit, aes(x=Year, y=msrc, color=Litter, group=group))+
  geom_point(size=3, aes(shape=Burn.Trt))+
  geom_line()+
  scale_shape_manual(values=c(1,16), name='BurnFreq')+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=msrc-se, ymax=msrc+se), width=0.2)+
  ylab('Standing Crop Root Biomass')+
  scale_x_continuous(breaks = c(2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
SRCLit

SRCN<-
  ggplot(data=src.n, aes(x=Nutrient, y=msrc, color=Nutrient))+
  geom_point(size=3)+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=msrc-se, ymax=msrc+se), width=0.2)+
  ylab('Standing Crop Root Biomass')+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
SRCN

ggarrange(SRCLit, SRCN)
###BNPP
bnpp<-read.csv('BNPP_2014_2019_2024.csv')

bnpp.litter<-bnpp |> 
  group_by(Burn_Trt, Litter) |> 
  summarise(mbnpp=mean(AFDMgm), sd=sd(AFDMgm), n=length(AFDMgm)) |> 
  mutate(se=sd/sqrt(n)) 

BNPPLit<-
  ggplot(data=bnpp.litter, aes(x=Burn_Trt, y=mbnpp, color=Litter))+
  geom_point(size=3)+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mbnpp-se, ymax=mbnpp+se), width=0.2)+
  ylab('BNPP')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
BNPPLit

#insects
insects<-read.csv('invertMetricsSAS.csv')

inrich<-insects |> 
  filter(Year!=2014) |> 
  group_by(Year, Litter, Nutrient) |> 
  summarise(mrich=mean(richness), sd=sd(richness), n=length(richness)) |> 
  mutate(se=sd/sqrt(n)) |> 
  mutate(group=paste(Litter, Nutrient, sep = "_"))
ineven<-insects |> 
  filter(Year!=2014) |> 
  group_by(Year, Litter, Nutrient) |>
  summarise(meven=mean(Evar), sd=sd(Evar), n=length(Evar)) |> 
  mutate(se=sd/sqrt(n))|> 
  mutate(group=paste(Litter, Nutrient, sep = "_"))

InRich<-
ggplot(data=inrich, aes(x=Year, y=mrich, color=Nutrient, group = group ))+
  geom_point(size=3, aes(shape=Litter))+
  geom_line()+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.2)+
  ylab('Insect Richness')+
  scale_x_continuous(breaks = c(2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  facet_wrap(~Nutrient, ncol=1)
InRich

InEven<-
  ggplot(data=ineven, aes(x=Year, y=meven, color=Nutrient, group = group ))+
  geom_point(size=3, aes(shape=Litter))+
  geom_line()+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.2)+
  ylab('Insect Evenness')+
  scale_x_continuous(breaks = c(2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  facet_wrap(~Nutrient, ncol=1)
InEven

ggarrange(InRich, InEven)

inabund.lit<-insects |> 
  filter(Year!=2014) |> 
  group_by(Litter) |> 
  summarise(mabund=mean(abundance), sd=sd(abundance), n=length(abundance)) |> 
  mutate(se=sd/sqrt(n)) 
inabund.nlit<-insects |> 
  filter(Year!=2014) |> 
  group_by(Year, Litter, Nutrient) |>
  summarise(mabund=mean(abundance), sd=sd(abundance), n=length(abundance)) |> 
  mutate(se=sd/sqrt(n))|> 
  mutate(group=paste(Litter, Nutrient, sep = "_"))


InAbundLN<-
  ggplot(data=inabund.nlit, aes(x=Year, y=mabund, color=Nutrient, group = group ))+
  geom_point(size=3, aes(shape=Litter))+
  geom_line()+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black", 'burlywood4', "green4"))+
  geom_errorbar(aes(ymin=mabund-se, ymax=mabund+se), width=0.2)+
  ylab('Insect Abundance')+
  scale_x_continuous(breaks = c(2019, 2021, 2023))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  facet_wrap(~Nutrient, ncol=2)
InAbundLN

InAbundLit<-
  ggplot(data=inabund.lit, aes(x=Litter, y=mabund, color=Litter))+
  geom_point(size = 3)+
  scale_color_manual(values=c("gold", 'darkgoldenrod4'))+
  geom_errorbar(aes(ymin=mabund-se, ymax=mabund+se), width=0.2)+
  ylab('Insect Abundance')+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
InAbundLit

ggarrange(InAbundLit, InAbundLN)

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




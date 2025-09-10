### Analyze and visualize ANPP data
###
### Last modified: Sept 10, 2025

### 
### Set up workspace
###
{
setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")
setwd("C:\\Users\\k_wilcox\\Dropbox\\Shared projects\\Ghost Fire\\DATA\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)
theme_set(theme_bw(20))

}


###
### Read in, clean, and combine all years of ANPP data
###

### NOTE: I STILL NEED TO GO THROUGH AND CLEAN EACH YEAR OF DATA BEFORE COMBINING TO GET PLOT MEANS
anpp_14_21 <- read.csv("Compiled data\\ANPP_2014-2021_GFPW.CSV") %>% 
#  rename(plot=Plot) %>% 
  mutate(
    QC_flag=NA,
    PlotID=paste(Watershed, Block, Plot, sep="_"),
    BlockID=paste(Watershed, Block, sep="_")
    ) %>%
  dplyr::select(Year, BurnFreq, Watershed, Block, Plot, PlotID, BlockID,
                Grass, Forb, Woody, P.Dead, QC_flag)

## 2022
anpp_22 <- read.csv("GhostFire2022_Data\\Biomass\\GhostFire_Biomass_Data_2022.csv") %>%
  rename(Watershed=Wateshed) %>%
  separate_wider_position(Plot, c(Block=1, Plot=1)) %>%
  mutate(
    PlotID=paste(Watershed, Block, Plot, sep="_"),
    BlockID=paste(Watershed, Block, sep="_"),
    Plot=as.numeric(Plot)
  ) %>%
  filter(QC_flag!=1) %>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, PlotID, BlockID) %>%
  summarize(
    Grass = sum(Grass, na.rm=T), # assumes NAs represent 0s, but we should change this if that's not the case
    Forb = sum(Forb, na.rm=T),# assumes NAs represent 0s, but we should change this if that's not the case
    Woody = sum(Woody, na.rm=T),
    P.Dead = sum(P.Dead, na.rm=T),
    QC_flag = max(QC_flag, na.rm=T),
    .groups="drop"
#    Notes = paste(Notes, sep="_") # doesn't work, I think we would need to use reframe to do this
    )

## 2023
anpp_23 <- read.csv("GhostFire2023_Data\\Biomass\\GhostFire_Biomass_Data_2023.csv") %>%
  separate_wider_position(Plot, c(Block=1, Plot=1)) %>%
  mutate(
    PlotID=paste(Watershed, Block, Plot, sep="_"),
    BlockID=paste(Watershed, Block, sep="_"),
    Plot=as.numeric(Plot)
  ) %>%
  filter(QC_flag!=1) %>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, PlotID, BlockID) %>%
  summarize(
    Grass = sum(Grass, na.rm=T), # assumes NAs represent 0s, but we should change this if that's not the case
    Forb = sum(Forb, na.rm=T),# assumes NAs represent 0s, but we should change this if that's not the case
    Woody = sum(Woody, na.rm=T),
    P.Dead = sum(P.Dead, na.rm=T),
    QC_flag = max(QC_flag, na.rm=T),
    .groups="drop"
    #    Notes = paste(Notes, sep="_") # doesn't work, I think we would need to use reframe to do this
  )

## 2024
anpp_24 <- read.csv("GhostFire2024_Data\\Biomass\\GhostFire_ANPP_2024.csv") %>%
  rename(P.Dead=Pdead) %>%
  mutate(
    PlotID=paste(Watershed, Block, Plot, sep="_"),
    BlockID=paste(Watershed, Block, sep="_"),
    Plot=as.numeric(Plot)
  ) %>%
  mutate(QC_flag=0) %>%
  group_by(Year, BurnFreq, Watershed, Block, Plot, PlotID, BlockID) %>%
  summarize(
    Grass = sum(Grass, na.rm=T), # assumes NAs represent 0s, but we should change this if that's not the case
    Forb = sum(Forb, na.rm=T),# assumes NAs represent 0s, but we should change this if that's not the case
    Woody = sum(Woody, na.rm=T),
    P.Dead = sum(P.Dead, na.rm=T),
    QC_flag = max(QC_flag, na.rm=T),
    .groups="drop"
    #    Notes = paste(Notes, sep="_") # doesn't work, I think we would need to use reframe to do this
  )




### Combine all years together
anpp_14_24 <- anpp_14_21 %>% 
  bind_rows(
    anpp_22,
    anpp_23,
    anpp_24)

## Make sure all years have the same plot identifiers
unique(anpp_14_24$Year)
unique(anpp_14_24$BurnFreq)
unique(anpp_14_24$Watershed)
unique(anpp_14_24$Block)
unique(anpp_14_24$Plot)

with(anpp_14_24, table(Year, Block)) # 2017 only has one value and I think we didn't sample ANPP in that year
## there are a few mixxing plots in 2022, and in 2018... could look into these - I think the 2018 might be the missing bag of ANPP that happened :(

unique(anpp_14_21$Year)
unique(anpp_22$Year)
unique(anpp_23$Year)
unique(anpp_24$Year) ## It's here in 2024 -- I check in the raw data and its clearly a data entry error, fixed it in the excel spreadsheet and csv files

filter(anpp_14_24, BlockID=="20C_C")
filter(anpp_14_24, Grass>500)
filter(anpp_22, PlotID=="1D_B_5")
  #check ANPP data
ggplot(anpp_14_24, aes(x=Grass))+
  geom_histogram()+
  facet_wrap(~Year)

ggplot(anpp_14_24, aes(x=Forb))+
  geom_histogram()+
  facet_wrap(~Year)

ggplot(anpp_14_24, aes(x=Woody))+
  geom_histogram()+
  facet_wrap(~Year)

ggplot(anpp_14_24, aes(x=P.Dead))+
  geom_histogram()+
  facet_wrap(~Year)

### Left off here
trt_key<-read.csv("GF_PlotList_Trts.csv") %>% 
  select(-plot_id) %>% 
  rename(BurnFreq=Burn.Trt2)%>%
  select(-Burn.Trt) 

anpp2<-anpp %>% 
  left_join(trts)

###analysis of total biomass

fit <- lmer(total ~  as.factor(BurnFreq)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = filter(anpp2, Year!=2014))
anova(fit,  ddf="Kenward-Roger")

#doing contrasts 
emmeans(fit, pairwise~Nutrient, adjust="holm")

###analysis annual burn only
fit1 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(anpp2, BurnFreq==1&Year!=2014))
anova(fit1, ddf="Kenward-Roger")

###analysis of unburned only
fit20 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(anpp2, BurnFreq==20&Year!=2014))
anova(fit20, ddf="Kenward-Roger")

##################
########Doing the analysis of how to make annual to unburned and vice-versa
####################
means<-anpp2 %>% 
  group_by(Year, BurnFreq, Litter, Nutrient) %>%
  summarize(Mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(treatment=ifelse(BurnFreq==1&Nutrient=="C"&Litter=="A", "Annual Burn",
                          ifelse (BurnFreq==1&Nutrient=="U"&Litter=="P", "Annual+N+L",
                                  ifelse(BurnFreq==1&Nutrient=="U"&Litter=="A", "Annual+Nitrogen",
                                         ifelse(BurnFreq==1&Nutrient=="C"&Litter=="P", "Annual+Litter",ifelse(BurnFreq==20&Nutrient=="C"&Litter=="P", "Unburned", ifelse(BurnFreq==20&Nutrient=="S"&Litter=="A", "Unburned-N-L", ifelse(BurnFreq==20&Nutrient=="S"&Litter=="P", "Unburned-Nitrogen", ifelse(BurnFreq==20&Nutrient=="C"&Litter=="A", "Unburned-Litter", "drop")))))))))

#annaul
ggplot(data=subset(means, treatment!="drop"&treatment!="Unburned-N-L"&treatment!="Unburned-Nitrogen"&treatment!="Unburned-Litter"), aes(x=Year, y=Mtot, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Annual+Nitrogen", "Annual+Litter", "Annual+N+L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=Mtot-se, ymax=Mtot+se), width=0.1)+
  ylab("ANPP")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#unburned
ggplot(data=subset(means, treatment!="drop"&treatment!="Annual+N+L"&treatment!="Annual+Nitrogen"&treatment!="Annual+Litter"), aes(x=Year, y=Mtot, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Unburned-Nitrogen", "Unburned-Litter", "Unburned-N-L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=Mtot-se, ymax=Mtot+se), width=0.1)+
  ylab("ANPP")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#########################
###2014 data
########################
b14<-anpp2 %>% 
  filter(Year==2014)

#no difference in ANPP btwn burn trts
fit14 <- lmer(total ~  as.factor(BurnFreq) +(1|Watershed/Block), data = b14)
anova(fit14)

#there is a difference in ANPP by watershed
summary(aov(total~Watershed, data=b14))

means14<-b14 %>% 
  group_by(BurnFreq) %>%
  summarize(mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=means14, aes(x=as.factor(BurnFreq), y=mtot, fill=as.factor(BurnFreq)))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("black", "gray47"))+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1)+
  ylab("ANPP")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_x_discrete(name="Burn Frequency", labels=c("Annual", "Unburned"))

##############################
#####Analysis of overall treatments
###########################

###there are interactive effects of year and burn trt, litter and nutrients
meanN<-anpp2 %>% 
  filter(Year!=2014) %>% 
  group_by(Nutrient) %>%
  summarize(Mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=meanN, aes(x=Nutrient, y=Mtot, fill=Nutrient))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Mtot-se, ymax=Mtot+se), width=0.1, position=position_dodge(0.9))+
  scale_x_discrete(limits=c("S", 'C', 'U'), labels=c("-N", 'C','+N'))+
  scale_fill_manual(values=c( "darkolivegreen3","darkolivegreen1", 'darkolivegreen'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none")+
  annotate("text", x=1, y=450, label="C", size=5)+
  annotate("text", x=2, y=575, label="B", size=5)+
  annotate("text", x=3, y=650, label="A", size=5)+
  xlab('Nutrient Treatment')+
  ylab('ANPP')

#not interested in this
meanL<-anpp2 %>% 
  group_by(Year, Litter) %>%
  summarize(Mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=meanL, aes(x=Year, y=Mtot, color=Litter))+
  geom_point()+
  geom_line()

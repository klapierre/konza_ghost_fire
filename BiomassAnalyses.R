setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
theme_set(theme_bw())

anpp<-read.csv("ANPP_2014-2019.CSV") %>% 
  rename(plot=Plot) %>% 
  mutate(Plot=paste(Block, plot, sep="")) %>% 
  select(-plot)
#check ANPP data
ggplot(anpp, aes(x=total))+
  geom_histogram()+
  facet_wrap(~Year)

trts<-read.csv("GF_PlotList_Trts.csv") %>% 
  select(-plot_id) %>% 
  rename(BurnFreq=Burn.Trt2)%>%
  select(-Burn.Trt) 

anpp2<-anpp %>% 
  left_join(trts)

###analysis of total biomass

fit <- lmer(total ~  BurnFreq*Litter*Nutrient*Year +(1|Watershed/Block), data = anpp2)
anova(fit)

###analysis annual burn only
fit1 <- lmer(total ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(anpp2, BurnFreq==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(total ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(anpp2, BurnFreq==20))
anova(fit20)


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
  ylab("Number of Stems")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#unburned
ggplot(data=subset(means, treatment!="drop"&treatment!="Annual+N+L"&treatment!="Annual+Nitrogen"&treatment!="Annual+Litter"), aes(x=Year, y=Mtot, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Unburned-Nitrogen", "Unburned-Litter", "Unburned-N-L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=Mtot-se, ymax=Mtot+se), width=0.1)+
  ylab("Number of Stems")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
theme_set(theme_bw())

sd<-read.csv("StemDensity_2014-2019.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

trts<-read.csv("GF_PlotList_Trts.csv") %>% 
  select(-plot_id) %>% 
  rename(Burn=Burn.Trt2)%>%
  select(-Burn.Trt)

sd2<-sd %>% 
  left_join(trts)

###analysis of total stems
total=sd2 %>% 
  group_by(Year, Watershed, Block, Plot, Litter, Nutrient, treatment, Burn) %>% 
  summarise(total=sum(stems))

fit <- lmer(total ~  Burn*Litter*Nutrient*Year +(1|Watershed/Block), data = total)
anova(fit)

###analysis annual burn only
fit1 <- lmer(total ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(total, Burn==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(total ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(total, Burn==20))
anova(fit20)


means<-total %>% 
  group_by(Year, Burn, Litter, Nutrient) %>%
  summarize(Mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(treatment=ifelse(Burn==1&Nutrient=="C"&Litter=="A", "Annual Burn",
                          ifelse (Burn==1&Nutrient=="U"&Litter=="P", "Annual+N+L",
                                  ifelse(Burn==1&Nutrient=="U"&Litter=="A", "Annual+Nitrogen",
                                         ifelse(Burn==1&Nutrient=="C"&Litter=="P", "Annual+Litter",ifelse(Burn==20&Nutrient=="C"&Litter=="P", "Unburned", ifelse(Burn==20&Nutrient=="S"&Litter=="A", "Unburned-N-L", ifelse(Burn==20&Nutrient=="S"&Litter=="P", "Unburned-Nitrogen", ifelse(Burn==20&Nutrient=="C"&Litter=="A", "Unburned-Litter", "drop")))))))))

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
         
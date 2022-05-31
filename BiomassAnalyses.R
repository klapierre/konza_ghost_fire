setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)
theme_set(theme_bw(20))

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

fit <- lmer(total ~  as.factor(BurnFreq)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = filter(anpp2, Year!=2014))
anova(fit,  ddf="Kenward-Roger")

#doing contrasts 
emmeans(fit, pairwise~Nutrient, adjust="holm")

###analysis annual burn only
fit1 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(anpp2, BurnFreq==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(anpp2, BurnFreq==20))
anova(fit20)

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
  scale_fill_manual(values=c("orangered", "orange", 'orangered4'))+
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

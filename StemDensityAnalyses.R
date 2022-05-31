setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")
setwd("E:\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)
theme_set(theme_bw(20))

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

fit <- lmer(total ~  as.factor(Burn)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(total, Year!=2014))
anova(fit, ddf='Kenward-Roger')

emmeans(fit, pairwise~Litter|as.factor(Burn), adjust="holm")
emmeans(fit, pairwise~Nutrient*as.factor(Burn), adjust="holm")


###analysis annual burn only
fit1 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(total, Burn==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(total ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(total, Burn==20))
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


##pre treatment data in 2014
t14<-total %>% 
  filter(Year==2014)

#no difference in # stems btwn burn trts
fit14 <- lmer(total ~  Burn +(1|Watershed/Block), data = t14)
anova(fit14)
         
#ther eis a difference in stems by watershed
summary(aov(total~Watershed, data=t14))

means14<-t14 %>% 
  group_by(Burn) %>%
  summarize(Mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=means14, aes(x=as.factor(Burn), y=Mtot, fill=as.factor(Burn)))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("black", "gray47"))+
  geom_errorbar(aes(ymin=Mtot-se, ymax=Mtot+se), width=0.1)+
  ylab("Number of Stems")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_x_discrete(name="Burn Frequency", labels=c("Annual", "Unburned"))


##############################
#####Analysis of overall treatments
###########################

mstemsL<-total %>% 
  group_by(Burn, Litter) %>%
  filter(Year!=2014) %>% 
  summarize(mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=mstemsL, aes(x=as.factor(Burn), y=mtot, fill=Litter))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(values=c("gold", "darkgoldenrod4"), labels=c('Absent', 'Present'))+
  scale_x_discrete(labels=c('Annual', 'Unburned'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=0.75, y=245, label="A", size=5)+
  annotate("text", x=1.25, y=150, label="BCD", size=5)+
  annotate("text", x=1.75, y=170, label="C", size=5)+
  annotate("text", x=2.25, y=140, label="D", size=5)+
  xlab('Burn Treatment')+
  ylab('Number of Stems') 

mstemsN<-total %>% 
  filter(Year!=2014) %>% 
  group_by(Burn, Nutrient) %>%
  summarize(mtot=mean(total, na.rm=T), sddev=sd(total), n=length(total)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(Ntrt=ifelse(Nutrient=='S', 1, ifelse(Nutrient=='C', 2, 3)))

ggplot(data=mstemsN, aes(x=as.factor(Burn), y=mtot, fill=as.factor(Ntrt)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(name='Nutrient\nTreatment', values=c('orange', "orangered", 'orangered4'),labels=c("-N", ' C', '+N'))+
  scale_x_discrete(labels=c('Annual', 'Unburned'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=0.72, y=175, label="B", size=5)+
  annotate('text', x=1, y=215, label="A", size=5)+
  annotate("text", x=1.27, y=215, label="A", size=5)+
  annotate("text", x=1.72, y=165, label="B", size=5)+
  annotate("text", x=2, y=160, label="B", size=5)+
  annotate("text", x=2.27, y=155, label="B", size=5)+
  xlab('Burn Treatment')+
  ylab('Number of Stems') 


###tracking Andro and Poa populations

#Andro
fit <- lmer(stems ~  as.factor(Burn)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(sd2, Year!=2014&spnum==2))
anova(fit, ddf='Kenward-Roger')

emmeans(fit, pairwise~as.factor(Burn)|Nutrient, adjust="holm")

mstemsN<-sd2 %>% 
  filter(Year!=2014, spnum==2) %>% 
  group_by(Burn, Nutrient) %>%
  summarize(mtot=mean(stems, na.rm=T), sddev=sd(stems), n=length(stems)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(Ntrt=ifelse(Nutrient=='S', 1, ifelse(Nutrient=='C', 2, 3))) %>% 
  mutate(BurnTrt=ifelse(Burn==1, "Annual", "Unburned"))

ggplot(data=mstemsN, aes(x=BurnTrt, y=mtot, fill=as.factor(Ntrt)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(name='Nutrient\nTreatment', values=c('orange', "orangered", 'orangered4'),labels=c("-N", ' C', '+N'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=0.72, y=54, label="ABC", size=5)+
  annotate('text', x=1, y=56, label="ABC", size=5)+
  annotate("text", x=1.27, y=45, label="ABC", size=5)+
  annotate("text", x=1.72, y=30, label="C", size=5)+
  annotate("text", x=2, y=45, label="B", size=5)+
  annotate("text", x=2.27, y=50, label="A", size=5)+
  xlab('Burn Treatment')+
  ylab('Number of A. gerardii Stems')

mstemsN<-sd2 %>% 
  filter(Year!=2014, spnum==2) %>% 
  group_by(Burn, Nutrient, Litter) %>%
  summarize(mtot=mean(stems, na.rm=T), sddev=sd(stems), n=length(stems)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(Ntrt=ifelse(Nutrient=='S', 1, ifelse(Nutrient=='C', 2, 3))) %>% 
  mutate(BurnTrt=ifelse(Burn==1, "Annual", "Unburned"))

ggplot(data=mstemsN, aes(x=Litter, y=mtot, fill=as.factor(Ntrt)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(name='Nutrient\nTreatment', values=c('orange', "orangered", 'orangered4'),labels=c("-N", ' C', '+N'))+
  scale_x_discrete(labels=c("Absent", "Present"))+
  xlab('Light Treatment')+
  ylab('Number of A. gerardii Stems')+
  facet_wrap(~BurnTrt, ncol=1)


#Schiz - model has problems
fit <- lmer(stems ~  as.factor(Burn)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(sd2, Year!=2014&spnum==3))
anova(fit, ddf='Kenward-Roger')

#Poa - nothing happening here
fit <- lmer(stems ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(sd2, Year!=2014&spnum==17&Burn==20))
anova(fit, ddf='Kenward-Roger')

emmeans(fit, pairwise~Litter|as.factor(Burn), adjust="holm")


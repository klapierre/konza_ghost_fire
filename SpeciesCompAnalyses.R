setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")
setwd("E:\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(codyn)
library(stringr)
library(lme4)
library(lmerTest)
theme_set(theme_bw(20))

sp<-read.csv("SpComp_2014-2018.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

trts<-read.csv("GF_PlotList_Trts.csv")

sp2<-sp %>% 
  left_join(trts)

###################################
###analysis richness
######################################

richeven<-community_structure(sp2, time.var="Year", replicate.var = "plot_id", abundance.var = "pcover") %>% 
  left_join(trts)

fit <- lmer(richness ~  Burn.Trt*Litter*Nutrient*Year +(1|Watershed/Block), data = richeven)
anova(fit)

###analysis annual burn only
fit1 <- lmer(richness ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(richness ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==20))
anova(fit20)

##plotting richness
means<-richeven %>% 
  rename(Burn=Burn.Trt2) %>% 
  group_by(Year, Burn, Litter, Nutrient) %>%
  summarize(mrich=mean(richness, na.rm=T), sddev=sd(richness), n=length(richness)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(treatment=ifelse(Burn==1&Nutrient=="C"&Litter=="A", "Annual Burn",
                          ifelse (Burn==1&Nutrient=="U"&Litter=="P", "Annual+N+L",
                                  ifelse(Burn==1&Nutrient=="U"&Litter=="A", "Annual+Nitrogen",
                                         ifelse(Burn==1&Nutrient=="C"&Litter=="P", "Annual+Litter",ifelse(Burn==20&Nutrient=="C"&Litter=="P", "Unburned", ifelse(Burn==20&Nutrient=="S"&Litter=="A", "Unburned-N-L", ifelse(Burn==20&Nutrient=="S"&Litter=="P", "Unburned-Nitrogen", ifelse(Burn==20&Nutrient=="C"&Litter=="A", "Unburned-Litter", "drop")))))))))

#annaul
ggplot(data=subset(means, treatment!="drop"&treatment!="Unburned-N-L"&treatment!="Unburned-Nitrogen"&treatment!="Unburned-Litter"), aes(x=Year, y=mrich, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Annual+Nitrogen", "Annual+Litter", "Annual+N+L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.1)+
  ylab("Number of Species")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#unburned
ggplot(data=subset(means, treatment!="drop"&treatment!="Annual+N+L"&treatment!="Annual+Nitrogen"&treatment!="Annual+Litter"), aes(x=Year, y=mrich, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Unburned-Nitrogen", "Unburned-Litter", "Unburned-N-L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.1)+
  ylab("Number of Species")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##pre treatment data in 2014
r14<-richeven %>% 
  filter(Year==2014)

#no difference in # stems btwn burn trts
fit14 <- lmer(richness ~  Burn.Trt2 +(1|Watershed/Block), data = r14)
anova(fit14)

#ther eis a difference in stems by watershed
summary(aov(richness~Watershed, data=r14))

means14<-r14 %>% 
  group_by(Burn.Trt) %>%
  summarize(mrich=mean(richness, na.rm=T), sddev=sd(richness), n=length(richness)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=means14, aes(x=Burn.Trt, y=mrich, fill=Burn.Trt))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("black", "gray47"))+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.1)+
  ylab("Number of Species")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_x_discrete(name="Burn Frequency", labels=c("Annual", "Unburned"))

###########################
##Evenness
############################

fit <- lmer(Evar ~  Burn.Trt*Litter*Nutrient*Year +(1|Watershed/Block), data = richeven)
anova(fit)

###analysis annual burn only
fit1 <- lmer(Evar ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(Evar ~  Litter*Nutrient*Year +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==20))
anova(fit20)

####plotting evenness
means<-richeven %>% 
  rename(Burn=Burn.Trt2) %>% 
  group_by(Year, Burn, Litter, Nutrient) %>%
  summarize(meven=mean(Evar, na.rm=T), sddev=sd(Evar), n=length(Evar)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(treatment=ifelse(Burn==1&Nutrient=="C"&Litter=="A", "Annual Burn",
                          ifelse (Burn==1&Nutrient=="U"&Litter=="P", "Annual+N+L",
                                  ifelse(Burn==1&Nutrient=="U"&Litter=="A", "Annual+Nitrogen",
                                         ifelse(Burn==1&Nutrient=="C"&Litter=="P", "Annual+Litter",ifelse(Burn==20&Nutrient=="C"&Litter=="P", "Unburned", ifelse(Burn==20&Nutrient=="S"&Litter=="A", "Unburned-N-L", ifelse(Burn==20&Nutrient=="S"&Litter=="P", "Unburned-Nitrogen", ifelse(Burn==20&Nutrient=="C"&Litter=="A", "Unburned-Litter", "drop")))))))))

#annaul
ggplot(data=subset(means, treatment!="drop"&treatment!="Unburned-N-L"&treatment!="Unburned-Nitrogen"&treatment!="Unburned-Litter"), aes(x=Year, y=meven, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Annual+Nitrogen", "Annual+Litter", "Annual+N+L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.1)+
  ylab("Species Evenness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#unburned
ggplot(data=subset(means, treatment!="drop"&treatment!="Annual+N+L"&treatment!="Annual+Nitrogen"&treatment!="Annual+Litter"), aes(x=Year, y=meven, color=treatment))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual(name="Treatment", breaks = c("Annual Burn","Unburned-Nitrogen", "Unburned-Litter", "Unburned-N-L", "Unburned"), values=c("black", "red", "blue","darkorchid4", "gray47"))+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.1)+
  ylab("Species Evenness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##pre treatment data in 2014
r14<-richeven %>% 
  filter(Year==2014)

#no difference in # stems btwn burn trts
fit14 <- lmer(Evar ~  Burn.Trt2 +(1|Watershed/Block), data = r14)
anova(fit14)

#ther eis a difference in stems by watershed
summary(aov(Evar~Watershed, data=r14))

means14<-r14 %>% 
  group_by(Burn.Trt) %>%
  summarize(meven=mean(Evar, na.rm=T), sddev=sd(Evar), n=length(Evar)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=means14, aes(x=Burn.Trt, y=meven, fill=Burn.Trt))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("black", "gray47"))+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.1)+
  ylab("Species Evenness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  scale_x_discrete(name="Burn Frequency", labels=c("Annual", "Unburned"))


##############################
#Analysis of composition
############################

###Unburned as the reference
unref<-multivariate_difference(sp2, time.var="Year", species.var="", abundance.var = "pcover", replicate.var = "plot_id", treatment.var="", reference.treatment = "control_control")


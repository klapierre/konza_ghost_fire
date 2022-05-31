setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")
setwd("E:\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(codyn)
library(stringr)
library(lme4)
library(lmerTest)
library(vegan)
library(emmeans)
theme_set(theme_bw(20))

sp<-read.csv("SpComp_2014-2019.CSV") %>%
  mutate(plot=str_sub(Plot, -1)) %>% 
  select(-Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

trts<-read.csv("GF_PlotList_Trts.csv")

sp2<-sp %>% 
  left_join(trts)

richeven<-community_structure(sp2, time.var="Year", replicate.var = "plot_id", abundance.var = "pcover") %>% 
  left_join(trts)

###################################
###analysis richness
######################################

fit <- lmer(richness ~  Burn.Trt*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Year!=2014))
anova(fit, ddf="Kenward-Roger")

emmeans(fit, pairwise~Burn.Trt*Litter, adjust="holm")
emmeans(fit, pairwise~Nutrient, adjust="holm")

###analysis annual burn only
fit1 <- lmer(richness ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(richness ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==20))
anova(fit20)

##################
########Doing the analysis of how to make annual to unburned and vice-versa
####################

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

#no difference in # stems richness burn trts
fit14 <- lmer(richness ~  Burn.Trt2 +(1|Watershed/Block), data = r14)
anova(fit14)

#there is a difference in richness by watershed
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

##############################
#####Analysis of overall treatments
###########################

richMLit<-richeven %>% 
  rename(Burn=Burn.Trt) %>% 
  filter(Year!=2014) %>% 
  group_by(Burn, Litter) %>%
  summarize(mrich=mean(richness, na.rm=T), sddev=sd(richness), n=length(richness)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=richMLit, aes(x=Burn, y=mrich, fill=Litter))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(values=c('gold', 'darkgoldenrod4'), labels=c("Absent", "Present"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=0.75, y=13, label="C", size=5)+
  annotate("text", x=1.25, y=12, label="D", size=5)+
  annotate("text", x=1.75, y=17, label="A", size=5)+
  annotate("text", x=2.25, y=14, label="B", size=5)+
  xlab('Burn Treatment')+
  ylab('Species Richness')  

richMN<-richeven %>% 
  filter(Year!=2014) %>% 
  group_by(Nutrient) %>%
  summarize(mrich=mean(richness, na.rm=T), sddev=sd(richness), n=length(richness)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=richMN, aes(x=Nutrient, y=mrich, fill=Nutrient))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(values=c("orangered", "orange", 'orangered4'))+
  scale_x_discrete(limits=c("S", 'C', 'U'), labels=c("-N", 'C','+N'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  annotate("text", x=1, y=14, label="A", size=5)+
  annotate("text", x=2, y=14, label="A", size=5)+
  annotate("text", x=3, y=13, label="B", size=5)+
  xlab('Nutrient Treatment')+
  ylab('Species Richness') 

###########################
##Evenness
############################

fit <- lmer(Evar ~  Burn.Trt*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Year!=2014))
anova(fit, ddf="Kenward-Roger")
emmeans(fit, pairwise~Litter, adjust="holm")
emmeans(fit, pairwise~Nutrient*Burn.Trt, adjust="holm")


###analysis annual burn only
fit1 <- lmer(Evar ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==1))
anova(fit1)

###analysis of unburned only
fit20 <- lmer(Evar ~  Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(richeven, Burn.Trt2==20))
anova(fit20)

##################
########Doing the analysis of how to make annual to unburned and vice-versa
####################

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
#####Analysis of overall treatments
###########################

evenMLit<-richeven %>% 
  group_by(Litter) %>%
  summarize(meven=mean(Evar, na.rm=T), sddev=sd(Evar), n=length(Evar)) %>% 
  mutate(se=sddev/sqrt(n))

ggplot(data=evenMLit, aes(x=Litter, y=meven, fill=Litter))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(values=c("gold", "darkgoldenrod4"))+
  scale_x_discrete(labels=c('Absent', 'Present'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  annotate("text", x=1, y=0.38, label="A", size=5)+
  annotate("text", x=2, y=0.38, label="B", size=5)+
  xlab('Litter Treatment')+
  ylab('Species Evenness') 

evenMN<-richeven %>% 
  filter(Year!=2014) %>% 
  group_by(Nutrient, Burn.Trt) %>%
  summarize(meven=mean(Evar, na.rm=T), sddev=sd(Evar), n=length(Evar)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(nut=ifelse(Nutrient=="S", 1, ifelse(Nutrient=="C", 2, 3)))

ggplot(data=evenMN, aes(x=Burn.Trt, y=meven, fill=as.factor(nut)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=meven-se, ymax=meven+se), width=0.1, position=position_dodge(0.9))+
  scale_fill_manual(values=c("orangered", "orange", 'orangered4'), labels=c("-N", "C", "+N"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  annotate("text", x=0.72, y=.35, label="B", size=5)+
  annotate('text', x=1, y=.38, label="A", size=5)+
  annotate("text", x=1.27, y=.34, label="B", size=5)+
  annotate("text", x=1.72, y=.42, label="A", size=5)+
  annotate("text", x=2, y=.41, label="A", size=5)+
  annotate("text", x=2.27, y=.39, label="B", size=5)+
  xlab('Burn Treatment')+
  ylab('Species Evenness') 


##############################
#Analysis of composition
############################

sp3<-sp2 %>% 
  mutate(trt2=paste(Burn.Trt, treatment, sep="::"))

###Unburned as the reference
unref<-multivariate_difference(sp3, time.var="Year", species.var="spnum", abundance.var = "pcover", replicate.var = "plot_id", treatment.var="trt2", reference.treatment = "Unburned::control_control")

unref2<-unref%>% 
  separate(trt22, into=c("Burn.Trt2", "treatment"), sep="::") %>% 
  filter(Burn.Trt2!="Unburned") %>% 
  mutate(treatment2=ifelse(treatment=="control_control", "Annual Burn", ifelse(treatment=="control_nitrogen", "Annual+Nitrogen", ifelse(treatment=="litadded_control", "Annual+Litter", ifelse(treatment=="litadded_nitrogen", "Annual+N+L", "drop"))))) %>% 
  filter(treatment2!="drop")

ggplot(data=unref2, aes(x=Year, y=composition_diff, color=treatment2))+
  geom_point(size=3)+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Compositon Difference\n to Unburned")+
  scale_color_manual(name="Treatment", breaks=c("Annual Burn", "Annual+Nitrogen", "Annual+Litter", "Annual+N+L"), values=c("black", "red", "blue", "darkorchid4"))

###Annual as the reference
annref<-multivariate_difference(sp3, time.var="Year", species.var="spnum", abundance.var = "pcover", replicate.var = "plot_id", treatment.var="trt2", reference.treatment = "Annual::control_control")

annref2<-annref%>% 
  separate(trt22, into=c("Burn.Trt2", "treatment"), sep="::") %>% 
  filter(Burn.Trt2!="Annual") %>% 
  mutate(treatment2=ifelse(treatment=="control_control", "Unburned", ifelse(treatment=="control_sugar", "Unburned-Nitrogen", ifelse(treatment=="litremoved_control", "Unburned-Litter", ifelse(treatment=="litremoved_sugar", "Unburned-N-L", "drop"))))) %>% 
  filter(treatment2!="drop")

ggplot(data=annref2, aes(x=Year, y=composition_diff, color=treatment2))+
  geom_point(size=3)+
  geom_line()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Compositon Difference \nto Annual Burn")+
  scale_color_manual(name="Treatment", breaks=c("Unburned", "Unburned-Nitrogen", "Unburned-Litter", "Unburned-N-L"), values=c("gray47", "red", "blue", "darkorchid4"))

#####looking at 2014 data
sp14<-sp3 %>% 
  filter(Year==2014) %>% 
  mutate(sp=paste(genus, species, sep="_")) %>% 
  select(Burn.Trt, Plot, sp, pcover) %>% 
  pivot_wider(names_from='sp', values_from='pcover', values_fill = 0)

plotinfo=sp14%>%
  select(Burn.Trt, Plot)

mds<-metaMDS(sp14[3:59],distance= "bray")

scores<-as.data.frame(mds$points) %>% 
  bind_cols(plotinfo)

ggplot(data=scores, aes(x=MDS1, y=MDS2, color=Burn.Trt))+
  geom_point(size=3)+
  scale_color_manual(name="Burn Frequency", values=c('black', 'gray47'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

####################
####NMDS of all the data
########################
sp4<-sp2 %>% 
  mutate(sp=paste(genus, species, sep="_")) %>% 
  select(Burn.Trt, Nutrient, Litter, Year, Plot, sp, pcover) %>% 
  pivot_wider(names_from='sp', values_from='pcover', values_fill = 0)

##can't get model to converge with all data. Will try with in each burn trt.
mds<-metaMDS(sp4[6:92],distance= "bray", trymax = 100)

#running a model in unburned grasslands - no convergence for all years
ub<-sp2 %>% 
  mutate(sp=paste(genus, species, sep="_")) %>% 
  filter(Burn.Trt=="Unburned") %>% 
  select(Burn.Trt, Nutrient, Litter, Year, Plot, sp, pcover) %>% 
  pivot_wider(names_from='sp', values_from='pcover', values_fill = 0)

plotinfo=ub%>%
  select(Burn.Trt, Nutrient, Litter, Year, Plot)

mds<-metaMDS(ub[6:71],distance= "bray", trymax = 100)

#####running a model for annually burned grasslands, also can't get convergence
an<-sp2 %>% 
  mutate(sp=paste(genus, species, sep="_")) %>% 
  filter(Burn.Trt=="Annual") %>% 
  select(Burn.Trt, Nutrient, Litter, Year, Plot, sp, pcover) %>% 
  pivot_wider(names_from='sp', values_from='pcover', values_fill = 0)

plotinfo=an%>%
  select(Burn.Trt, Nutrient, Litter, Year, Plot)

mds<-metaMDS(an[6:63],distance= "bray", trymax = 100)

####trying with just pre-trt and 2019
sp5<-sp2 %>% 
  filter(Year==2014|Year==2019) %>% 
  mutate(sp=paste(genus, species, sep="_")) %>% 
  select(Burn.Trt, Nutrient, Litter, Year, Plot, sp, pcover) %>% 
  pivot_wider(names_from='sp', values_from='pcover', values_fill = 0)

##can't get model to converge with all data. Will try with in each burn trt.
mds<-metaMDS(sp5[6:76],distance= "bray", trymax = 100)

plotinfo=sp5%>%
  select(Burn.Trt, Nutrient, Litter, Year, Plot)

scores<-as.data.frame(mds$points) %>% 
  bind_cols(plotinfo) %>% 
  mutate(trt=paste(Nutrient, Litter, sep="_"))

ggplot(data=scores, aes(x=MDS1, y=MDS2, color=as.factor(Year), shape=trt))+
  geom_point(size=3)+
  scale_color_manual(name="Burn Frequency", values=c('black', 'gray47'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Burn.Trt, ncol=1)

####looking at Andro
fit <- lmer(pcover ~  as.factor(Burn.Trt)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = subset(sp2, Year!=2014&spnum==2))
anova(fit, ddf='Kenward-Roger')

emmeans(fit, pairwise~as.factor(Burn)|Nutrient, adjust="holm")

mstemsN<-sp2 %>% 
  filter(Year!=2014, spnum==2) %>% 
  group_by(Litter, Nutrient) %>%
  summarize(mtot=mean(pcover, na.rm=T), sddev=sd(pcover), n=length(pcover)) %>% 
  mutate(se=sddev/sqrt(n)) %>% 
  mutate(Ntrt=ifelse(Nutrient=='S', 1, ifelse(Nutrient=='C', 2, 3)))

ggplot(data=mstemsN, aes(x=Litter, y=mtot, fill=as.factor(Ntrt)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mtot-se, ymax=mtot+se), width=0.1, position=position_dodge(0.9))+
  #scale_fill_manual(name='Nutrient\nTreatment', values=c('orange', "orangered", 'orangered4'),labels=c("-N", ' C', '+N'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=0.72, y=54, label="ABC", size=5)+
  annotate('text', x=1, y=56, label="ABC", size=5)+
  annotate("text", x=1.27, y=45, label="ABC", size=5)+
  annotate("text", x=1.72, y=30, label="C", size=5)+
  annotate("text", x=2, y=45, label="B", size=5)+
  annotate("text", x=2.27, y=50, label="A", size=5)+
  xlab('Burn Treatment')+
  ylab('Number of A. gerardii Stems')

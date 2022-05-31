setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
library(lme4)
library(lmerTest)
library(vegan)
library(emmeans)
theme_set(theme_bw(20))

light<-read.csv("Light_2014_2019.CSV")%>%
  filter(Season=="Early")%>%
  rename(plot=Plot) %>% 
  mutate(Plot=paste(Block, plot, sep=""))

trts<-read.csv("GF_PlotList_Trts.csv") %>% 
  select(-plot_id) %>% 
  rename(BurnFreq=Burn.Trt2)%>%
  select(-Burn.Trt)

light2<-light %>% 
  left_join(trts)

l2014=light2 %>% 
  filter(Year==2014|Year==2015) %>% 
  rename(LightReduction=CanopyEffect) %>% 
  select(Year, Watershed, Block, plot, BurnFreq, Plot, LightReduction, Litter, Nutrient, treatment)

ltrtyrs<-light2 %>% 
  filter(Year!=2014&Year!=2015) %>% 
  mutate(LightReduction=ifelse(Litter=="A", CanopyEffect, ifelse(Litter=="P", LitterCanopyEffect, 999))) %>% 
  select(Year, Watershed, Block, plot, BurnFreq, Plot, LightReduction, Litter, Nutrient, treatment)

light3<-l2014 %>% 
  bind_rows(ltrtyrs)

means<-light3 %>% 
  group_by(Year, BurnFreq, Litter) %>%
  summarize(Mlight=mean(LightReduction, na.rm=T), sdlight=sd(LightReduction), n=length(LightReduction)) %>% 
  mutate(se=sdlight/sqrt(n)) %>% 
  mutate(WSBurn=ifelse(BurnFreq==1, "Annually Burned", "Unburned"))

ggplot(data=means, aes(x=Year, y=Mlight, color=as.factor(Litter)))+
  geom_point(size=5)+
  geom_line()+
  geom_errorbar(aes(ymin=Mlight-se, ymax=Mlight+se), width=0.1)+
  scale_color_manual(name="Litter", values = c("yellow", "black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Pct. Light Reduction")+
  facet_wrap(~WSBurn, ncol=1)

###analysis of light
fit <- lmer(LightReduction ~  as.factor(BurnFreq)*Litter*Nutrient*as.factor(Year) +(1|Watershed/Block), data = filter(light3, Year!=2014))
anova(fit, ddf='Kenward-Roger')

emmeans(fit, pairwise~Litter|as.factor(Burn), adjust="holm")
emmeans(fit, pairwise~Nutrient*as.factor(Burn), adjust="holm")
emmeans(fit, pairwise~Nutrient, adjust="holm")

means<-light3 %>% 
  group_by(Nutrient) %>%
  summarize(Mlight=mean(LightReduction, na.rm=T), sdlight=sd(LightReduction, na.rm = T), n=length(LightReduction)) %>% 
  mutate(se=sdlight/sqrt(n))

ggplot(data=means, aes(x=Nutrient, y=Mlight, fill=Nutrient))+
  geom_bar(stat="identity")+
  geom_line()+
  geom_errorbar(aes(ymin=Mlight-se, ymax=Mlight+se), width=0.1, postition=position_dodge(0.9))+
  scale_fill_manual(values=c("darkolivegreen3", "darkolivegreen1", 'darkolivegreen'))+
  scale_x_discrete(limits=c("S", 'C', 'U'), labels=c("-N", 'C','+N'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  annotate("text", x=1, y=38, label="A", size=5)+
  annotate("text", x=2, y=34, label="B", size=5)+
  annotate("text", x=3, y=31, label="C", size=5)+
  xlab('Nutrient Treatment')+
  ylab("Pct. Light Infiltration")

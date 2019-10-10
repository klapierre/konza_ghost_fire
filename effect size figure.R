setwd("C:/Users/megha/Dropbox/Konza Research/GhostFire/DATA/GhostFire2014_Data")

library(tidyverse)
library(codyn)

dat<-read.csv("GF2014_AllVariables_BlockLevel.csv")
sig<-read.csv("GF_2014_ForFig.csv")%>%
  mutate(sig=ifelse(P.value<0.05, 1, 0))%>%
  rename(type=ï..type)
theme_set(theme_bw(12))

mean<-dat%>%
  select(-Watershed, -Block) %>% 
  group_by(Burn.Trt)%>%
  gather(measure, value, ANPP_Total:ammonium)%>%
  group_by(Burn.Trt, measure)%>%
  summarise(mean=mean(value, na.rm=T))%>%
  ungroup()%>%
  spread(Burn.Trt, mean)%>%
  mutate(diff=(Annual-Unburned)/Annual)%>%
  left_join(sig)%>%
  na.omit()%>%
  mutate(signif=ifelse(sig==1, "*",""))

ggplot(data=mean, aes(x=reorder(measure, -diff), y=diff, fill=type, label=signif))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("Measure")+
  ylab("Percent Difference (A-U/A)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values=c("black", "cornflowerblue", "chocolate", "chartreuse4", "burlywood4"), name="Category", labels=c("Abiotic", "Insects", "Microbes", "Plant Aboveground", "Plant Belowground"))+
  geom_text(aes(label=signif), color="black", size=7, vjust=0.7)
  
             
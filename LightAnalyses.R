setwd("C:\\Users\\mavolio2\\Dropbox\\Konza Research\\GhostFire\\DATA\\Compiled data\\")

library(tidyverse)
theme_set(theme_bw())

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

means<-light2 %>% 
  group_by(Year, BurnFreq, Litter) %>% 
  summarize(Mlight=mean(CanopyEffect), sdlight=sd(CanopyEffect), n=length(CanopyEffect)) %>% 
  mutate(se=sdlight/sqrt(n))

ggplot(data=means, aes(x=Year, y=Mlight, color=as.factor(BurnFreq), shape=Litter))+
  geom_point(size=5)+
  geom_line()+
  geom_errorbar(aes(ymin=Mlight-se, ymax=Mlight+se), width=0.1)+
  scale_color_manual(name="Watershed\nBurn Freq.", values = c("red", "black"))+
  scale_shape_manual(limits = c("Absent", "Present"), values=c(2,19))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Pct. Light Reduction")

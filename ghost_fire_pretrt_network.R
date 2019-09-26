################################################################################
##  ghost_fire_pretrt_network.R: Ecosystem coupling analysis with Ghost Fire pre-treatment data.
##  Author: Kimberly Komatsu
##  Date created: September 26, 2019
################################################################################

library(codyn)
library(Hmisc)
library(network)
library(igraph)
library(tidyverse)
options(stringsAsFactors = F) #turns off default that makes any strings into factors

#kim's desktop
setwd('C:\\Users\\la pierrek\\Dropbox (Smithsonian)\\konza projects\\Ghost Fire\\DATA\\GhostFire2014_Data')



##############################
### import and process data
allData <- read.csv('GF2014_AllVariables_PlotLevel.csv')%>%
  #keep subset of variables for network analysis (remove mycorr count here, because only have for half of plots)
  select(-PlantShannon, -ForbRelCov, -GrassRelCov, -WoodyRelCov, -UnkGrassCov2, -Insect_Shannon, -DryMass, -C.N, -N.P, -BG, -Phos, -NAG, -CBH, -LAP, -PHX, -POX, -PC1, -PC2, -PC3, -Mycorr_Count)%>%
  rename(BNPP=AFDM)

### get regressions for ash-free dry mass from the plots we did it for (only half plots had AFDM done for standing crop), then use to estimate AFDM from total dry mass for all plots
root <- allData%>%
  select(Burn.Trt, Watershed, Block, Plot, Scrop.gm2_DM, Scrop.gm2_AFDM)

summary(lm(Scrop.gm2_AFDM~Scrop.gm2_DM + Burn.Trt + Watershed, data=subset(root, !is.na(Scrop.gm2_AFDM)))) #sig effect of burn, so can pool all data to calculate AFDM estimates for all plots
with(subset(root, !is.na(Scrop.gm2_AFDM)), plot(Scrop.gm2_AFDM~Scrop.gm2_DM))

#unburned
summary(lm(Scrop.gm2_AFDM~Scrop.gm2_DM, data=subset(root, !is.na(Scrop.gm2_AFDM)&Burn.Trt=='Unburned'))) #AFDM = 92.6906 + 0.5736*dry_mass
with(subset(root, !is.na(Scrop.gm2_AFDM)&Burn.Trt=='Unburned'), plot(Scrop.gm2_AFDM~Scrop.gm2_DM))

#annual burn
summary(lm(Scrop.gm2_AFDM~Scrop.gm2_DM, data=subset(root, !is.na(Scrop.gm2_AFDM)&Burn.Trt=='Annual'))) #AFDM = 7110751 + 0.52960*dry_mass
with(subset(root, !is.na(Scrop.gm2_AFDM)&Burn.Trt=='Annual'), plot(Scrop.gm2_AFDM~Scrop.gm2_DM))

cleanData <- allData%>%
  mutate(root_SC=ifelse(Burn.Trt=='Unburned', (92.6906 + 0.5736*Scrop.gm2_DM), (7110751 + 0.52960*Scrop.gm2_DM)))%>%
  select(-Scrop.gm2_DM, -Scrop.gm2_AFDM)



##############################
#Unburned
##############################
### get correlation coefficients
unburnedData <- cleanData%>%
  filter(Burn.Trt=='Unburned')

#get p values
pMatrix <- rcorr(as.matrix(unburnedData[,-1:-4]), type='pearson') #calculate all possible correlations to get p values
p <- pMatrix$P
p[upper.tri(p, diag=T)] <- 'NA'

allP <- p%>%
  as.data.frame%>%
  rownames_to_column(var = 'var1')%>%
  gather(var2, value, -var1)%>%
  rename(p=value)
allMatrix <- as.matrix(unburnedData[,-1:-4])%>% 
  cor(method='pearson') #calculate all possible correlation
allMatrix[upper.tri(allMatrix, diag=T)] <- 'NA'

#get pearson correlation coefficients
allCorrelation <- allMatrix%>%
  as.data.frame%>%
  rownames_to_column(var = 'var1')%>%
  gather(var2, value, -var1)%>%
  rename(pearson=value)%>%
  left_join(allP)%>% #join with p-values
  filter(pearson!='NA')%>%
  mutate(pearson_sig=ifelse(p>0.05, 0, pearson))

#write.csv(allCorrelation, 'unburned_correlation_matrix.csv')

#calculate average correlation coefficient
unburnedavgCorrelationSig <- mean(abs(as.numeric(allCorrelation$pearson_sig))) #0.05749341
unburnedavgCorrelation <- mean(abs(as.numeric(allCorrelation$pearson))) #0.1784711



##############################
### make figures

#get list of nodes
nodes <- allCorrelation%>%
  mutate(label=as.character(var1))%>%
  select(label)%>%
  unique()%>%
  rowid_to_column('id')
nodes[nrow(nodes)+1,] = list(29,'root_SC') #add in root standing crop, which is the one variable not in var1 column (but is in var 2 column)


#get list of edge strengths
edges <- allCorrelation%>%
  left_join(nodes, by=c('var1'='label'))%>%
  select(-var1)%>%
  mutate(var1=as.integer(id))%>%
  select(-id)%>%
  left_join(nodes, by=c('var2'='label'))%>%
  select(-var2)%>%
  mutate(var2=as.integer(ifelse(!is.na(id), id, 29)))%>%
  select(var1, var2, pearson_sig)%>%
  filter(pearson_sig!=0)

###plot network
network <- network(edges, vertex.attr=nodes, matrix.type='edgelist', ignore.eval=T)

plot.network(network, mode='circle', usearrows=F, vertex.cex=2, vertex.col=nodes$color, edge.lwd=1, edge.col='#626363', displaylabels=T)





##############################
#Annual Burn
##############################
### get correlation coefficients
annualData <- cleanData%>%
  filter(Burn.Trt=='Annual')

#get p values
pMatrix <- rcorr(as.matrix(annualData[,-1:-4]), type='pearson') #calculate all possible correlations to get p values
p <- pMatrix$P
p[upper.tri(p, diag=T)] <- 'NA'

allP <- p%>%
  as.data.frame%>%
  rownames_to_column(var = 'var1')%>%
  gather(var2, value, -var1)%>%
  rename(p=value)
allMatrix <- as.matrix(annualData[,-1:-4])%>% 
  cor(method='pearson') #calculate all possible correlation
allMatrix[upper.tri(allMatrix, diag=T)] <- 'NA'

#get pearson correlation coefficients
allCorrelation <- allMatrix%>%
  as.data.frame%>%
  rownames_to_column(var = 'var1')%>%
  gather(var2, value, -var1)%>%
  rename(pearson=value)%>%
  left_join(allP)%>% #join with p-values
  filter(pearson!='NA')%>%
  mutate(pearson_sig=ifelse(p>0.05, 0, pearson))

#write.csv(allCorrelation, 'annual_correlation_matrix.csv')

#calculate average correlation coefficient
annualavgCorrelationSig <- mean(abs(as.numeric(allCorrelation$pearson_sig))) #0.1235768
annualavgCorrelation <- mean(abs(as.numeric(allCorrelation$pearson))) #0.2426026



##############################
### make figures

#get list of nodes
nodes <- allCorrelation%>%
  mutate(label=as.character(var1))%>%
  select(label)%>%
  unique()%>%
  rowid_to_column('id')
nodes[nrow(nodes)+1,] = list(29,'root_SC') #add in root standing crop, which is the one variable not in var1 column (but is in var 2 column)


#get list of edge strengths
edges <- allCorrelation%>%
  left_join(nodes, by=c('var1'='label'))%>%
  select(-var1)%>%
  mutate(var1=as.integer(id))%>%
  select(-id)%>%
  left_join(nodes, by=c('var2'='label'))%>%
  select(-var2)%>%
  mutate(var2=as.integer(ifelse(!is.na(id), id, 29)))%>%
  select(var1, var2, pearson_sig)%>%
  filter(pearson_sig!=0)

###plot network
network <- network(edges, vertex.attr=nodes, matrix.type='edgelist', ignore.eval=T)

plot.network(network, mode='circle', usearrows=F, vertex.cex=2, vertex.col=nodes$color, edge.lwd=1, edge.col='#626363', displaylabels=T)























###prelim regression plots
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=30, vjust=-0.8), axis.text.x=element_text(size=25),
             axis.title.y=element_text(size=30, angle=90, vjust=0.7, margin=margin(l=0,r=10,t=0,b=0)), axis.text.y=element_text(size=25),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

ggplot(data=allData, aes(x=LMA, y=beetle_richness)) + #sig
  geom_point(color='#c17313', size=5) + 
  geom_smooth(method='lm', se=F, color='#c17313', size=2) +
  xlab('Leaf Mass Area') + ylab('Ground Beetle\nRichness') +
  annotate("text", x=52, y=3.42, label='r = 0.919\np = 0.003', size=6)
#export at 600x400

ggplot(data=allData, aes(x=LMA, y=beetle_count)) + #non-sig
  geom_point(color='#60605e', size=5) + 
  xlab('Leaf Mass Area') + ylab('Ground Beetle\nAbundance') +
  annotate("text", x=73, y=6, label='r = 0.189\np = 0.685', size=6)
#export at 600x400

ggplot(data=allData, aes(x=soil_ph, y=soil_P)) + #sig
  geom_point(color='#c00000', size=5) + 
  geom_smooth(method='lm', se=F, color='#c00000', size=2) +
  xlab('Soil pH') + ylab('\nSoil P') +
  annotate("text", x=6.2, y=275, label='r = 0.763\np = 0.046', size=6)
#export at 600x400
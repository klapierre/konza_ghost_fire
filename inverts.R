################################################################################
##  inverts.R: Community diversity metrics and composition for invertebrates in the
##             Konza Prairie LTER Ghost Fire experiment.
##
##  Authors: K. Komatsu
##  Date created: Sept 106, 2025
################################################################################

library(codyn)
library(vegan)
library(gridExtra)
library(tidyverse)

setwd('C:\\Users\\kjkomatsu\\Smithsonian Dropbox\\Kimberly Komatsu\\konza projects\\Ghost Fire')

# functions ---------------------------------------------------------------
# bar graph summary statistics
# barGraphStats(data=, variable="", byFactorNames=c(""))

barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

# graph theme set ---------------------------------------------------------------
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_text(size=20), legend.text=element_text(size=20))


# data ---------------------------------------------------------------

trt <- read.csv('SiteLocation & Exp Design\\GF_PlotList.csv') %>% 
  select(-Plot, -Burn.Trt2, -plot_id) %>% 
  rename(watershed=Watershed, burn_trt=Burn.Trt, block=Block, plot=plot_num, litter=Litter, nutrient=Nutrient)

invertComp2014 <- read.csv('DATA\\GhostFire2014_Data\\Invertebrates\\La Pierre_ghost fire_invert_community_2014.csv') %>% 
  select(-burn_trt)
invertComp2019 <- read.csv('DATA\\GhostFire2019_Data\\Invertebrates\\ghost_fire_invert_community_2019.csv') %>% 
  select(-plot_trt, -litter_trt, -burn_trt) %>% mutate(stage=str_to_lower(stage), collected=str_to_lower(collected))
invertComp2024 <- read.csv('DATA\\GhostFire2024_Data\\Invertebrates\\ghost_fire_invert_community_2024.csv') %>% 
  select(-plot_trt, -litter_trt, -burn_trt) %>% mutate(stage=str_to_lower(stage), collected=str_to_lower(collected))

invertComp <- rbind(invertComp2014, invertComp2019, invertComp2024) %>% 
  left_join(trt) %>% 
  mutate(replicate=paste(block, plot, sep='')) %>% 
  group_by(year, watershed, block, plot, replicate, litter, nutrient, treatment, burn_trt, arthropod_ID) %>% 
  summarise(total_count = sum(count), .groups='drop') #sum across counts of nymphs/adults and collected/observed


# calculate overall metrics ---------------------------------------------------------------

invertAbund <- invertComp %>% 
  group_by(year, watershed, replicate, block, plot, burn_trt, litter, nutrient) %>% 
  summarize(abundance=sum(total_count), .groups='drop')

invertMetrics <- community_structure(df=invertComp, time.var='year', abundance.var='total_count', replicate.var='replicate') %>% 
  left_join(invertAbund)

invertMetrics$nutrient <- factor(invertMetrics$nutrient, levels=c('S','C','U'))
invertMetrics$litter <- factor(invertMetrics$litter, levels=c('P','A'))
invertMetrics$burn_trt <- factor(invertMetrics$burn_trt, levels=c('Annual','Unburned'))



# pre-treatment figures ---------------------------------------------------------------

richnessFig <- ggplot(barGraphStats(data=subset(invertMetrics, year==2014), variable="richness", byFactorNames=c("burn_trt")), 
                      aes(x=burn_trt, y=mean, fill=burn_trt)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkgreen','tan'), ) +
  xlab('Burn') + ylab('Invertebrate Richness') +
  theme(legend.position='none')

evennessFig <- ggplot(barGraphStats(data=subset(invertMetrics, year==2014), variable="Evar", byFactorNames=c("burn_trt")), 
                      aes(x=burn_trt, y=mean, fill=burn_trt)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkgreen','tan'), ) +
  xlab('Burn') + ylab('Invertebrate Evenness') +
  theme(legend.position='none')

abundanceFig <- ggplot(barGraphStats(data=subset(invertMetrics, year==2014), variable="richness", byFactorNames=c("burn_trt")), 
                       aes(x=burn_trt, y=mean, fill=burn_trt)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkgreen','tan'), ) +
  xlab('Burn') + ylab('Invertebrate Abundance') +
  theme(legend.position='none')

grid.arrange(nrow=1, ncol=3,
             richnessFig, evennessFig, abundanceFig)
#export at 1000x500


# treatment year figures ---------------------------------------------------------------

richnessFig <- ggplot(barGraphStats(data=subset(invertMetrics, year!=2014), variable="richness", byFactorNames=c("nutrient","litter","burn_trt")), 
                      aes(x=interaction(nutrient, litter), y=mean, fill=nutrient)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkolivegreen1','darkolivegreen3','darkolivegreen'), ) +
  geom_vline(xintercept=3.5) +
  xlab('Treatment') + ylab('Invertebrate Richness') +
  facet_wrap(~burn_trt)

evennessFig <- ggplot(barGraphStats(data=subset(invertMetrics, year!=2014), variable="Evar", byFactorNames=c("nutrient","litter","burn_trt")), 
                      aes(x=interaction(nutrient, litter), y=mean, fill=nutrient)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkolivegreen1','darkolivegreen3','darkolivegreen'), ) +
  geom_vline(xintercept=3.5) +
  xlab('Treatment') + ylab('Invertebrate Evenness') +
  facet_wrap(~burn_trt)

abundanceFig <- ggplot(barGraphStats(data=subset(invertMetrics, year!=2014), variable="richness", byFactorNames=c("nutrient","litter","burn_trt")), 
                      aes(x=interaction(nutrient, litter), y=mean, fill=nutrient)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  scale_fill_manual(values=c('darkolivegreen1','darkolivegreen3','darkolivegreen'), ) +
  geom_vline(xintercept=3.5) +
  xlab('Treatment') + ylab('Invertebrate Abundance') +
  facet_wrap(~burn_trt)

grid.arrange(nrow=3, ncol=1,
             richnessFig, evennessFig, abundanceFig)
#export at 1000x1500




# PERMANOVAs ----------------------------------------------------------

#PERMANOVA 2014 - burn trts
comm2014 <- invertComp %>% 
  filter(year==2014) %>%  
  select(year, watershed, block, plot, burn_trt, arthropod_ID, total_count) %>% 
  pivot_wider(names_from='arthropod_ID', values_from = 'total_count', values_fill = 0)  

permanova <- adonis2(formula = comm2014[,6:47] ~ burn_trt, 
                    data=comm2014, permutations=999, method="bray",
                    by='margin') 
print(permanova)

#plot NMDS 2014
sppBC <- metaMDS(comm2014[,6:47])

plotData <- comm2014[,1:5]

#Use the vegan ellipse function to make ellipses
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

BC_NMDS = data.frame(MDS1 = sppBC$points[,1], MDS2 = sppBC$points[,2], group=comm2014$burn_trt)
BC_NMDS_Graph <- cbind(plotData,BC_NMDS)

plot(sppBC, type = "n")  # type="n" plots empty axes (no points yet)
points(sppBC, display = "sites", col = as.numeric(plotData$burn_trt))

BC_Ord_Ellipses <- ordiellipse(sppBC, plotData$burn_trt, display = "sites",
                             kind = "se", conf = 0.95, label = T)               

ord3 <- data.frame(plotData, scores(sppBC,display="sites"))%>%
  group_by(burn_trt)

BC_Ellipses <- data.frame() 
for(g in unique(BC_NMDS$group)){
  BC_Ellipses <- rbind(BC_Ellipses, cbind(as.data.frame(with(BC_NMDS[BC_NMDS$group==g,], 
                                                             veganCovEllipse(BC_Ord_Ellipses[[g]]$cov,
                                                                             BC_Ord_Ellipses[[g]]$center,
                                                                             BC_Ord_Ellipses[[g]]$scale)))
                                          ,group=g))
} #Generate ellipses points

ggplot(subset(BC_NMDS_Graph), aes(x=MDS1, y=MDS2)) +
  geom_point(size=6, aes(color=burn_trt)) +  # Color points by burn_trt
  geom_path(data = filter(BC_Ellipses), 
            aes(x = NMDS1, y = NMDS2, color = group),  # Color ellipses by burn_trt
            size = 3) +
  labs(color="Burn Treatment", linetype = "", shape = "") +
  scale_color_manual(values=c("#de1a24", "#056517")) +  # Custom colors for treatments
  xlab("NMDS1") + 
  ylab("NMDS2") + 
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        legend.text = element_text(size = 22))
#export at 700x500




#PERMANOVA 2019+2024 - expt trts
commTrt <- invertComp %>% 
  filter(year!=2014) %>%  
  select(year, watershed, block, plot, burn_trt, litter, nutrient, arthropod_ID, total_count) %>%   
  mutate(replicate=paste(watershed,block,plot, sep='_')) %>% 
  pivot_wider(names_from='arthropod_ID', values_from = 'total_count', values_fill = 0)

permanova <- adonis2(formula = commTrt[,9:185] ~ year*burn_trt*litter*nutrient, 
                    strata=commTrt$replicate,
                    data=commTrt, permutations=999, method="bray",
                    by='terms') 
print(permanova) 

#plot NMDS 2019+2024
sppBC <- metaMDS(commTrt[,9:185])

plotData <- commTrt[,1:8]

#Use the vegan ellipse function to make ellipses
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

BC_NMDS = data.frame(MDS1 = sppBC$points[,1], MDS2 = sppBC$points[,2],
                     burn_trt = commTrt$burn_trt,
                     nutrient = commTrt$nutrient,
                     litter   = commTrt$litter,
                     treatment= interaction(commTrt$nutrient, commTrt$litter))
BC_NMDS_Graph <- cbind(plotData, BC_NMDS)

plot(sppBC, type = "n")  # type="n" plots empty axes (no points yet)
points(sppBC, display = "sites", col = as.numeric(plotData$burn_trt))

BC_Ord_Ellipses_nutrient <- ordiellipse(sppBC, plotData$nutrient, display = "sites",
                                        kind = "se", conf = 0.95, label = T)               

BC_Ord_Ellipses_litter <- ordiellipse(sppBC, plotData$litter, display = "sites",
                                        kind = "se", conf = 0.95, label = T)  

BC_Ord_Ellipses_treatment <- ordiellipse(sppBC, interaction(plotData$nutrient,plotData$litter), display = "sites",
                                      kind = "se", conf = 0.95, label = T)  

ord3 <- data.frame(plotData, scores(sppBC, display="sites"))%>%
  group_by(burn_trt, nutrient, litter)

BC_Ellipses <- data.frame() 
for(g in unique(BC_NMDS$treatment)){
  BC_Ellipses <- rbind(BC_Ellipses, cbind(as.data.frame(with(BC_NMDS[BC_NMDS$treatment==g,], 
                                                             veganCovEllipse(BC_Ord_Ellipses_treatment[[g]]$cov,
                                                                             BC_Ord_Ellipses_treatment[[g]]$center,
                                                                             BC_Ord_Ellipses_treatment[[g]]$scale)))
                                          ,treatment=g))
} #Generate ellipses points

ggplot(subset(BC_NMDS_Graph), aes(x=MDS1, y=MDS2)) +
  geom_point(size=6, aes(color=nutrient, shape=litter)) + 
  geom_path(data = filter(BC_Ellipses), 
            aes(x = NMDS1, y = NMDS2, color = treatment),  
            size = 3) +
  labs(color="Treatment", linetype = "", shape = "") +
  # scale_color_manual(values=c("#de1a24", "#056517")) +  # Custom colors for treatments
  xlab("NMDS1") + 
  ylab("NMDS2") + 
  facet_wrap(~burn_trt) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"),
        legend.text = element_text(size = 22))
#export at 700x500
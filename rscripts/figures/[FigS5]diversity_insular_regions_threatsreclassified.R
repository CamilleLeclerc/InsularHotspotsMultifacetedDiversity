##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(cowplot)
library(dplyr)
library(genefilter)
library(ggalt)
library(ggmap)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggtern)
library(jcolors)
library(maptools)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sf)
library(shapefiles)
library(tidyr)
library(tricolore)


theme_niwot <- function(){
  theme_bw() +
    theme(#text = element_text(family = "Helvetica Light"),
      axis.text = element_text(size = 13, colour="black"), 
      axis.title = element_text(size = 13, colour="black", face="bold"),
      axis.line.x = element_line(color="black"), 
      axis.line.y = element_line(color="black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),                                          
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
      plot.title = element_text(size = 18, vjust = 1, hjust = 0),
      legend.text = element_text(size = 12),          
      legend.title = element_blank(),                              
      legend.position = c(0.95, 0.15), 
      legend.key = element_blank(),
      legend.background = element_rect(color = "black", 
                                       fill = "transparent", 
                                       size = 2, linetype = "blank"))
}


##--------------
## LOAD DATASETS
##--------------
Info_Archip_Threat <- read.csv("outputs/Info_Archip_Threatreclassified.txt", sep="")

TD <- Info_Archip_Threat %>% select(Archip, Threat, Nb_sp_thr)
TD.mixed <- TD %>% filter(Threat == "mixed") ; TD.mixed <- TD.mixed[,c(1,3)] ; colnames(TD.mixed) <- c("Archip", "TD.mixed")
TD.habitat <- TD %>% filter(Threat == "habitat") ; TD.habitat <- TD.habitat[,c(1,3)] ; colnames(TD.habitat) <- c("Archip", "TD.habitat")
TD.direct <- TD %>% filter(Threat == "direct") ; TD.direct <- TD.direct[,c(1,3)] ; colnames(TD.direct) <- c("Archip", "TD.direct")
TD <- left_join(TD.habitat, TD.direct, by = "Archip")
TD <- left_join(TD, TD.mixed, by = "Archip")
rm(TD.mixed, TD.direct, TD.habitat)
TD$relat.TD.mixed <- TD$TD.mixed/rowSums(TD[,2:4], na.rm = TRUE)
TD$relat.TD.direct <- TD$TD.direct/rowSums(TD[,2:4], na.rm = TRUE)
TD$relat.TD.habitat <- TD$TD.habitat/rowSums(TD[,2:4], na.rm = TRUE)

PD <- Info_Archip_Threat %>% select(Archip, Threat, PD)
PD.mixed <- PD %>% filter(Threat == "mixed") ; PD.mixed <- PD.mixed[,c(1,3)] ; colnames(PD.mixed) <- c("Archip", "PD.mixed")
PD.habitat <- PD %>% filter(Threat == "habitat") ; PD.habitat <- PD.habitat[,c(1,3)] ; colnames(PD.habitat) <- c("Archip", "PD.habitat")
PD.direct <- PD %>% filter(Threat == "direct") ; PD.direct <- PD.direct[,c(1,3)] ; colnames(PD.direct) <- c("Archip", "PD.direct")
PD <- left_join(PD.habitat, PD.direct, by = "Archip")
PD <- left_join(PD, PD.mixed, by = "Archip")
rm(PD.mixed, PD.direct, PD.habitat)
PD$relat.PD.mixed <- PD$PD.mixed/rowSums(PD[,2:4], na.rm = TRUE)
PD$relat.PD.direct <- PD$PD.direct/rowSums(PD[,2:4], na.rm = TRUE)
PD$relat.PD.habitat <- PD$PD.habitat/rowSums(PD[,2:4], na.rm = TRUE)

FD <- Info_Archip_Threat %>% select(Archip, Threat, FRic)
FD.mixed <- FD %>% filter(Threat == "mixed") ; FD.mixed <- FD.mixed[,c(1,3)] ; colnames(FD.mixed) <- c("Archip", "FD.mixed")
FD.habitat <- FD %>% filter(Threat == "habitat") ; FD.habitat <- FD.habitat[,c(1,3)] ; colnames(FD.habitat) <- c("Archip", "FD.habitat")
FD.direct <- FD %>% filter(Threat == "direct") ; FD.direct <- FD.direct[,c(1,3)] ; colnames(FD.direct) <- c("Archip", "FD.direct")
FD <- left_join(FD.habitat, FD.direct, by = "Archip")
FD <- left_join(FD, FD.mixed, by = "Archip")
rm(FD.mixed, FD.direct, FD.habitat)
FD$relat.FD.mixed <- FD$FD.mixed/rowSums(FD[,2:4], na.rm = TRUE)
FD$relat.FD.direct <- FD$FD.direct/rowSums(FD[,2:4], na.rm = TRUE)
FD$relat.FD.habitat <- FD$FD.habitat/rowSums(FD[,2:4], na.rm = TRUE)


Diversity <- left_join(TD, PD, by = "Archip")
Diversity <- left_join(Diversity, FD, by = "Archip")
Diversity <- Diversity %>% select(Archip, relat.TD.mixed, relat.TD.direct, relat.TD.habitat, relat.PD.mixed, relat.PD.direct, relat.PD.habitat, relat.FD.mixed, relat.FD.direct, relat.FD.habitat)
Diversity$relat.div.mixed <- rowSums(Diversity[,c(2,5,8)], na.rm = TRUE)/3
Diversity$relat.div.direct <- rowSums(Diversity[,c(3,6,9)], na.rm = TRUE)/3
Diversity$relat.div.habitat <- rowSums(Diversity[,c(4,7,10)], na.rm = TRUE)/3
rowSums(Diversity[,11:13], na.rm = TRUE)

Diversity[13,11] <- (Diversity[13,2] + Diversity[13,5])/2
Diversity[13,12] <- 0
Diversity[13,13] <- (Diversity[13,4] + Diversity[13,7])/2
Diversity[14,11] <- (Diversity[14,2] + Diversity[14,5])/2
Diversity[14,12] <- (Diversity[14,3] + Diversity[14,6])/2
Diversity[14,13] <- (Diversity[14,4] + Diversity[14,7])/2
Diversity[16,11] <- (Diversity[16,2] + Diversity[16,5])/2
Diversity[16,12] <- (Diversity[16,3] + Diversity[16,6])/2
Diversity[16,13] <- (Diversity[16,4] + Diversity[16,7])/2
rowSums(Diversity[,11:13], na.rm = TRUE)
rm(FD, Info_Archip_Threat, PD, TD)
Diversity <- Diversity %>% select(Archip, relat.div.mixed, relat.div.direct, relat.div.habitat)


islands.of.endemic.mammals <- read.csv("data/islands_of_endemic_mammals.txt", sep="")
head(islands.of.endemic.mammals)
islands.of.endemic.mammals <- unique(islands.of.endemic.mammals %>% select(ULM_ID, REGION, Area))

iucn.mammals.endemic.trait <- read.csv("data/iucn_mammals_endemic_trait.txt", sep="")
head(iucn.mammals.endemic.trait)
iucn.mammals.endemic.trait <- unique(iucn.mammals.endemic.trait %>% select(binomial, statut, category, foraging.niche, body.mass, habitat.breadth, main.diet, foraging.period))

iucn.mammals.insular <- read.delim("data/iucn_mammals_insular.txt")
head(iucn.mammals.insular)
iucn.mammals.insular <- unique(iucn.mammals.insular %>% select(binomial, ulm_id))
colnames(iucn.mammals.insular)[2] <- "ULM_ID"

iucn.mammals.endemic.conservation <- read.delim("data/conservation_of_endemic_mammals.txt")
iucn.mammals.endemic.conservation <- unique(iucn.mammals.endemic.conservation %>% select(binomial, populationTrend))

data.endemic.mammals <- left_join(iucn.mammals.endemic.trait, iucn.mammals.insular, by = "binomial")
data.endemic.mammals <- left_join(data.endemic.mammals, iucn.mammals.endemic.conservation, by = "binomial")
data.endemic.mammals <- left_join(data.endemic.mammals, islands.of.endemic.mammals, by = "ULM_ID")
head(data.endemic.mammals)
unique(data.endemic.mammals$statut)
unique(data.endemic.mammals$category)
data.endemic.mammals <- data.endemic.mammals %>% filter(category != "EX")
data.endemic.mammals <- data.endemic.mammals[!is.na(data.endemic.mammals$REGION),]
length(unique(data.endemic.mammals$binomial))
length(unique(data.endemic.mammals$REGION))
length(unique(data.endemic.mammals$ULM_ID))

rm(islands.of.endemic.mammals, iucn.mammals.endemic.trait, iucn.mammals.insular, iucn.mammals.endemic.conservation)




SAR_TD_Archip <- read.csv("outputs/DAR_TD_Archip.txt", sep="")
SAR_PD_Archip <- read.csv("outputs/DAR_PD_Archip.txt", sep="")
SAR_FD_Archip <- read.csv("outputs/DAR_FD_Archip.txt", sep="")

data_diversity <- SAR_TD_Archip[,c(1, 3)]
colnames(data_diversity) <- c("Archip", "TD_obs")
data_diversity <- left_join(data_diversity, SAR_FD_Archip[,c(1, 3)], by="Archip")
colnames(data_diversity)[3] <- "FD"
data_diversity <- left_join(data_diversity, SAR_PD_Archip[,c(1, 3)], by="Archip")
colnames(data_diversity)[4] <- "PD"
data_diversity <- data_diversity[order(data_diversity$Archip, decreasing=F), ]
data_diversity$Archip


data_diversity$Mean_diversity <- NA
data_diversity$Upper_diversity <- NA
data_diversity$Lower_diversity <- NA

for (i in 1:nrow(data_diversity)) {
  
  sub_data_diversity <- data_diversity[i,]
  data_diversity$Mean_diversity[i] <- rowMeans(sub_data_diversity[,2:4], na.rm = TRUE)
  data_diversity$Upper_diversity[i] <- data_diversity$Mean_diversity[i] + rowSds(sub_data_diversity[,2:4], na.rm = TRUE)
  data_diversity$Lower_diversity[i] <- data_diversity$Mean_diversity[i] - rowSds(sub_data_diversity[,2:4], na.rm = TRUE)
  
}
rm(sub_data_diversity, i)




TD_Rank <- SAR_TD_Archip[,c(1,6)]
PD_Rank <- SAR_PD_Archip[,c(1,6)]
FD_Rank <- SAR_FD_Archip[,c(1,6)]

Hotspot_Rank_Sum <- TD_Rank
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, PD_Rank, by = "Archip")
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, FD_Rank, by = "Archip")
colnames(Hotspot_Rank_Sum) <- c("Archip", "TD_Rank", "PD_Rank", "FD_Rank")
Hotspot_Rank_Sum$Tot_rank <- rowSums(Hotspot_Rank_Sum[,2:4], na.rm = TRUE)
Hotspot_Rank_Sum <- Hotspot_Rank_Sum[order(Hotspot_Rank_Sum$Tot_rank, decreasing=F), ]
Hotspot_Rank_Sum$Rank_final <- c(1:6, 8, 7, 9:19)
data_diversity <- left_join(data_diversity, Hotspot_Rank_Sum[,c(1, 6)], by="Archip")

rm(SAR_TD_Archip, SAR_PD_Archip, SAR_FD_Archip, TD_Rank, PD_Rank, FD_Rank, Hotspot_Rank_Sum, data.endemic.mammals)


data_diversity <- data_diversity %>% select(Archip, Mean_diversity)

data <- left_join(Diversity, data_diversity, by = "Archip")




threat.mixed <- ggplot(data = data, aes(x = Mean_diversity, y = relat.div.mixed)) +
                      geom_smooth(method = 'loess', formula = 'y ~ x', span = 1, color = "black") +
                      geom_point(aes(size = Mean_diversity, fill = Mean_diversity), shape = 21) +
                      scale_fill_distiller(palette = "Spectral") + xlim(0, 1) + scale_y_continuous(limits = c(-0.2, 1), breaks = seq(0, 1, by = 0.25)) +
                      geom_text_repel(aes(label = c("PNG", "MIOI", "MGI", "CrI", "Ja", "GGI", "Su", "Wa", "Ph", "MB", "SL", "IB", "BESSI", "Ta", "EMI", "NC", "PM", "ClI", "TNZI")), size = 4, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                      xlab("Standardized threatened diversity ") + ylab("Representativeness of mixed threats") + theme_niwot() + theme(legend.position = "none")  
threat.direct <- ggplot(data = data, aes(x = Mean_diversity, y = relat.div.direct)) +
                      geom_smooth(method = 'loess', formula = 'y ~ x', span = 2, color = "black") +
                      geom_point(aes(size = Mean_diversity, fill = Mean_diversity), shape = 21) +
                      scale_fill_distiller(palette = "Spectral") + xlim(0, 1) + scale_y_continuous(limits = c(-0.2, 1), breaks = seq(0, 1, by = 0.25)) +
                      geom_text_repel(aes(label = c("PNG", "MIOI", "MGI", "CrI", "Ja", "GGI", "Su", "Wa", "Ph", "MB", "SL", "IB", "BESSI", "Ta", "EMI", "NC", "PM", "ClI", "TNZI")), size = 4, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                      xlab("Standardized threatened diversity ") + ylab("Representativeness of direct threats") + theme_niwot() + theme(legend.position = "none")  
threat.habitat <- ggplot(data = data, aes(x = Mean_diversity, y = relat.div.habitat)) +
                      geom_smooth(method = 'loess', formula = 'y ~ x', span = 2, color = "black") +
                      geom_point(aes(size = Mean_diversity, fill = Mean_diversity), shape = 21) +
                      scale_fill_distiller(palette = "Spectral") + xlim(0, 1) + scale_y_continuous(limits = c(-0.2, 1), breaks = seq(0, 1, by = 0.25)) +
                      geom_text_repel(aes(label = c("PNG", "MIOI", "MGI", "CrI", "Ja", "GGI", "Su", "Wa", "Ph", "MB", "SL", "IB", "BESSI", "Ta", "EMI", "NC", "PM", "ClI", "TNZI")), size = 4, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                      xlab("Standardized threatened diversity ") + ylab("Representativeness of habitat threats") + theme_niwot() + theme(legend.position = "none")  

ggarrange(threat.mixed, threat.direct, threat.habitat, ncol = 3, nrow = 1, labels = c("(a)", "(b)", "(c)"))


##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(cowplot)
library(dplyr)
library(genefilter)
library(ggalt)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(jcolors)
library(RColorBrewer)


#theme
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
Archip_PAs <- read.csv("outputs/regions_PAs.txt", sep="")

data_residuals <- SAR_TD_Archip[,c(1, 3, 5)]
colnames(data_residuals) <- c("Archip", "TD_obs", "Resid_TD")
data_residuals <- left_join(data_residuals, SAR_FD_Archip[,c(1, 3, 5)], by="Archip")
colnames(data_residuals)[4] <- "FD"
colnames(data_residuals)[5] <- "Resid_FD"
data_residuals <- left_join(data_residuals, SAR_PD_Archip[,c(1, 3, 5)], by="Archip")
colnames(data_residuals)[6] <- "PD"
colnames(data_residuals)[7] <- "Resid_PD"
data_residuals <- left_join(data_residuals, Archip_PAs[,c(1, 5)], by="Archip")
colnames(data_residuals)[8] <- "Perc_cover_PAs"
data_residuals <- data_residuals[order(data_residuals$Archip, decreasing=F), ]
data_residuals$Archip

data_residuals$Mean_resid <- NA
data_residuals$Upper_resid <- NA
data_residuals$Lower_resid <- NA
data_residuals$Mean_diversity <- NA
data_residuals$Upper_diversity <- NA
data_residuals$Lower_diversity <- NA

for (i in 1:nrow(data_residuals)) {
  
  sub_data_residuals <- data_residuals[i,]
  data_residuals$Mean_resid[i] <- rowMeans(sub_data_residuals[,c(3,5,7)], na.rm = TRUE)
  data_residuals$Upper_resid[i] <- data_residuals$Mean_resid[i] + rowSds(sub_data_residuals[,c(3,5,7)], na.rm = TRUE)
  data_residuals$Lower_resid[i] <- data_residuals$Mean_resid[i] - rowSds(sub_data_residuals[,c(3,5,7)], na.rm = TRUE)
  
  data_residuals$Mean_diversity[i] <- rowMeans(sub_data_residuals[,c(2,4,6)], na.rm = TRUE)
  data_residuals$Upper_diversity[i] <- data_residuals$Mean_diversity[i] + rowSds(sub_data_residuals[,c(2,4,6)], na.rm = TRUE)
  data_residuals$Lower_diversity[i] <- data_residuals$Mean_diversity[i] - rowSds(sub_data_residuals[,c(2,4,6)], na.rm = TRUE)
  
}
rm(sub_data_residuals, i)


#RANK DES ARCHIPELS
TD_Rank <- SAR_TD_Archip[,c(1,6)]
TD_Resid <- SAR_TD_Archip[,c(1,5)]
PD_Rank <- SAR_PD_Archip[,c(1,6)]
PD_Resid <- SAR_PD_Archip[,c(1,5)]
FD_Rank <- SAR_FD_Archip[,c(1,6)]
FD_Resid <- SAR_FD_Archip[,c(1,5)]

Hotspot_Rank_Sum <- TD_Rank
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, PD_Rank, by = "Archip")
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, FD_Rank, by = "Archip")
colnames(Hotspot_Rank_Sum) <- c("Archip", "TD_Rank", "PD_Rank", "FD_Rank")
Hotspot_Rank_Sum$Tot_rank <- rowSums(Hotspot_Rank_Sum[,2:4], na.rm = TRUE)
Hotspot_Rank_Sum <- Hotspot_Rank_Sum[order(Hotspot_Rank_Sum$Tot_rank, decreasing=F), ]
Hotspot_Rank_Sum$Rank_final <- c(1:6, 8, 7, 9:19)
data_residuals <- left_join(data_residuals, Hotspot_Rank_Sum[,c(1, 6)], by="Archip")

rm(SAR_TD_Archip, Archip_PAs, SAR_PD_Archip, SAR_FD_Archip, TD_Resid, PD_Resid, FD_Resid, TD_Rank, PD_Rank, FD_Rank, Hotspot_Rank_Sum)


ggplot(data = data_residuals, aes(x = Mean_diversity, y = Mean_resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#bdbdbd") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#bdbdbd") +
  geom_errorbarh(aes(xmin = Lower_diversity, xmax = Upper_diversity), width = 0, size = 0.1, color = "#969696") +
  geom_errorbar(aes(ymin = Lower_resid, ymax = Upper_resid), width = 0, size = 0.1, color = "#969696") +
  geom_point(aes(size = Perc_cover_PAs, fill = Perc_cover_PAs), shape = 21) +
  scale_fill_distiller(palette = "Greys", direction = -1)  + xlim(0, 1) + ylim(-0.5, 0.5) +
  geom_text_repel(aes(label = c("BESSI", "ClI", "CrI", "EMI", "GGI", "IB", "Ja", "MIOI", "MB", "MGI", "NC", "PNG", "Ph", "PM", "SL", "Su", "Ta", "TNZI", "Wa")), size = 4, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
  xlab("Standardized threatened diversity") + ylab("Residual threatened diversity") + theme_niwot() #+ theme(legend.position = "none")  



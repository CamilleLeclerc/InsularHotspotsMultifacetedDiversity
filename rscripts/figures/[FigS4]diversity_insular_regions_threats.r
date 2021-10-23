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
library(viridis)
library(wesanderson)


##--------------
## LOAD DATASETS
##--------------
Info_Archip_Threat <- read.csv("outputs/Info_Archip_Threat.txt", sep="")

data <- Info_Archip_Threat[,c(1:3, 5:7)]
colnames(data) <- c("ArchipThreat", "Archip", "Threat", "TD", "PD", "FD")
data$TD <- data$TD/max(data$TD)
data$PD <- data$PD/max(data$PD)
data$FD <- data$FD/max(data$FD, na.rm = TRUE)

#RANK DES ARCHIPELS
SAR_TD_Archip <- read.csv("outputs/DAR_TD_Archip.txt", sep="")
SAR_PD_Archip <- read.csv("outputs/DAR_PD_Archip.txt", sep="")
SAR_FD_Archip <- read.csv("outputs/DAR_FD_Archip.txt", sep="")

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
rm(SAR_TD_Archip, SAR_PD_Archip, SAR_FD_Archip, TD_Rank, PD_Rank, FD_Rank)
data <- left_join(data, Hotspot_Rank_Sum[,c(1,6)], by = "Archip")
rm(Hotspot_Rank_Sum)

levels(data$Threat)
data$Threat <- factor(data$Threat, levels = c("Wildlife exploitation",
                                              "Cultivation", "Energy production & mining", "Habitat modifications", "Urbanization",
                                              "Climate change", "Geological events", "Human intrusions & disturbance", "Biological invasions", "Pollution", "Transport corridors"))




TD <- ggplot(data, aes(x = Threat, y = reorder(Archip, -Rank_final))) + geom_tile(aes(fill = TD), colour = "white") +
        scale_fill_gradientn(colours = wes_palette("Zissou1", 1000, type = "continuous")) +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank(), axis.text.y  = element_text(colour="black", size=10)) +
        theme(legend.position="none")
        
PD <- ggplot(data, aes(x = Threat, y = reorder(Archip, -Rank_final))) + geom_tile(aes(fill = PD), colour = "white") +
        scale_fill_gradientn(colours = wes_palette("Zissou1", 1000, type = "continuous")) +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank(), axis.text.y  = element_text(colour="black", size=10)) +
        theme(legend.position="none")

FD <- ggplot(data, aes(x = Threat, y = reorder(Archip, -Rank_final))) + geom_tile(aes(fill = FD), colour = "white") +
        scale_fill_gradientn(colours = wes_palette("Zissou1", 1000, type = "continuous")) +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE) +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(colour="black", size=10)) +
        theme(axis.title.y = element_blank(), axis.text.y  = element_text(colour="black", size=10)) +
        theme(legend.position="none")

ggdraw() +
  draw_plot(TD, 0, 0.66, 1, 0.33) +
  draw_plot(PD, 0, 0.33, 1, 0.33) +
  draw_plot(FD, 0, 0, 1, 0.33) +
  draw_plot_label(c("A", "B", "C"), c(0, 0, 0), c(1, 0.66, 0.33), size = 15)

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
library(scales)


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
SAR_TD_Archip <- read.csv("outputs/DAR_TD_Archip.txt", sep="") ; colnames(SAR_TD_Archip)[6] <- "Rank_TDAR"
SAR_PD_Archip <- read.csv("outputs/DAR_PD_Archip.txt", sep="") ; colnames(SAR_PD_Archip)[6] <- "Rank_PDAR"
SAR_FD_Archip <- read.csv("outputs/DAR_FD_Archip.txt", sep="") ; colnames(SAR_FD_Archip)[6] <- "Rank_FDAR"


SAR_TD_Archip$Rank_woTDAR <- as.integer(rank(-SAR_TD_Archip$TD_obs))
SAR_PD_Archip$Rank_woPDAR <- as.integer(rank(-SAR_PD_Archip$PD_obs))
SAR_FD_Archip$Rank_woFDAR <- as.integer(rank(-SAR_FD_Archip$FD_obs))

data_residuals <- SAR_TD_Archip[,c(1, 6, 7)]
data_residuals <- left_join(data_residuals, SAR_FD_Archip[,c(1, 6, 7)], by="Archip")
data_residuals <- left_join(data_residuals, SAR_PD_Archip[,c(1, 6, 7)], by="Archip")
data_residuals <- data_residuals[order(data_residuals$Archip, decreasing=F), ]
data_residuals$Archip


show_col(hue_pal()(19))

#https://www.modalisa.com/logiciel/modalisa/support/lexique/correlation-rangs-spearman/
TD_RANK <- ggplot(data = data_residuals, aes(x = Rank_TDAR, y = Rank_woTDAR)) +
            geom_abline(slope = 1, linetype = "dashed") +
            geom_smooth(method = "lm", color = "#636363") +
            geom_point(aes(fill = Archip), size = 3, shape = 21) +
            xlim(0, 20) + ylim(0, 20) +
            xlab("Rank - DAR approach") + ylab("Rank - Raw diversity data") +
            theme_niwot() +
            stat_cor(method = "spearman", label.x = 1, label.y = 18) +
            theme(legend.position = "none")
TD_RANK


PD_RANK <- ggplot(data = data_residuals, aes(x = Rank_PDAR, y = Rank_woPDAR)) +
            geom_abline(slope = 1, linetype = "dashed") +
            geom_smooth(method = "lm", color = "#636363") +
            geom_point(aes(fill = Archip), size = 3, shape = 21) +
            xlim(0, 20) + ylim(0, 20) +
            xlab("Rank - DAR approach") + ylab("Rank - Raw diversity data") +
            theme_niwot() +
            stat_cor(method = "spearman", label.x = 1, label.y = 18) +
            theme(legend.position = "none")
PD_RANK


FD_RANK <- ggplot(data = data_residuals, aes(x = Rank_FDAR, y = Rank_woFDAR)) +
            geom_abline(slope = 1, linetype = "dashed") +
            geom_smooth(method = "lm", color = "#636363") +
            geom_point(aes(fill = Archip), size = 3, shape = 21) +
            xlim(0, 20) + ylim(0, 20) +
            xlab("Rank - DAR approach") + ylab("Rank - Raw diversity data") +
            theme_niwot() +
            stat_cor(method = "spearman", label.x = 1, label.y = 18) + 
            theme(legend.position = "none")
FD_RANK


plot_grid(TD_RANK, PD_RANK, FD_RANK,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

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
SAR_TD_Archip <- read.csv("outputs/DAR_TD_Archip.txt", sep="")
SAR_PD_Archip <- read.csv("outputs/DAR_PD_Archip.txt", sep="")
SAR_FD_Archip <- read.csv("outputs/DAR_FD_Archip.txt", sep="")

data_residuals <- SAR_TD_Archip[,c(1, 5, 6)]
colnames(data_residuals) <- c("Archip", "Resid_TD", "Rank_TD")
data_residuals <- left_join(data_residuals, SAR_FD_Archip[,c(1, 5, 6)], by="Archip")
colnames(data_residuals)[4] <- "Resid_FD"
colnames(data_residuals)[5] <- "Rank_FD"
data_residuals <- left_join(data_residuals, SAR_PD_Archip[,c(1, 5, 6)], by="Archip")
colnames(data_residuals)[6] <- "Resid_PD"
colnames(data_residuals)[7] <- "Rank_PD"
data_residuals <- data_residuals[order(data_residuals$Archip, decreasing=F), ]
data_residuals$Archip


show_col(hue_pal()(19))

shapiro.test(data_residuals$Resid_TD)
shapiro.test(data_residuals$Resid_PD)
shapiro.test(data_residuals$Resid_FD)

TD_PD <- ggplot(data = data_residuals, aes(x = Resid_TD, y = Resid_PD)) +
          #geom_abline(slope = 1, linetype = "dashed") +
          geom_vline(xintercept = 0, linetype = "dashed") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_smooth(method = "lm", color = "#636363") +
          geom_point(aes(fill = Archip), size = 3, shape = 21) +
          xlim(-0.55, 0.55) + ylim(-0.55, 0.75) +
          xlab("Residual taxonomic diversity") + ylab("Residual phylogenetic diversity") +
          theme_niwot() +
          stat_cor(method = "spearman", label.x = -0.55, label.y = 0.55) +
          theme(legend.position = "none")
TD_PD


TD_FD <- ggplot(data = data_residuals, aes(x = Resid_TD, y = Resid_FD)) +
          #geom_abline(slope = 1, linetype = "dashed") +
          geom_vline(xintercept = 0, linetype = "dashed") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_smooth(method = "lm", color = "#636363") +
          geom_point(aes(fill = Archip), size = 3, shape = 21) +
          xlim(-0.55, 0.55) + ylim(-0.55, 0.75) +
          xlab("Residual taxonomic diversity") + ylab("Residual functional diversity") +
          theme_niwot() +
          stat_cor(method = "spearman", label.x = -0.55, label.y = 0.55) +
          theme(legend.position = "none")
TD_FD


PD_FD <- ggplot(data = data_residuals, aes(x = Resid_PD, y = Resid_FD)) +
          #geom_abline(slope = 1, linetype = "dashed") +
          geom_vline(xintercept = 0, linetype = "dashed") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_smooth(method = "lm", color = "#636363") +
          geom_point(aes(fill = Archip), size = 3, shape = 21) +
          xlim(-0.55, 0.55) + ylim(-0.55, 0.75) +
          xlab("Residual phylogenetic diversity") + ylab("Residual functional diversity") +
          theme_niwot() +
          stat_cor(method = "spearman", label.x = -0.55, label.y = 0.55) +
          theme(legend.position = "none")
PD_FD


plot_grid(TD_PD, TD_FD, PD_FD, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

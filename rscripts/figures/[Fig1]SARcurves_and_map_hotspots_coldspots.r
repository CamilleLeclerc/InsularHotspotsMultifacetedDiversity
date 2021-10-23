##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(cowplot)
library(dplyr)
library(ggalt)
library(ggmap)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(maptools)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sf)
library(shapefiles)
library(viridis)


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

threats.of.endemic.mammals <- read.csv("data/threats_of_endemic_mammals.txt", sep="")
head(threats.of.endemic.mammals)
threats.of.endemic.mammals <- unique(threats.of.endemic.mammals %>% select(binomial, threat))


data.endemic.mammals <- left_join(iucn.mammals.endemic.trait, iucn.mammals.insular, by = "binomial")
data.endemic.mammals <- left_join(data.endemic.mammals, islands.of.endemic.mammals, by = "ULM_ID")
data.endemic.mammals <- left_join(data.endemic.mammals, threats.of.endemic.mammals, by = "binomial")
head(data.endemic.mammals)
unique(data.endemic.mammals$statut)
unique(data.endemic.mammals$category)
data.endemic.mammals <- data.endemic.mammals %>% filter(category != "EX")
data.endemic.mammals <- data.endemic.mammals[!is.na(data.endemic.mammals$REGION),]
length(unique(data.endemic.mammals$binomial))
length(unique(data.endemic.mammals$REGION))
length(unique(data.endemic.mammals$ULM_ID))




SAR_TD_Archip <- read.csv("outputs/DAR_TD_Archip.txt", sep="")
TD_Rank <- SAR_TD_Archip[,c(1,6)]
TD_Resid <- SAR_TD_Archip[,c(1,5)]
SAR_PD_Archip <- read.csv("outputs/DAR_PD_Archip.txt", sep="")
PD_Rank <- SAR_PD_Archip[,c(1,6)]
PD_Resid <- SAR_PD_Archip[,c(1,5)]
SAR_FD_Archip <- read.csv("outputs/DAR_FD_Archip.txt", sep="")
FD_Rank <- SAR_FD_Archip[,c(1,6)]
FD_Resid <- SAR_FD_Archip[,c(1,5)]

Hotspot_Rank_Sum <- TD_Rank
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, PD_Rank, by = "Archip")
Hotspot_Rank_Sum <- left_join(Hotspot_Rank_Sum, FD_Rank, by = "Archip")
colnames(Hotspot_Rank_Sum) <- c("Archip", "TD_Rank", "PD_Rank", "FD_Rank")
Hotspot_Rank_Sum$Tot_rank <- rowSums(Hotspot_Rank_Sum[,2:4], na.rm = TRUE)
Hotspot_Rank_Sum <- Hotspot_Rank_Sum[order(Hotspot_Rank_Sum$Tot_rank, decreasing=F), ]
Hotspot_Rank_Sum$Rank_final <- c(1:6, 8, 7, 9:19)


shp_islands <- readShapeSpatial("data/GADM_islands_17883data.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shp_islands <- shp_islands[shp_islands@data$ULM_ID %in% data.endemic.mammals$ULM_ID,]
shp_islands_data <- as.data.frame(cbind(shp_islands@data$ULM_ID, as.character(shp_islands@data$ARCHIP)))
colnames(shp_islands_data) <- c("ULM_ID", "ARCHIP")
class(shp_islands_data)
class(shp_islands_data$ULM_ID)
shp_islands_data$ULM_ID <- as.numeric(as.character(shp_islands_data$ULM_ID))
class(shp_islands_data$ULM_ID)
shp_islands_data <- unique(left_join(shp_islands_data, data.endemic.mammals %>% select(ULM_ID,REGION), by = "ULM_ID"))
colnames(shp_islands_data) <- c("ULM_ID", "Old_archip", "Archip")
shp_islands_data <- left_join(shp_islands_data, Hotspot_Rank_Sum[,c(1,6)], by = "Archip")
shp_islands@data$ARCHIP
shp_islands@data$ARCHIP <- shp_islands_data$Archip
shp_islands@data$ARCHIP
shp_islands@data$RANK <- shp_islands_data$Rank_final
shp_islands@data$id = rownames(shp_islands@data)
shp_islands.points = fortify(shp_islands, region="id")
shp_islands.df = join(shp_islands.points, shp_islands@data, by="id")


shp_regions <- readShapeSpatial("data/Regions.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shp_regions_data <- as.data.frame(shp_regions@data$NAME)
colnames(shp_regions_data) <- c("Archip")
class(shp_regions_data)
class(shp_regions_data$Archip)
shp_regions_data <- left_join(shp_regions_data, Hotspot_Rank_Sum[,c(1,6)], by = "Archip")
shp_regions@data$RANK <- shp_regions_data$Rank_final
shp_regions@data$id = rownames(shp_regions@data)
shp_regions.points = fortify(shp_regions, region="id")
shp_regions.df = join(shp_regions.points, shp_regions@data, by="id")


wmap <- readOGR(dsn = path.expand("data"), layer = "ne_10m_land")
grat <- readOGR(dsn = path.expand("data"), layer = "ne_10m_graticules_30")
bord <- readOGR(dsn = path.expand("data"), layer = "ne_10m_wgs84_bounding_box")
wmapF <- fortify(wmap)
gratF <- fortify(grat)
bordF <- fortify(bord)



MAP <- ggplot() + 
        geom_polygon(data = wmapF, aes(x = long, y = lat, group = group), color = "grey20") + 
        geom_path(data = gratF, aes(x = long, y = lat, group = group), color = "grey50", linetype = "dashed", size = .1) +
        geom_polygon(data = shp_regions.df, aes(long, lat, group = group, fill = RANK, color = RANK), size = 0.05, alpha = 0.5) +
        geom_polygon(data = shp_islands.df, aes(long, lat, group = group, fill = RANK, color = RANK), size = 0.05, alpha = 0.7) +
        geom_polygon(data = bordF, aes(x = long, y = lat, group = group), alpha = 0, color = "grey20") +
        scale_fill_viridis() +
        scale_color_viridis() +
        coord_map("mollweide", xlim=c(-180,180)) +
        theme_void() +
        theme(legend.position="none") +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank())
MAP
          
          
SAR_TD_Archip_graph <- read.csv("outputs/DAR_TD_Archip_graph.txt", sep="")
SAR_PD_Archip_graph <- read.csv("outputs/DAR_PD_Archip_graph.txt", sep="")
SAR_FD_Archip_graph <- read.csv("outputs/DAR_FD_Archip_graph.txt", sep="")          

SAR_TD_Archip <- ggplot(SAR_TD_Archip_graph, aes(x = Area, y = Diversity)) + geom_line(aes(x = Area, y = Curve), stat = "identity") + 
                  geom_point(aes(fill = Rank), size = 4, shape = 21) +
                  scale_fill_viridis() +
                  geom_text_repel(aes(label = c("ClI", "GGI", "MGI", "BESSI", "NC", "PM", "Ta", "IB", "SL", "MB", "EMI", "CrI", "Ph", "Wa", "TNZI", "Ja", "MIOI", "PNG", "Su")), size = 4, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                  xlab("Area (km²)") + ylab("Standardized Taxonomic Diversity") + theme_niwot() + theme(legend.position = "none")
SAR_TD_Archip 

SAR_PD_Archip <- ggplot(SAR_PD_Archip_graph, aes(x = Area, y = Diversity)) + geom_line(aes(x = Area, y = Curve), stat = "identity") +
                  geom_point(aes(fill = Rank), size = 4, shape = 21) +
                  scale_fill_viridis() +
                  geom_text_repel(aes(label = c("ClI", "GGI", "MGI", "BESSI", "NC", "PM", "Ta", "IB", "SL", "MB", "EMI", "CrI", "Ph", "Wa", "TNZI", "Ja", "MIOI", "PNG", "Su")), size = 3.5, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                  xlab("Area (km²)")+ylab("Standardized Phylogenetic Diversity") + theme_niwot() + theme(legend.position = "none")
SAR_PD_Archip

SAR_FD_Archip <- ggplot(SAR_FD_Archip_graph, aes(x = Area, y = Diversity)) + geom_line(aes(x = Area, y = Curve), stat = "identity") +
                  geom_point(aes(fill = Rank), size = 4, shape = 21) +
                  scale_fill_viridis() +
                  geom_text_repel(aes(label = c("ClI", "GGI", "MGI", "PM", "IB", "SL", "MB", "EMI", "CrI", "Ph", "Wa", "TNZI", "Ja", "MIOI", "PNG", "Su")), size = 3.5, box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines")) +
                  xlab("Area (km²)")+ylab("Standardized Functional Diversity") + theme_niwot() + theme(legend.position = "none")
SAR_FD_Archip


ggarrange(ggarrange(SAR_TD_Archip, SAR_PD_Archip, SAR_FD_Archip, ncol = 3, labels = c("A", "B", "C")), MAP,
          nrow = 2, labels = "D") 




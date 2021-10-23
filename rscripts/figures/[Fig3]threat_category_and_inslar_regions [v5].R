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




tric_threat <- Tricolore(Diversity, p1 = 'relat.div.direct', p2 = 'relat.div.habitat', p3 = 'relat.div.mixed', chroma = 1)
Diversity$tric_threat <- tric_threat$rgb
tric_threat$key


library(ggtern) 
ggtern(data=Diversity,aes(x=relat.div.direct,y=relat.div.habitat,z=relat.div.mixed,color=Archip)) +
  geom_point() +
  labs(x="direct",y="habitat",z="mixed") +
  scale_T_continuous(breaks=unique(Diversity$x))+ 
  scale_L_continuous(breaks=unique(Diversity$y))+ 
  scale_R_continuous(breaks=unique(Diversity$z))


##--------------
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
shp_islands_data <- left_join(shp_islands_data, Diversity[,c(1, 11, 12, 13, 14)], by = "Archip")
shp_islands@data$ARCHIP <- shp_islands_data$Archip
shp_islands@data$mixed <- shp_islands_data$relat.div.mixed
shp_islands@data$direct <- shp_islands_data$relat.div.direct
shp_islands@data$habitat <- shp_islands_data$relat.div.habitat
shp_islands@data$tric_threat <- shp_islands_data$tric_threat
shp_islands@data$id = rownames(shp_islands@data)
shp_islands.points = fortify(shp_islands, region="id")
shp_islands.df = join(shp_islands.points, shp_islands@data, by="id")


shp_regions <- readShapeSpatial("data/Regions.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shp_regions_data <- as.data.frame(shp_regions@data$NAME)
colnames(shp_regions_data) <- c("Archip")
class(shp_regions_data)
class(shp_regions_data$Archip)
shp_regions_data <- left_join(shp_islands_data, Diversity[,c(1, 11, 12, 13, 14)], by = "Archip")
shp_regions@data$mixed <- shp_regions_data$relat.div.mixed
shp_regions@data$direct <- shp_regions_data$relat.div.direct
shp_regions@data$habitat <- shp_regions_data$relat.div.habitat
shp_regions@data$tric_threat <- shp_regions_data$tric_threat
shp_regions@data$id = rownames(shp_regions@data)
shp_regions.points = fortify(shp_regions, region="id")
shp_regions.df = join(shp_regions.points, shp_regions@data, by="id")


wmap <- readOGR(dsn = path.expand("Natural_earth"), layer = "ne_10m_land")
grat <- readOGR(dsn = path.expand("Natural_earth"), layer = "ne_10m_graticules_30")
bord <- readOGR(dsn = path.expand("Natural_earth"), layer = "ne_10m_wgs84_bounding_box")
wmapF <- fortify(wmap)
gratF <- fortify(grat)
bordF <- fortify(bord)


#levels(unique(shp_regions.df$NAME))
levels(unique(shp_islands.df$ARCHIP))
Diversity <- Diversity[order(Diversity$Archip),]
unique(Diversity[,c(1, 14)])


MAP <- ggplot() + 
          geom_polygon(data = wmapF, aes(x = long, y = lat, group = group), color = "grey20") + 
          geom_path(data = gratF, aes(x = long, y = lat, group = group), color = "grey50", linetype = "dashed", size = .1) +
          #geom_polygon(data = shp_regions.df, aes(long, lat, group = group, fill = NAME, color = NAME), size = 0.2, alpha = 0.5) +
          geom_polygon(data = shp_islands.df, aes(long, lat, group = group, fill = ARCHIP, color = ARCHIP), size = 0.05, alpha = 0.7) +
          geom_polygon(data = bordF, aes(x = long, y = lat, group = group), alpha = 0, color = "grey20") +
          scale_colour_manual(values = c("#7486AB", "#DF73D6", "#AB84DA", "#629168", "#629168", "#629168", "#727272", "#629168", "#DF73D6", "#DF73D6", "#727272", "#629168", "#629168", "#DF73D6", "#629168", "#629168", "#727272", "#DF73D6", "#629168")) +
          scale_fill_manual(values = c("#7486AB", "#DF73D6", "#AB84DA", "#629168", "#629168", "#629168", "#727272", "#629168", "#DF73D6", "#DF73D6", "#727272", "#629168", "#629168", "#DF73D6", "#629168", "#629168", "#727272", "#DF73D6", "#629168")) +
          coord_map("mollweide", xlim=c(-180,180)) +
          theme_void() +
          theme(legend.position="none") +
          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank(),
                axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank())
MAP

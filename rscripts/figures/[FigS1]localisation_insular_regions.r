##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(dplyr)
library(ggplot2)
library(lwgeom)
library(maptools)
library(sf)
library(shapefiles)
library(rgdal)
library(rgeos)


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


data.islands <- unique(data.endemic.mammals[,c(9:10)])
length(unique(data.islands$ULM_ID))
length(unique(data.islands$REGION))


##--------------
## ISLANDS PLOT
##--------------
islands <- st_read("data/GADM_islands_17883data.shp")
str(islands)
islands.focus <- islands[islands$ULM_ID %in% data.islands[, "ULM_ID"],]

class(islands.focus)
islands.focus.centroid <- islands.focus
st_geometry(islands.focus.centroid) <- NULL
length(unique(islands.focus.centroid$ULM_ID)) #1796 islands, c'est 13 de moins 
islands.focus.centroid$geometry <- NULL

data.islands <- left_join(data.islands, islands.focus.centroid %>% select(ULM_ID, LONG, LAT), by = "ULM_ID")


wmap <- readOGR(dsn = path.expand("data"), layer = "ne_10m_land")
grat <- readOGR(dsn = path.expand("data"), layer = "ne_10m_graticules_30")
bord <- readOGR(dsn = path.expand("data"), layer = "ne_10m_wgs84_bounding_box")

wmapF <- fortify(wmap)
gratF <- fortify(grat)
bordF <- fortify(bord)


col_width = function(txt, key.width, key.spacing, ps) {
  txt.width = strwidth(txt, units="inches", ps = par(ps = ps))
  max(txt.width + key.width + 2*key.spacing)
}

opt.cols = function(txt, title, key.width, key.spacing, plot.width, ps) {
  title.width = strwidth(expression(bold(title)),  units="inches", ps = par(ps = ps))
  (plot.width - title.width) %/% col_width(txt, key.width, key.spacing, ps)
}

width_scale = 12
key.width = width_scale/50
key.spacing = width_scale/50
ps = 12
num_cols = opt.cols(data.islands$REGION, 'var', key.width, key.spacing, width_scale, ps)

ggplot(wmapF, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "grey20") + 
  geom_path(data = gratF, color = "grey50", linetype = "dashed", size = .1) +
  geom_point(data = data.islands, aes(LONG, LAT, group = REGION, fill = REGION, stroke = 0.5), shape = 21, colour = "black", size = 3) +
  geom_polygon(data = bordF, alpha = 0, color = "grey20") +
  coord_map("mollweide", xlim=c(-180,180)) +
  theme_void() +
  theme(legend.key = element_blank(), legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size = 12),
        legend.spacing.x = grid::unit(key.spacing, "inch"),
        legend.key.size = grid::unit(key.width, "inch"),
        legend.key.width = grid::unit(key.width, "inch")) + guides(fill = guide_legend(ncol = num_cols)) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank())

##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(berryFunctions)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(sars)
library(stringr)


source("rfunctions/species_to_FE.R")
source("rfunctions/FE_metrics.R")
source("rfunctions/quality_funct_space.R")
source("rfunctions/plot_funct_space.R")
source("rfunctions/multidimFD.R")


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

data.endemic.mammals <- left_join(iucn.mammals.endemic.trait, iucn.mammals.insular, by = "binomial")
data.endemic.mammals <- left_join(data.endemic.mammals, islands.of.endemic.mammals, by = "ULM_ID")
head(data.endemic.mammals)
unique(data.endemic.mammals$statut)
unique(data.endemic.mammals$category)
data.endemic.mammals <- data.endemic.mammals %>% filter(category != "EX")
data.endemic.mammals <- data.endemic.mammals[!is.na(data.endemic.mammals$REGION),]
length(unique(data.endemic.mammals$binomial))
length(unique(data.endemic.mammals$REGION))
length(unique(data.endemic.mammals$ULM_ID))


##---------------------
## FUNCTIONAL DIVERSITY
##---------------------
Mammals.IUCN <- unique(data.endemic.mammals %>% select(binomial, category))
Mammals.Archip <- unique(data.endemic.mammals %>% select(binomial, REGION))
Mammals.traits <- unique(data.endemic.mammals %>% select(binomial, body.mass, main.diet, foraging.period, foraging.niche, habitat.breadth))
colnames(Mammals.traits) <- c("binomial", "BM", "DomDiet", "Activity", "DomFN", "HB")


#Importing presence of species in assemblages [global pool / assemblages by archipelago]
Archip.IUCN <- merge(Mammals.Archip, Mammals.IUCN, by="binomial", all=TRUE)
Archip.IUCN <- unique(Archip.IUCN)
Archip.IUCN <- na.omit(Archip.IUCN)
summary(as.factor(Archip.IUCN$category))
Archip.IUCN.thr <- Archip.IUCN[Archip.IUCN$category != "DD" & Archip.IUCN$category != "LC" & Archip.IUCN$category != "NT",]
Archip.IUCN.nothr <- Archip.IUCN[Archip.IUCN$category != "VU" & Archip.IUCN$category != "EN" & Archip.IUCN$category != "CR",]

Mammals.thr.Archip <- Archip.IUCN.thr[,c(1:2)]
Mammals.thr.Archip <- unique(Mammals.thr.Archip)
presence.thr <- as.matrix(table(Mammals.thr.Archip$REGION, Mammals.thr.Archip$binomial))
class(presence.thr) #Table
presence.thr <- unclass(presence.thr)
class(presence.thr) #Matrix

Mammals.nothr.Archip <- Archip.IUCN.nothr[,c(1:2)]
Mammals.nothr.Archip <- unique(Mammals.nothr.Archip)
presence.nothr <- as.matrix(table(Mammals.nothr.Archip$REGION, Mammals.nothr.Archip$binomial))
class(presence.nothr) #Table
presence.nothr <- unclass(presence.nothr)
class(presence.nothr) #Matrix
presence.nothr[presence.nothr == 1] <- 0

presence <- cbind(presence.thr, presence.nothr)
ncol(presence)


#Importing species raw trait values
traits <- Mammals.traits
rownames(traits) <- traits[,1]
traits[,1] <- NULL

#Checking that species names are the same in the two matrices
sum(row.names(traits) %in% colnames(presence)) == ncol(presence)

#Looking at trait coding after importing data
summary(traits) # all traits are considered as categorical while some should be coded as category or ordinal
# => need to set correct type of variables

quantile(as.numeric(as.character(traits$BM)), probs = seq(0, 1, 0.20))
traits$BM_cat <-as.numeric(as.character(traits$BM))
traits$BM_cat <- replace(traits$BM_cat, traits$BM_cat > 0 & traits$BM_cat <= 15.440, 1)
traits$BM_cat <- replace(traits$BM_cat, traits$BM_cat > 15.440 & traits$BM_cat <= 69.480, 2)
traits$BM_cat <- replace(traits$BM_cat, traits$BM_cat > 69.480 & traits$BM_cat <= 177.560, 3)
traits$BM_cat <- replace(traits$BM_cat, traits$BM_cat > 177.560 & traits$BM_cat <= 817.504, 4)
traits$BM_cat <- replace(traits$BM_cat, traits$BM_cat > 817.504 & traits$BM_cat <= 1750000.000, 5)

summary(traits)
traits <- traits[,-1]
traits$BM_cat <-as.factor(traits$BM_cat)

#empty dataframe to store trait values
traits_f <- as.data.frame( matrix(NA, nrow(traits), ncol(traits), dimnames=list(row.names(traits), names(traits))))

#ordinal traits converted to "ordered" mode
traits_f[,"BM_cat"] <- factor(traits[,"BM_cat"], levels = c("1", "2", "3", "4", "5"), labels = c("vsmall", "small", "medium", "large", "vlarge" ), ordered = TRUE )

traits_f[,"DomDiet"] <- factor(traits[,"DomDiet"])
colnames(traits_f)[1] <-"DietDom"

traits$Activity <- replace(as.character(traits$Activity), traits$Activity == "CDN", "NDC")
traits_f[,"Activity"] <- factor(traits[,"Activity"])

traits_f[,"DomFN"] <- as.factor(traits[,"DomFN"])
colnames(traits_f)[3] <- "FNDom"

traits_f[,"HB"] <- factor(traits[,"HB"], levels = c("1", "2", "3", "4", "5", "6"), ordered = TRUE )

#Categoric ordinal vs nominal
is.ordered(traits_f[,"BM_cat"])
is.ordered(traits_f[,"DietDom"])
is.ordered(traits_f[,"Activity"])
is.ordered(traits_f[,"FNDom"])
is.ordered(traits_f[,"HB"])

#checking the new trait database
summary(traits_f)

rm(Mammals.IUCN, Mammals.Archip, Mammals.traits, traits, Mammals.thr.Archip, Mammals.nothr.Archip, presence.nothr, presence.thr)


#Computing FUNCTIONAL ENTITIES = unique combinations of trait values##
species_to_FE_mammals <- species_to_FE(traits_f)  # Grouping species into FE
species_to_FE_mammals$FE_codes  # Codes of FE
                                # the 1000 species were grouped into 270 Funct entities
apply(species_to_FE_mammals$FE_sp_01,1,sum) # Number of species per FE
summary(apply(species_to_FE_mammals$FE_sp_01,1,sum))
FE <- species_to_FE_mammals$FE  # FE to which each species belongs
FE_mammals_01 <- species_to_FE_mammals$FE_sp_01
FE_traits <- species_to_FE_mammals$FE_traits  # Trait values of the FE


#Matrix of FE biomass in assemblages
assemblage_FE_weight <- matrix(0, nrow(presence), nrow(FE_mammals_01), dimnames=list(row.names(presence), row.names(FE_mammals_01))) #Empty matrix

for (k in row.names(FE_mammals_01)) #loop on FE
  {
     sp_k <- names(which(FE_mammals_01[k,]==1))
     if(length(sp_k) == 1) {assemblage_FE_weight[,k] <- presence[,sp_k]} # if only one species in FE k
     if(length(sp_k) > 1) {assemblage_FE_weight[,k] <- apply(presence[,sp_k],1,sum)} # if more than 1 species in FE k
}#End of k

sum(presence) == sum(assemblage_FE_weight) #Check total biomass kept constant


#Matrix of FE occurence (0/1) in assemblages
assemblage_FE_occ <- assemblage_FE_weight
assemblage_FE_occ[which(assemblage_FE_occ > 0)] <- 1


#Computing diversity metrics based on Funct ent for the set of fruits baskets studied
assemblages_FE_metrics <- FE_metrics(species_to_FE_mammals, presence, check_species_pool = FALSE, folder_plot = "outputs/Results_FE", nm_asb_plot = row.names(presence))
round(assemblages_FE_metrics,3)
# plots illustrating distribution of species into Funct entities are in the subfolder in your working directory.


#Computing MULTIDIMENSIONAL FUNCTIONAL DIVERISTY INDICES based on FE position in a functional space to assess##
#How weight is distributed in the functional space independently from packing of species into FE (which is assessed by metrics presented above)
#Computing Functional space based on trait value of Functional entities (not based on species trait values
#because we want to represent distance between combinations of trait values independently from their frequency among species, i.e. to give same weight to each FE whatever its number of species)
qual_funct_space_FE <- quality_funct_space(FE_traits, traits_weights = NULL, nbdim = 10, metric = "Gower", dendro = FALSE, plot = "quality_funct_space_FE")
qual_funct_space_FE$meanSD

#FE coordinates in the best space
coord_FE_4D <- qual_funct_space_FE$details_funct_space$mat_coord[,1:4]

#Species coordinates in the space according to those of FE
coord_sp_4D <- coord_FE_4D[FE,]
row.names(coord_sp_4D) <- names(FE)



#FD indices
#Computing FD indices according to species weights
presence <- presence[ , order(colnames(presence))]
coord_sp_4D <- coord_sp_4D[order(rownames(coord_sp_4D)) , ]

FD_assemblage_sp <- multidimFD(coord_sp_4D, presence[!rownames(presence) %in% "New Caledonia", ], check_species_pool = FALSE, verb = TRUE)
FD_assemblage_sp[,"FRic"]

#Computing FD indices according to FE weights
FD_assemblage_FE <- multidimFD(coord_FE_4D, assemblage_FE_weight, check_species_pool = FALSE, verb = TRUE  )
FD_assemblage_FE[,"FRic"]

rm(Archip.IUCN, Archip.IUCN.nothr, Archip.IUCN.thr, assemblage_FE_occ, assemblage_FE_weight, assemblages_FE_metrics, coord_FE_4D, coord_sp_4D, FD_assemblage_sp, FE_mammals_01, FE_traits, presence, qual_funct_space_FE, species_to_FE_mammals, traits_f, FE, k, sp_k, FE_metrics, GFD_matcomm, multidimFD, plot_funct_space, quality_funct_space, species_to_FE)

class(FD_assemblage_FE)
database_mammals_archip <- as.data.frame(FD_assemblage_FE)
database_mammals_archip$Archip <- rownames(database_mammals_archip)
rownames(database_mammals_archip) <- NULL
database_mammals_archip$Area <- NA
database_mammals_archip <- database_mammals_archip[,c(25, 26, 19)]


for (i in 1:nrow(database_mammals_archip)){

  sub_archip <- data.endemic.mammals[data.endemic.mammals$REGION == database_mammals_archip[i,1],]
  sub_archip_area <- unique(sub_archip %>% select(ULM_ID, Area))
  database_mammals_archip[i,2] <- sum(sub_archip_area$Area)

  }
  
rm(sub_archip, sub_archip_area, i)


database_mammals_thr_archip <- as.data.frame(cbind(database_mammals_archip$Area, database_mammals_archip$FRic))
colnames(database_mammals_thr_archip) <- c("Area","FD_obs")
rownames(database_mammals_thr_archip) <- database_mammals_archip$Archip


##----------------------------
## DIVERSITY AREA RELATIONSHIP
##----------------------------
sars_models() #list the 20 functions used
display_sars_models() #information list about the 20 functions

####Standardized Average Model####
## Averaged Model FD ##
FD_averaged <- database_mammals_thr_archip
FD_averaged <- FD_averaged[complete.cases(FD_averaged), ]
FD_averaged$FD_obs <- FD_averaged$FD_obs/max(FD_averaged$FD_obs)


fit_FD <- sar_average(data = FD_averaged)
#20 remaining models used to construct the multi  FAR:
#Power, PowerR, Extended Power model 1, Extended Power model 2, Persistence function 1, Persistence function 2, Logarithmic, Kobayashi, MMF, Monod, Negative exponential, Chapman Richards, Cumulative Weibull 3 par., Asymptotic regression, Rational function, Gompertz, Cumulative Weibull 4 par., Beta-P cumulative, Heleg(Logistic), Linear model

plot(fit_FD)
summary(fit_FD)
plot(fit_FD, type = 'multi',allCurves = FALSE)
plot(fit_FD, type='bar')
fit_FD$details
str(fit_FD)
fitted_FD_values <- fit_FD$mmi

####Residuals Computation####
database_mammals_thr_archip <- database_mammals_thr_archip[complete.cases(database_mammals_thr_archip), ]
database_mammals_thr_archip <- database_mammals_thr_archip[order(database_mammals_thr_archip$Area, decreasing=F), ]
database_mammals_thr_archip$FD_obs <- database_mammals_thr_archip$FD_obs/max(database_mammals_thr_archip$FD_obs)
database_mammals_thr_archip$Fitted_values <- fitted_FD_values
database_mammals_thr_archip$Residuals <- database_mammals_thr_archip$FD_obs - database_mammals_thr_archip$Fitted_values
database_mammals_thr_archip$Archip <- rownames(database_mammals_thr_archip)
colnames(database_mammals_thr_archip)
database_mammals_thr_archip <- database_mammals_thr_archip[,c(5, 1:4)]
rownames(database_mammals_thr_archip) <- NULL
database_mammals_thr_archip <- database_mammals_thr_archip[order(database_mammals_thr_archip$Residuals, decreasing=T), ]
database_mammals_thr_archip$Rank <- 1:nrow(database_mammals_thr_archip)
write.table(database_mammals_thr_archip, "outputs/DAR_FD_Archip.txt", row.names = FALSE)

####DATA FOR GRAPH####
class(fit_FD)
str(fit_FD)

data_graph_FD <- data.frame(matrix(nrow=nrow(database_mammals_thr_archip), ncol=3))
colnames(data_graph_FD) <- c("Area", "Diversity", "Curve")
data_graph_FD$Area <- database_mammals_thr_archip$Area
data_graph_FD$Diversity <- database_mammals_thr_archip$FD_obs
data_graph_FD <- data_graph_FD[order(data_graph_FD$Area, decreasing=F), ]
data_graph_FD$Curve <- fit_FD$mmi
data_graph_FD

class(data_graph_FD)
class(data_graph_FD$Area)
class(data_graph_FD$Diversity)
class(data_graph_FD$Curve)

data_graph_FD <- left_join(data_graph_FD, database_mammals_thr_archip, by="Area")
data_graph_FD <- data_graph_FD[,c(4, 8, 1:3)]
write.table(data_graph_FD, "outputs/DAR_FD_Archip_graph.txt", row.names = FALSE)

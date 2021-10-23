##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(ape)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(picante)
library(RColorBrewer)
library(sars)


##--------------
## LOAD DATASETS
##--------------
islands.of.endemic.mammals <- read.csv("data/islands_of_endemic_mammals.txt", sep="")
head(islands.of.endemic.mammals)
islands.of.endemic.mammals <- unique(islands.of.endemic.mammals %>% select(ULM_ID, REGION, Area))

iucn.mammals.endemic.trait <- read.csv("data/iucn_mammals_endemic_trait.txt", sep="")
head(iucn.mammals.endemic.trait)
iucn.mammals.endemic.trait <- unique(iucn.mammals.endemic.trait %>% select(binomial, statut, category))

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

mammals_tree <- read.nexus("data/mammals_phylogeny.nex")


##-----------------------
## PHYLOGENETIC DIVERSITY
##-----------------------
splist <- as.data.frame(unique(data.endemic.mammals$binomial))
colnames(splist) <- "Species"
splist$Species <- gsub(' ', '_', splist$Species)

splist_phylo <- as.data.frame(mammals_tree[[1]]$tip.label)
colnames(splist_phylo) <- "Species"

diff_splist <- anti_join(splist_phylo, splist, by = "Species")

sp_abs <- anti_join(splist, splist_phylo, by = "Species")
splist$Species <- gsub('Arctonyx_hoevenii', 'Arctonyx_collaris', splist$Species)
splist$Species <- gsub('Atopogale_cubana', 'Solenodon_cubanus', splist$Species)
splist$Species <- gsub('Austronomus_kuboriensis', 'Tadarida_kuboriensis', splist$Species)
splist$Species <- gsub('Boneia_bidens', 'Rousettus_bidens', splist$Species)
splist$Species <- gsub('Brassomys_albidens', 'Coccymys_albidens', splist$Species)
splist$Species <- gsub('Carlito_syrichta', 'Tarsius_syrichta', splist$Species)
splist$Species <- gsub('Cephalopachus_bancanus', 'Tarsius_bancanus', splist$Species)
splist$Species <- gsub('Funambulus_obscurus', 'Funambulus_sublineatus', splist$Species)
splist$Species <- gsub('Hypsugo_kitcheneri', 'Pipistrellus_kitcheneri', splist$Species)
splist$Species <- gsub('Hypsugo_vordermanni', 'Pipistrellus_vordermanni', splist$Species)
splist$Species <- gsub('Macronycteris_commersoni', 'Hipposideros_commersoni', splist$Species)
splist$Species <- gsub('Macronycteris_thomensis', 'Hipposideros_thomensis', splist$Species)
splist$Species <- gsub('Mysateles_melanurus', 'Mesocapromys_melanurus', splist$Species)
splist$Species <- gsub('Ozimops_loriae', 'Mormopterus_loriae', splist$Species)
splist$Species <- gsub('Paremballonura_atrata', 'Emballonura_atrata', splist$Species)
splist$Species <- gsub('Semnopithecus_vetulus', 'Trachypithecus_vetulus', splist$Species)
splist$Species <- gsub('Trachypithecus_mauritius', 'Trachypithecus_auratus', splist$Species)
splist$Species <- gsub('Tupaia_everetti', 'Urogale_everetti', splist$Species)

diff_splist <- anti_join(splist_phylo, splist, by = "Species")

splist <- as.character(splist$Species)
class(splist)
mammals_tree <- .uncompressTipLabel(mammals_tree)
for (i in 1:length(mammals_tree)){
  mammals_tree[[i]] <- drop.tip(mammals_tree[[i]], which(!mammals_tree[[i]]$tip.label %in% splist))
}


rm(splist, splist_phylo, diff_splist, sp_abs, i)


database_mammals_archip <- unique(data.endemic.mammals %>% select(REGION, binomial, category))
database_mammals_archip$binomial <- gsub(' ', '_', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Arctonyx_hoevenii', 'Arctonyx_collaris', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Atopogale_cubana', 'Solenodon_cubanus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Austronomus_kuboriensis', 'Tadarida_kuboriensis', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Boneia_bidens', 'Rousettus_bidens', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Brassomys_albidens', 'Coccymys_albidens', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Carlito_syrichta', 'Tarsius_syrichta', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Cephalopachus_bancanus', 'Tarsius_bancanus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Funambulus_obscurus', 'Funambulus_sublineatus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Hypsugo_kitcheneri', 'Pipistrellus_kitcheneri', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Hypsugo_vordermanni', 'Pipistrellus_vordermanni', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Macronycteris_commersoni', 'Hipposideros_commersoni', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Macronycteris_thomensis', 'Hipposideros_thomensis', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Mysateles_melanurus', 'Mesocapromys_melanurus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Ozimops_loriae', 'Mormopterus_loriae', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Paremballonura_atrata', 'Emballonura_atrata', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Semnopithecus_vetulus', 'Trachypithecus_vetulus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Trachypithecus_mauritius', 'Trachypithecus_auratus', database_mammals_archip$binomial)
database_mammals_archip$binomial <- gsub('Tupaia_everetti', 'Urogale_everetti', database_mammals_archip$binomial)


database_thr_mammals_archip <- database_mammals_archip[which(database_mammals_archip$category=="VU" | database_mammals_archip$category=="EN" | database_mammals_archip$category=="CR"),]
database_thr_mammals_archip <- unique(database_thr_mammals_archip[,1:2])
thr_comm_matrix <- as.matrix(table(database_thr_mammals_archip$REGION, database_thr_mammals_archip$binomial))
thr_comm_matrix <- unclass(thr_comm_matrix)
ncol(thr_comm_matrix)
nrow(thr_comm_matrix)

database_nothr_mammals_archip <- database_mammals_archip[which(database_mammals_archip$category=="DD" | database_mammals_archip$category=="LC" | database_mammals_archip$category=="NT"),]
database_nothr_mammals_archip <- unique(database_nothr_mammals_archip[,1:2])
nothr_comm_matrix <- as.matrix(table(database_nothr_mammals_archip$REGION, database_nothr_mammals_archip$binomial))
nothr_comm_matrix <- unclass(nothr_comm_matrix)
nothr_comm_matrix[,] <- 0
ncol(nothr_comm_matrix)
nrow(nothr_comm_matrix)

ncol(thr_comm_matrix) + ncol(nothr_comm_matrix)

comm_matrix <- cbind(thr_comm_matrix, nothr_comm_matrix)

comm_matrix_tot <- t(as.matrix(comm_matrix[1,]))
comm_matrix_tot[,] <- 1
rownames(comm_matrix_tot) <- "total"


database_mammals_archip <- data.frame(matrix(nrow = 19, ncol = length(mammals_tree)))
rownames(database_mammals_archip) <- rownames(comm_matrix)

for (i in 1:length(mammals_tree)){
  results_pd <- pd(comm_matrix, mammals_tree[[i]], include.root=T)
  database_mammals_archip[,i] <- results_pd$PD
  }


database_mammals_archip$PD <- rowMeans(database_mammals_archip[,1:1000])
database_mammals_archip$Archip <- rownames(database_mammals_archip)
database_mammals_archip <- database_mammals_archip[,1001:1002]
rownames(database_mammals_archip) <- NULL
database_mammals_archip$Area <- NA
database_mammals_archip <- database_mammals_archip[,c(2, 3, 1)]

for (i in 1:nrow(database_mammals_archip)){

  sub_archip <- data.endemic.mammals[data.endemic.mammals$REGION == database_mammals_archip[i,1],]
  sub_archip_area <- unique(sub_archip %>% select(ULM_ID, Area))
  database_mammals_archip[i,2] <- sum(sub_archip_area$Area)

  }
  
rm(comm_matrix, comm_matrix_tot, database_nothr_mammals_archip, database_thr_mammals_archip, mammals_tree, nothr_comm_matrix, results_pd,sub_archip, sub_archip_area, thr_comm_matrix, i)


database_mammals_thr_archip <- as.data.frame(cbind(database_mammals_archip$Area, database_mammals_archip$PD))
colnames(database_mammals_thr_archip) <- c("Area","PD_obs")
rownames(database_mammals_thr_archip) <- database_mammals_archip$Archip


##----------------------------
## DIVERSITY AREA RELATIONSHIP
##----------------------------
sars_models() #list the 20 functions used
display_sars_models() #information list about the 20 functions

####Standardized Average Model####
## Averaged Model PD ##
PD_averaged <- database_mammals_thr_archip
PD_averaged$PD_obs <- PD_averaged$PD_obs/max(PD_averaged$PD_obs)

fit_PD <- sar_average(data = PD_averaged)
#20 remaining models used to construct the multi  PAR:
#Power, PowerR, Extended Power model 1, Extended Power model 2, Persistence function 1, Persistence function 2, Logarithmic, Kobayashi, MMF, Monod, Negative exponential, Chapman Richards, Cumulative Weibull 3 par., Asymptotic regression, Rational function, Gompertz, Cumulative Weibull 4 par., Beta-P cumulative, Heleg(Logistic), Linear model

plot(fit_PD)
summary(fit_PD)
plot(fit_PD, type = 'multi',allCurves = FALSE)
plot(fit_PD, type='bar')
fit_PD$details
str(fit_PD)
fitted_PD_values <- fit_PD$mmi

####Residuals Computation####
database_mammals_thr_archip <- database_mammals_thr_archip[order(database_mammals_thr_archip$Area, decreasing=F), ]
database_mammals_thr_archip$PD_obs <- database_mammals_thr_archip$PD_obs/max(database_mammals_thr_archip$PD_obs)
database_mammals_thr_archip$Fitted_values <- fitted_PD_values
database_mammals_thr_archip$Residuals <- database_mammals_thr_archip$PD_obs - database_mammals_thr_archip$Fitted_values
database_mammals_thr_archip$Archip <- rownames(database_mammals_thr_archip)
colnames(database_mammals_thr_archip)
database_mammals_thr_archip <- database_mammals_thr_archip[,c(5, 1:4)]
rownames(database_mammals_thr_archip) <- NULL
database_mammals_thr_archip <- database_mammals_thr_archip[order(database_mammals_thr_archip$Residuals, decreasing=T), ]
database_mammals_thr_archip$Rank <- 1:nrow(database_mammals_thr_archip)
write.table(database_mammals_thr_archip, "outputs/DAR_PD_Archip.txt", row.names = FALSE)

####DATA FOR GRAPH####
class(fit_PD)
str(fit_PD)

data_graph_PD <- data.frame(matrix(nrow=nrow(database_mammals_thr_archip), ncol=3))
colnames(data_graph_PD) <- c("Area", "Diversity", "Curve")
data_graph_PD$Area <- database_mammals_thr_archip$Area
data_graph_PD$Diversity <- database_mammals_thr_archip$PD_obs
data_graph_PD <- data_graph_PD[order(data_graph_PD$Area, decreasing=F), ]
data_graph_PD$Curve <- fit_PD$mmi
data_graph_PD

class(data_graph_PD)
class(data_graph_PD$Area)
class(data_graph_PD$Diversity)
class(data_graph_PD$Curve)

data_graph_PD <- left_join(data_graph_PD, database_mammals_thr_archip, by="Area")
data_graph_PD <- data_graph_PD[,c(4, 8, 1:3)]
write.table(data_graph_PD, "outputs/DAR_PD_Archip_graph.txt", row.names = FALSE)

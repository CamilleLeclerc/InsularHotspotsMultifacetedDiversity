##--------------------------
## LOAD PACKAGES & FUNCTIONS
##--------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)
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




database.mammals.archip <- data.frame(matrix(nrow = 19, ncol = 5))
colnames(database.mammals.archip) <- c("Archip", "Area", "Nb.islands", "Nb.sp.tot", "Nb.sp.thr")
database.mammals.archip$Archip <- unique(data.endemic.mammals$REGION)

for (i in 1:nrow(database.mammals.archip)){

  sub.archip <- data.endemic.mammals[data.endemic.mammals$REGION == database.mammals.archip[i,1],]
  database.mammals.archip[i,4] <- length(unique(sub.archip$binomial))
  database.mammals.archip[i,3] <- length(unique(sub.archip$ULM_ID))
  
  sub.archip.area <- unique(sub.archip %>% select(ULM_ID, Area))
  database.mammals.archip[i,2] <- sum(sub.archip.area$Area)

  sub.archip.thr <- sub.archip[sub.archip$category != "DD" & sub.archip$category != "LC" & sub.archip$category != "NT",]
  database.mammals.archip[i,5] <- length(unique(sub.archip.thr$binomial))

  }

rm(i, sub.archip, sub.archip.area, sub.archip.thr)




database.mammals.thr.archip <- database.mammals.archip %>% select(Area, Nb.sp.thr)
colnames(database.mammals.thr.archip) <- c("Area","TD.brut")
rownames(database.mammals.thr.archip) <- database.mammals.archip$Archip


##----------------------------
## DIVERSITY AREA RELATIONSHIP
##----------------------------
sars_models() #list the 20 functions used
display_sars_models() #information list about the 20 functions

####Standardized Average Model####
## Averaged Model TD ##
TD.averaged <- database.mammals.thr.archip
TD.averaged$TD.obs <- TD.averaged$TD.brut/max(TD.averaged$TD.brut)

fit.TD <- sar_average(data = TD.averaged %>% select(Area, TD.obs))
#20 remaining models used to construct the multi  SAR:
#Power, PowerR, Extended Power model 1, Extended Power model 2, Persistence function 1, Persistence function 2, Logarithmic, Kobayashi, MMF, Monod, Negative exponential, Chapman Richards, Cumulative Weibull 3 par., Asymptotic regression, Rational function, Gompertz, Cumulative Weibull 4 par., Beta-P cumulative, Heleg(Logistic), Linear model

plot(fit.TD)  
summary(fit.TD)
plot(fit.TD, type = 'multi', allCurves = FALSE)
plot(fit.TD, type='bar')
fit.TD$details
str(fit.TD)
fitted.TD.values <- fit.TD$mmi

####Residuals Computation####
database.mammals.thr.archip <- database.mammals.thr.archip[order(database.mammals.thr.archip$Area, decreasing=F), ]
database.mammals.thr.archip$TD.obs <- database.mammals.thr.archip$TD.brut/max(database.mammals.thr.archip$TD.brut)
database.mammals.thr.archip$Fitted.values <- fitted.TD.values
database.mammals.thr.archip$Residuals <- database.mammals.thr.archip$TD.obs - database.mammals.thr.archip$Fitted.values
database.mammals.thr.archip$Archip <- rownames(database.mammals.thr.archip)
colnames(database.mammals.thr.archip)
database.mammals.thr.archip <- database.mammals.thr.archip[,c(6, 1:5)]      
rownames(database.mammals.thr.archip) <- NULL
database.mammals.thr.archip <- database.mammals.thr.archip[order(database.mammals.thr.archip$Residuals, decreasing=T), ]
database.mammals.thr.archip$Rank <- 1:nrow(database.mammals.thr.archip) 
write.table(database.mammals.thr.archip, "outputs/DAR_TD_Archip.txt", row.names = FALSE)

####DATA FOR GRAPH####
class(fit.TD)
str(fit.TD)

data.graph.TD <- data.frame(matrix(nrow=nrow(database.mammals.thr.archip), ncol=3))
colnames(data.graph.TD) <- c("Area", "Diversity", "Curve")
data.graph.TD$Area <- database.mammals.thr.archip$Area
data.graph.TD$Diversity <- database.mammals.thr.archip$TD.obs
data.graph.TD <- data.graph.TD[order(data.graph.TD$Area, decreasing=F), ]
data.graph.TD$Curve <- fit.TD$mmi
data.graph.TD

class(data.graph.TD)
class(data.graph.TD$Area)
class(data.graph.TD$Diversity)
class(data.graph.TD$Curve)

data.graph.TD <- left_join(data.graph.TD, database.mammals.thr.archip, by="Area")
data.graph.TD <- data.graph.TD[,c(4, 8, 1:3)]
write.table(data.graph.TD, "outputs/DAR_TD_Archip_graph.txt", row.names = FALSE)

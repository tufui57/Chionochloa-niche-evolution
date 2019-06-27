########################################################################################
### Quantification of niche volume
########################################################################################

### Original method in ecospat to calculate Schonner's D
library(ecospat)
library(nichePlot)
source(".//functions//F_SchonnerDdataframeFormat.R")

genus_name="Chionochloa"
# Load PCA scores
if(genus_name == "Acaena"){
  load(".\\Scores_Acaena_landcover5km.data")
}else{
  load(".\\Scores_chion_landcover_worldclim1_5km.data")

}


# Species names
spname <- colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))]
# Species that doesn't have < 5 records can't be used in this analysis.
spname2 <- spname[!(sapply(spname, function(i){sum(scores[,i]) < 5}))]

########################################################################################
### Genus niche volume
########################################################################################
genusd <- scores[rowSums(scores[, spname2], na.rm = T) > 0, ]

D <-  SchoenerD_ecospat(background = scores, "PC1", "PC2",
                    data1 = genusd,  data2 = scores)


########################################################################################
### Genus niche overlap
########################################################################################

genus_name="Acaena"
load(".\\Scores_Acaena_landcover5km.data")
# Species names
spname <- colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))]
# Species that doesn't have < 5 records can't be used in this analysis.
spname2 <- spname[!(sapply(spname, function(i){sum(scores[,i]) < 5}))]
aca <- scores[rowSums(scores[, spname2], na.rm = T) > 0, ]

genus_name="Chionochloa"
load(".\\Scores_chion_landcover_worldclim1_5km.data")
# Species names
spname <- colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))]
# Species that doesn't have < 5 records can't be used in this analysis.
spname2 <- spname[!(sapply(spname, function(i){sum(scores[,i]) < 5}))]
chion <- scores[rowSums(scores[, spname2], na.rm = T) > 0, ]

D <-  SchoenerD_ecospat(background = scores, "PC1", "PC2",
                        data1 = aca,  data2 = chion)

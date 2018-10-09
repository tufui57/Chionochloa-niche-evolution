#####################################################################################
## Calculate speccies persistent occurrences using Schoenner's D
#####################################################################################

library(ecospat)
library(nichePlot)
library(dplyr)
source(".//functions//F_SchonnerDdataframeFormat.r")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

genus_name <-  "Chionochloa"

###################################
### Data preparation
###################################

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}else{
  genus_tag <- "chion"
}

# Data import
load(paste(".\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")
)

### Omit NA from land cover data
scores <- scores[!is.na(scores$landCoverChange),]

# LGM climate
load(paste(".\\LGM_mainisland_worldclim1_5km_scores.data", sep = "")
)


### Schoenner's D between current climate of species occurrences within primary open habitat and LGM climate
spname <- colnames(scores)[grep(paste("^", genus_name, sep = ""), colnames(scores))]
primaryOpen <- scores[(scores$landCoverChange == "nonF-nonF"),]

background <- rbind(scores[, c("PC1", "PC2")], newdf[, c("PC1", "PC2")])

scho <- list()

for(i in spname){
  ######################### NOTE ##################################################
  # If species has occurrence point with the max values of background's axes,
  # ecospat.grid.clim.dyn() gives the following error;
  # Error in quantile.default(spr, th.env) :
  # missing values and NaN's not allowed if 'na.rm' is FALSE
  
  # Get rid of the points from species occurrences
  scores.sp <- primaryOpen[(primaryOpen[, i] == 1), ]
  removeThisRow <- which(scores.sp$PC2 == max(background$PC2))
  
  # Number of points (spcies occurrences) must be > 5 in order to calculate Schonner's D.
  # Species whose occurrences within primary oepn habitat is < 5 are omitted.
  if(length(removeThisRow) == 0){
    scores.sp2 <- scores.sp
  }else{
    scores.sp2 <- scores.sp[-removeThisRow,]
  }
  
  if(nrow(scores.sp2) > 5){
    scho[[i]] <- SchoenerD_ecospat(background, "PC1", "PC2", newdf, scores.sp2)
  }else{
    print(paste(i, "has < 5 occurrences in primary open habitat at 5km resolution"))
    scho[[i]] <- "This species has < 5 occurrences"
  }
  
}

scho2 <- lapply(scho[scho != "This species has < 5 occurrences"], "[[", 1)
write.csv(do.call(rbind, scho2), file = paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))




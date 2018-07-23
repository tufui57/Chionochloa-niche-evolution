#####################################################################################
## Calculate speccies persistent occurrences using Schoenner's D
#####################################################################################

library(ecospat)
library(nichePlot)
library(dplyr)
source(".//Acaena niche evolution//F_SchonnerDdataframeFormat.r")
source(".//Acaena niche evolution//F_Create_Package_speciseNameCleaning.r")

genus_name <- "Chionochloa" # "Acaena"

###################################
### Data preparation
###################################

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}else{
  genus_tag <- "chion"
}

# Data import
load(paste(".//Scores_", genus_tag, ".data", sep=""))

# LGM climate
load(paste(".\\LGM_mainisland_worldclim",
           Worldclim, "_", reso, "km_scores.data", sep = "")
)


### Schoenner's D between current climate of species occurrences within primary open habitat and LGM climate
spname <- colnames(scores)[grep("^Chion", colnames(scores))]
primaryOpen <- scores[scores$landCoverChange == "NF-nonF",]

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
    scho[[i]] <- "This species has < 5 occurrences"
  }
  
}

scho2 <- lapply(scho[scho != "This species has < 5 occurrences"], "[[", 1)
write.csv(do.call(rbind, scho2), file = paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))



#################################################################################################################
## Compare the above Schoenner's D with the proportion of occurrences within open area with persistent climate
#################################################################################################################

persistent0.045 <- read.csv(".//persistentRatio_age_chion5km0.045.csv")
schoD <- read.csv(paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))

persistent0.045_2 <- merge(persistent0.045, schoD, by.x="spname", by.y="X")

# Name tag
tag <- makeTag_separate(persistent0.045_2$spname, genus_name, "_")

### Plot
png(paste(".//persistentRatio_chion_0.045_vs_schonnerD_", genus_tag, ".png", sep=""), width = 800, height = 500)
plot(persistent0.045_2$D, persistent0.045_2$persistentRatio,
     xlab = "Schoener's D", ylab = "Radius = 0.045",
     main = "Proportion of species occurrences within open area with persistent climate"
)
text(persistent0.045_2$D, persistent0.045_2$persistentRatio, labels = tag$tag, cex = 1.2, pos=1)
dev.off()

summary(lm(persistent0.045_2$D ~ persistent0.045_2$persistentRatio))


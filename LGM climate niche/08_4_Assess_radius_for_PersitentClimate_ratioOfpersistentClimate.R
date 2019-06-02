#################################################################################
### Ratio of lands with persistent climate
#################################################################################

source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)

### Prepare climate similarity

scores$similarityLevel <- rowSums(scores[, grep("lgm", colnames(scores))])

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))

# Plot simialrity levels on NZ map
similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "similarityLevel")

#################################################################################
### Ratio of lands with persistent climate
#################################################################################

# How many of the land in NZ have peristent climate?
# Persistent climate is defined as the grid cells with similarity leve l>= 18.
similarScore <- (scores$similarityLevel > 17) %>%  scores[.,]
nrow(similarScore) / nrow(scores)

# How many of the land in NZ currently have different climate from the LGM?
nonsimilarScore <- (scores$similarityLevel == 0) %>%  scores[.,]
nrow(nonsimilarScore) / nrow(scores)

# How many of the land in NZ have primary open area with persistent climate?
similarScore <- (scores$similarityLevel > 17) %>%  scores[.,]
primaryOpen.persistentClimate <- similarScore[similarScore$landCoverChange == "nonF-nonF", ]
nrow(primaryOpen.persistentClimate) / nrow(scores)

# How many of the persistent climate is found in primary open area?
nrow(primaryOpen.persistentClimate) / nrow(similarScore)

# Where is the priamry open habitat with persistent cliamte?
plot(primaryOpen.persistentClimate$x, primaryOpen.persistentClimate$y)


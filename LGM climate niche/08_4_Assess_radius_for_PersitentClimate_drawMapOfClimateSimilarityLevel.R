
source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")
source(".//Chionochloa niche evolution//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of climate similarity levels
#################################################################################

scores$similarityLevel <- rowSums(scores[, grep("lgm", colnames(scores))])

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))

similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "similarityLevel")

png("Y:\\ClimateSimilarityLavel_sinceLGM.png")
plot(similarityLevelRaster)
dev.off()
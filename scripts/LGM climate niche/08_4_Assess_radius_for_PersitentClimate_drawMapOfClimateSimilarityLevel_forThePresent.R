
# Give genus name before running the following scripts
genus_name = "Acaena"

source(".//Chionochloa niche evolution//scripts//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F02_create_raster_from_dataframe.R")
library(raster)

#################################################################################
### Draw a map of climate similarity levels of current area
#################################################################################

scores$similarityLevel <- rowSums(scores[, grep("lgm", colnames(scores))])

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))

# Plot simialrity levels on NZ map
similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores,
                                                     coordinateCols = c("x", "y"),
                                                     cellvalueCol = "similarityLevel")

# Colour gradient for raster
colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))

png("Y:\\ClimateSimilarityLavel_sinceLGM.png")

plot(similarityLevelRaster,
     col=colfunc(21),
     axes=FALSE, box=FALSE,
     legend.args=list(text='Climate similarity', side=4, font=2, line=2.5, cex=0.8)
     )
dev.off()




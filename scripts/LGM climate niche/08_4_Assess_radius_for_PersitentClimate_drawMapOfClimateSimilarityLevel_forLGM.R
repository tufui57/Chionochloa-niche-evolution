#################################################################################
### Draw a map of climate similarity levels of LGM area
#################################################################################

#################################################################################
### Calculate climate similarity levels of grid cells in LGM land area
#################################################################################

# Radius size
a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.005))

# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)

for(i in a){
  # Load past PCA scores with the LGM neighbourhood data
  load(paste("Y://3rd chpater_niche filling//meta data//Assessment of climate persistence//currentNicheSimilarToLGM_", 
             i, "_chion_",reso, "km.data", sep = ""))
  
  # Add availability of climate niche in LGM to current PCA scores
  scores[, paste("lgm", i, sep="")] <- ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0)
  
}

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]

# Species name
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]


### Not completed

# 
# # Reference raster of coordinate system & extent
# # This raster mustn't be used for resampling for 5km resolution.
# ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))
# 
# similarityLevelRaster <- convert_dataframe_to_raster(ref = ref5, dat = scores,
#                                                      coordinateCols = c("x", "y"),
#                                                      cellvalueCol = "similarityLevel")
# 
# # Colour gradient for raster
# colfunc <- colorRampPalette(c("cyan", "dodgerblue4"))
# 
# png("Y:\\ClimateSimilarityLavel_sinceLGM.png")
# 
# plot(similarityLevelRaster,
#      col=colfunc(21),
#      axes=FALSE, box=FALSE,
#      legend.args=list(text='Climate similarity', side=4, font=2, line=2.5, cex=0.8)
# )
# dev.off()

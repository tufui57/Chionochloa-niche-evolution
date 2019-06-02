############################################################################################################
# Elevation data
############################################################################################################

# Import data
dat <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim1_5km.csv", sep="")
         )

spname <- colnames(dat)[grepl(paste("^", genus_name, sep = ""), colnames(dat))]

# Mean elevations of all species occurrence records
genusOcc <- dat[rowSums(dat[, spname], na.rm = T) > 0, ]
mean(genusOcc$value)

# Mean elevations for each species occurrence records on 5km grid resolution
ele <- sapply(spname, function(x){
  sprow <- which(dat[, x] == 1)
  alt <- mean(dat[sprow, "value"])
  return(alt)
  }
)

write.csv(ele, paste(".//", genus_name, "_elevation.csv", sep=""))

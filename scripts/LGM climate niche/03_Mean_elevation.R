############################################################################################################
# Elevation data
############################################################################################################

# Import data
dat <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv", sep="")
         )

spname <- colnames(dat)[grepl(paste("^", genus_name, sep = ""), colnames(dat))]

ele <- sapply(spname, function(x){
  sprow <- which(dat[, x] == 1)
  alt <- mean(dat[sprow, "value"])
  return(alt)
  }
)

write.csv(ele, paste(".//", genus_name, "_elevation.csv", sep=""))

############################################################################################################
# Create climate data at 5km resolution
############################################################################################################

library(raster)

### Extract bioclim data
# Import bioclim 2.5 min (5km) raster data
files <- paste("Y:\\GIS map and Climate data\\worldclim\\wc2.0_2.5m_bio", 
               list.files("Y:\\GIS map and Climate data\\worldclim\\wc2.0_2.5m_bio"),
               sep="\\"
)

bio <- lapply(files[grep("tif$", files)], raster)

# Change names to match occurrence data columns
nam <- sapply(sapply(files[grep("bil$", files)], strsplit, "\\\\|.bil"), "[[", 5)
nam2 <- gsub("_411", "", gsub("bio", "bioclim", nam))
names(bio) <- nam2

# Raster stack
bioNZ <- stack(bio)

save(bioNZ, file = "Y:\\GIS map and Climate data\\worldclim\\wc2.0_2.5m_19bioclim.data")

############################################################################################################
# Project coordinate system of bioclim rasters from WGS84 to NZTM
############################################################################################################

############################################################################################################
# Relate climate raster to occurrence and land cover data
############################################################################################################

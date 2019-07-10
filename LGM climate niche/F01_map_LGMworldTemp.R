################################################################################
### World climate map at the Last Glacial Maximum(22,000 years ago)
################################################################################

library(dplyr)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(grid)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)


####################################################
### Import past climate raster
####################################################
time = "mrlgmbi_2-5m"

## Data import
pathbio <- paste("Y:\\GIS map and Climate data\\worldclim\\", time, sep = "")

files <- list.files(pathbio)

# If the raster file is tif file
if(sum(grepl("tif$", files)) > 0){
  ext <- ".tif"
}

# If the raster file is bil file
if(sum(grepl("bil$", files)) > 0){
  ext <- ".bil"
}

string <- paste("bi1", ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
  gsub('.{1}$', '', .)

### Note! The order of variables is automatically changed into alphabetically and numerically.
rasters <- raster(paste(pathbio, files[grepl(string, files)], sep = "\\"))

png("temp_worldmap_LGM.png")
plot(rasters)
dev.off()

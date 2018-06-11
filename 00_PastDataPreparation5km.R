################################################################################
### Climatic niche 
### in Last Glacial Maximum(22,000 years ago), Last inter-glacial(120,000 - 140,000 years ago), Mid Holocene(6000 years ago)
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

# Genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

# Reference raster
ref <- raster("Y:\\GIS map and Climate data\\current_landcover5km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)

########################################
### Get PCA axes of current climate
########################################
# Data import
da1 <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_history5km.csv", sep = ""))
d <- da1[is.na(da1$landCoverChange) == F, ]

spname <- grepl(genus_name, colnames(d)) %>% colnames(d)[.]

for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", vars, sep = "")],
              center = TRUE,
              scale. = TRUE,
              retx = TRUE
)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], spname,
                           "x", "y", "preLandcover", "currentLandcover", "landCoverChange")], pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)

save(scores, file = paste(".\\Scores_", genus_tag,"_landcover5km.data", sep = ""))

####################################################
### Calculate PC values for past climate
####################################################

# Center and scale the data
c.fun <- function(df, center, scale) {
  return((df-center)/scale )
}

centeredData <- apply(d[, paste("bioclim", vars, sep = "")],
                      MARGIN = 1, FUN = c.fun, pca$center, pca$scale)

# compute the principal components
pcs <- t(pca$rotation) %*% centeredData

### Prepare past climate data
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

string <- paste("bi", vars, ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
  gsub('.{1}$', '', .)

# Get file names of Last inter glacial period
# Files of Last inter glacial period have different file names from others
if(time == "lig_30s_bio"){
  string <- paste("bio_", vars, ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
    gsub('.{1}$', '', .) # Remove last letter
}

### Note! The order of variables is automatically changed into alphabetically and numerically.
rasters <- lapply(paste(pathbio, files[grepl(string, files)], sep = "\\"),
                  raster
)
# Change order of contents
rasters <- rasters[c(1,4,2,3)]



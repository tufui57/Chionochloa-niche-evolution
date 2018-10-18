###############################################################
## Data preparation for drawing maps and PCA
###############################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)

source(".\\functions\\F_plot_map_and_PCA.r")

if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
  
  # data import
  chdata <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv", sep = ""))
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
  # data import
  chdata <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv", sep=""))
}

# species name
spname <- grepl(genus_name, colnames(chdata)) %>% colnames(chdata)[.]

for(i in spname){
  chdata[is.na(chdata[,i]),i] <- 0
}


# Eliminate species without occurrence records
spname <- (colSums(chdata[, spname]) > 0) %>% spname[.]

# Get PCA axes
pca <- prcomp(chdata[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(chdata[, c(colnames(chdata)[grep("^bio", colnames(chdata))], spname, "x", "y")],
                     pca$x[, 1:2])

# reference rasters
ref <- raster("Y://GIS map and Climate data//pre-human_landcover1km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland <- crop(nzland, ref)


extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))


##############################################################################################
###     Draw maps for each sp coloured by land cover change     ###
##############################################################################################

library(dplyr)
library(raster)
library(rgdal)
library(maptools)

genus_name <- "Chionochloa"
  
if(genus_name == "Chionochloa"){
    genus_tag <- "chion"
    
    # data import
    chdata <- read.csv(paste("Y://Acaena project//", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
  }

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
  # data import
  chdata <- read.csv(paste("Y://Acaena project//", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
  
}

chdata <- chdata[is.na(chdata$landCoverChange) == F, ]

# species name
spname <- grepl(genus_name, colnames(chdata)) %>% colnames(chdata)[.]

for(i in spname){
  chdata[is.na(chdata[,i]),i] <- 0
}

# Reference raster to create point object from coordinates
ref <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)


extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))


###############################################################
## Point map for landcover change of sp 
###############################################################

## Plot map for all species of target genus
chdata$allsp <- ifelse(rowSums(chdata[, spname]) > 0, 1, 0)

map_plot_colourByLandcover("allsp", chdata)
map_plot_monoColour("allsp", chdata)

## Plot map for a species
chdata2 <- lapply(spname, function(i){
  chdata[chdata[,i] == 1,]
  }
  )

maps <- lapply(1:length(spname), function(i){
  map_plot_monoColour(spname[i], chdata2[[i]])
  })


###############################################################
## Plot niche space for landcover change
###############################################################

## Plot niche space for all species of target genus
scores$allsp <- ifelse(rowSums(scores[, spname]) > 0, 1, 0)

niche_plot_colourByLandcover("allsp", scores)
niche_plot_monoColour("allsp", scores)

## Plot niche space for all species of target genus
scores2 <- lapply(spname, function(i){
  scores[scores[,i] == 1,]
}
)

pcas <- lapply(1:length(spname), function(i){
  map_plot_monoColour(spname[i], scores2[[i]])
})


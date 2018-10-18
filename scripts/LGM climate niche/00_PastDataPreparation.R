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


########################################
### Get PCA axes of current climate
########################################
# Data import
da1 <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim",
                      Worldclim, "_", reso, "km.csv", sep = ""
                      )
                )
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

if(file.exists(paste(".\\Scores_", genus_tag,"_landcover_worldclim", Worldclim, "_", reso, "km.data", sep = "")
               )== FALSE
   ){
  save(scores, file = paste(".\\Scores_", genus_tag,"_landcover_worldclim",
                          Worldclim, "_", reso, "km.data", sep = ""
                          )
     )
}

####################################################
### Import past climate raster
####################################################

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

### Note! The order of variables is automatically changed into alphabetically and numerically.
rasters <- lapply(paste(pathbio, files[grepl(string, files)], sep = "\\"),
                  raster
)

# Get file names of Last inter glacial period
# Files of Last inter glacial period have different file names from others
if(time == "lig_30s_bio"){
  string <- paste("bio_", vars, ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
    gsub('.{1}$', '', .) # Remove last letter
  
  # Name of rasters
  rastername <- files[grepl("bil$", files)] %>% gsub("_", "",.) %>% gsub("lig30s", "",.) %>% gsub(".bil", "",.)
}

if(time == "mrlgmbi_2-5m"){
  rastername <- sapply(rasters, names) %>% gsub("mrlgm", "",.)
}


# Name rasters
names(rasters) <- rastername

# Change order of contents
rasters <- rasters[c(1,4,2,3)]



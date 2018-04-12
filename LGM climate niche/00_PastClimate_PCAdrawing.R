################################################################################
### Climatic niche 
### in Last Glacial Maximum(22,000 years ago), Last inter-glacial(120,000 - 140,000 years ago), Mid Holocene(6000 years ago)
################################################################################

library(raster)
library(dplyr)

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m"
  #"ccmidbi_30s"

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)


genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

#load(".//Scores_acaena_landcover.data")

########################################
### Get PCA axes of current climate
########################################
# Data import
da1 <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
d <- da1[is.na(da1$landCoverChange) == F, ]

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


#save(scores, file = ".//Scores_acaena_landcover.data")

####################################################
### Calculate PC values for past climate
####################################################

# Center and scale the data
c.fun <- function(df, center, scale) {
  return((df-center)/scale )
}

centeredData <- apply(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
                      MARGIN = 1, FUN = c.fun, pca$center, pca$scale)

# compute the principal components
pcs <- t(pca$rotation) %*% centeredData

# compare with results of prcom (option retx=TRUE gives ^cs in x)
head(t(pcs))
head(pca$x)
# check if results are the same
sum(abs(t(pcs)-pca$x))

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

# Resample raster
nz <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")
nzras <- lapply(rasters, resample, nz) %>% 
  lapply(., crop, extent(c(160,185), c(-55,-30))) # Past terrestrial area of Zealandia is different from the one of current NZ

lgmdata <- lapply(nzras, values) %>% do.call(cbind, .)

lgmdata2 <- lgmdata[!is.na(lgmdata[, 1]), ]

### Compute the components for new data
newdata <- data.frame(lgmdata2)
centeredNewData <- apply(newdata, MARGIN = 1, FUN = c.fun, pca$center, pca$scale)
pcsnew <- t(pca$rotation) %*% centeredNewData

newdf <- data.frame(t(pcsnew))
colnames(newdf) <- paste("PC", 1:4, sep = "")


#################################################################################
### Find current 1km grid cells within neighbourhood of past available climate niche
#################################################################################

# how do you decide the distance?
a = 0.001
### NOTE; the following takes time circa. half an hour.
neighbours <- neighbours_within_a_squire(newdf, scores, a = 0.001, coordinateNames = c("PC1", "PC2"))

save(neighbours, file=".//currentNicheSimilarToLGM.data")

mutate(scores,  lgm = lapply(neighbours, nrow) %>% unlist)


#################################################################################
### Plot Climatic niche space change from past to the present
#################################################################################
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

pMain <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = newdf, aes(PC1, PC2), color = 'red', alpha = 0.25) 

ggsave(paste(time, ".png", sep = ""), pMain)

### Where has been available before and after the past?
# Primary open habtat
ol <- scores[scores[, "landCoverChange"] == "nonF-nonF", c("PC1", "PC2", "landCoverChange")]

p <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  # point of each sp
  geom_point(data = ol, aes(PC1, PC2), color = 'blue', alpha = 0.05) +
  geom_point(data = newdf, aes(PC1, PC2), color = 'red', alpha = 0.005) 

ggsave(paste(time, "_primaryOpenHabitat.png", sep = ""), p)



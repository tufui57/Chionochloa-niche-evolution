###############################################################
## Data preparation 
###############################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)

source(".\\Acaena niche evolution\\F_plot_map_and_PCA.r")


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


# Eliminate species without occurrence records
spname <- (colSums(chdata[, spname]) > 0) %>% spname[.]


# Get PCA axes
pca <- prcomp(chdata[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(chdata[, c(colnames(chdata)[grep("^bio", colnames(chdata))], spname,
                                "x", "y", 
                                "landCoverChange", "currentLandcover", "preLandcover")],
                     pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)

# Convert landcover ID to names
scores$pre <- factor(ifelse(scores$preLandcover == 1, "NF", "nonF"))
scores$current <- factor(ifelse(scores$currentLandcover == 1, "NF",
                                ifelse(scores$currentLandcover == 3, "nonF", "non potential habitat" # EF(2) is also non potential habitat
                                ))
)


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


###############################################################
## Plot map and niche space 
###############################################################


lapply(seq(1,length(spname),3), function(i){
  
  png(paste("Y://niche_map_3sp", spname[i], ".png", sep=""), width = 800, height = 1300)
  
  pMain1 <- niche_plot_colourByLandcover(spname[i], scores)
  pMap1 <- map_plot_colourByLandcover(spname[i], chdata)
  title1 <- textGrob(gsub("_", " ", spname[i]), gp = gpar(fontface = "bold", cex = 1.75))
  spi <- grid.arrange(pMap1, pMain1, top = title1, ncol = 2, widths = c(3,5))
  
  pMain2 <- niche_plot_colourByLandcover(spname[i+1], scores)
  pMap2 <- map_plot_colourByLandcover(spname[i+1], chdata)
  title2 <- textGrob(gsub("_", " ", spname[i+1]), gp=gpar(fontface="bold", cex=1.75))
  spi2 <- grid.arrange(pMap2, pMain2, top = title2, ncol = 2, widths = c(3,5))
  
  pMain3 <- niche_plot_colourByLandcover(spname[i+2], scores)
  pMap3 <- map_plot_colourByLandcover(spname[i+2], chdata)
  title3 <- textGrob(gsub("_", " ", spname[i+2]), gp=gpar(fontface="bold", cex=1.75))
  spi3 <- grid.arrange(pMap3, pMain3, top = title3, ncol = 2, widths = c(3,5))
  
  # Plot in multiple panels
  #grid.arrange(pMap1, pMain1, pMap2, pMain2, pMap3, pMain3, ncol = 2, nrow = 3, widths = c(3,5))
  grid.arrange(spi, spi2, spi3, nrow = 3)
  
  dev.off()
  
})

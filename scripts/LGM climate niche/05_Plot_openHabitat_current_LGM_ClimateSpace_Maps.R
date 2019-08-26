#################################################################################
### Plot map of primary open area, current and LGM climate
#################################################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)

########################################
### Data preparation
########################################

load("Y:\\5th chapter SAI chapter\\raw data\\Scores_Acaena_landcover_worldclim1_5km.data")

# Climate availability
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

scores2 <- scores[!is.na(scores$landCoverChange), ]

# Primary open habtat
primary <- scores2[scores2[, "landCoverChange"] == "nonF-nonF", ]

# Secondary open habitats 
secondary <- scores2[scores2[, "landCoverChange"] == "NF-nonF", ]

# Load LGM climate scores
load(".\\LGM_mainisland_worldclim1_5km_scores.data")
# Extents
extent_x = c(min(scores2$PC1), max(scores2$PC1))
extent_y = c(min(scores2$PC2), max(scores2$PC2))

###############################################################
## Map data preparation
###############################################################

# Reference raster
ref <- raster(paste("Y:\\GIS map and Climate data\\current_landcover5km.bil", sep = ""))

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)


#####################
# Map
#####################

# Convert land cover change column
scores2$changeNo <- NA
# Primary open habtat
primary <- scores2[scores2[, "landCoverChange"] == "nonF-nonF", ]

# Prepare a polygon of LGM terrestrial area
if(file.exists(".\\LGM_mainisland\\mainIsland.shp")){
  mainIsland <- readOGR(".\\LGM_mainisland\\mainIsland.shp")
}else{
  source(".//Chionochloa niche evolution//scripts//LGM climate niche//01_SpatialDataPreparation.R")
}

### Plot map
map <- ggplot() +
  # LGM outline
  geom_polygon(data = mainIsland, aes(x = long, y = lat, group = group), colour = "lightpink3", fill = 'lightpink') +
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
  # Primary open habitat
  geom_point(data = primary, aes(x = x, y = y), color = "blue", size = 0.001) +
  guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
  labs(x="", y="") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size = 23),
        legend.justification = c(1, 1), legend.position = c(0.4, 0.9),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

niche <- ggplot() +
  ### Be careful with the order to overlap points. Should be same as maps
  # LGM climate
  geom_point(data = newdf, aes(PC1, PC2), color = "lightpink")

png("Y://niche_map_worldclim1_5km.png",
    width = 1200, height = 650)
grid.arrange(map, niche, ncol = 2, widths = c(3,5))
dev.off()






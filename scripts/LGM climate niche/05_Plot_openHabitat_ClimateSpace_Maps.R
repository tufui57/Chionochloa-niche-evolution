#################################################################################
### Plot map and PCA of persistent climate of open habitat
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

genus_name <- "Chionochloa"

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

########################################
### Plot cliamte space
########################################

niche <- ggplot() +
  ### Be careful with the order to overlap points. Should be same as maps
  # LGM climate
  geom_point(data = newdf, aes(PC1, PC2), color = "lightpink") +
  # NZ
  geom_point(data = scores2, aes(PC1, PC2), color = 'gray90') +
  # Primary open area
  geom_point(data = primary, aes(PC1, PC2), color = "blue") +
  # Secondary open habitat
  geom_point(data = secondary, aes(PC1, PC2), color = "red") +
  # extent
  xlim(extent_x) +
  ylim(extent_y) +
  # No background
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
        )

#####################
# Map
#####################

# Convert land cover change column
scores2$changeNo <- NA
# Primary open habtat
scores2[scores2[, "landCoverChange"] == "nonF-nonF", "changeNo"] <- "Primary"
# Secondary open habitats 
scores2[scores2[, "landCoverChange"] == "NF-nonF", "changeNo"] <- "Secondary"

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
  geom_point(data = scores2, aes(x = x, y = y, color = changeNo), size = 0.001) +
  scale_colour_manual(
    # title
    name = "",
    breaks = c("Primary","Secondary"),
    label = c("Primary\nopen area", "Secondary\nopen area"),
    # colours
    values = c("blue", "red")
  ) +
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

png("Y://Persistent_niche_map_worldclim1_5km.png",
    width = 1200, height = 650)
grid.arrange(map, niche, ncol = 2, widths = c(3,5))
dev.off()




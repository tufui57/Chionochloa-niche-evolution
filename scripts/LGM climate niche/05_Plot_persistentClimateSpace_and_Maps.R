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

########################################
### Niche data preparation
########################################

genus_name <- "Chionochloa"

worldclim=1
reso=5

# Create genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

########################################
### Climate score preparation
########################################
load(paste("Y:\\Scores_", genus_tag,"_landcover_worldclim",
           worldclim, "_", reso, "km.data", sep = ""
)
)

# Climate simialrity levels
source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
scores$similarityLevel <- rowSums(scores[, grep("lgm", colnames(scores))])

# Persistent climate is defined as the grid cells with similarity leve l>= 18.
persistentClimate <- (scores$similarityLevel > 17) %>%  scores[.,]

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]

# Primary open habtat with persistent climate
primaryPersistent <- persistentClimate[persistentClimate[, "landCoverChange"] == "nonF-nonF", ]


# Load LGM climate scores
load(paste(".\\LGM_mainisland_worldclim",
           worldclim, "_", reso, "km_scores.data", sep = ""))
# Extents
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

###############################################################
## Map data preparation
###############################################################

# Data import
d <- read.csv(paste("Y://2nd chapter_phylogentic niche conservation//meta data//", genus_name, "_bioclim_landcover_history_worldclim",
                    worldclim, "_", reso, "km.csv", sep=""
))

# Reference raster
ref <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep = ""))

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
  geom_point(data = scores, aes(PC1, PC2), color = 'gray90') +
  # Primary open area
  geom_point(data = primary, aes(PC1, PC2), color = "blue") +
  # Persistent open habitat
  geom_point(data = primaryPersistent, aes(PC1, PC2), color = "red") +
  # extent
  xlim(extent_x) +
  ylim(extent_y) +
  # No background
  theme(axis.title = element_text(size = 15),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




#####################
# Map
#####################

# Convert land cover change column
primary$changeNo <- NA
primary[primary$similarityLevel > 17, "changeNo"] <- "Persistent"
primary[primary$similarityLevel <= 17, "changeNo"] <- "Primary"

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
  geom_point(data = primary, aes(x = x, y = y, color = changeNo), size = 0.001) +
  scale_colour_manual(
    # title
    name = "",
    breaks = c("Persistent", "Primary"),
    label = c("Persistent open area with \n persitent climate","Primary open area"),
    # colours
    values = c("red", "blue")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
  labs(x="", y="") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        legend.justification = c(1, 1), legend.position = c(0.4, 0.9),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

png(paste("Y://Persistent_niche_map_worldclim1_", reso, "km.png", sep=""),
    width = 1200, height = 650)
grid.arrange(map, niche, ncol = 2, widths = c(3,5))
dev.off()




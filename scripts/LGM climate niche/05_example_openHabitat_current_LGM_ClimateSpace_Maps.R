########################################
### PCA drawing
########################################

library(dplyr)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(grid)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)
#"ccmidbi_30s"


### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

genus_name <- "Acaena"

########################################
### Data preparation
########################################

# LGM climate
load(".\\LGM_mainisland_worldclim1_5km_scores.data")

# Current climate
load(".\\Scores_acaena_landcover_worldclim1_5km.data")

# Add cell ID
scores$cellID <- 1:nrow(scores)


########################################################################################################################
### Definition of persistent climate has changed. So, change the following object too. 
########################################################################################################################
# # Persistent climate of open habitat; persistent climate in primary open habitat
# persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]


#################################################################################
### Plot Climate space
#################################################################################

sp <- "Acaena_caesiiglauca"
dat <- scores[scores[, sp] == 1, ]

# Stable open habtat
primary_sp <- dat[dat[, "landCoverChange"] == "nonF-nonF", ]
# Unstable open habtat
unstable_sp <- dat[dat[, "landCoverChange"] != "nonF-nonF", ]

##################################################################
### Plot species climate niche in primary open areas
##################################################################
pMain <- ggplot() +

    # LGM climate
  geom_point(data = newdf, aes(PC1, PC2), color = 'lightpink', alpha = 0.25) +
  # species niche in stable open area
  geom_point(data = primary_sp, aes(PC1, PC2), color = 'green', alpha = 0.25) +
  # # Primary open area
  # geom_point(data = primary, aes(PC1, PC2), color = 'blue', alpha = 0.25) +
  # # species niche in unstable open area
  # geom_point(data = unstable_sp, aes(PC1, PC2), color = 'brown', alpha = 0.25) +

  ylim(c(-2, 5)) +
  xlim(c(-4, 6)) +
  # Font size and background
  theme(text = element_text(size = 30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )


#####################
# Map
#####################

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


### Plot map
map <- ggplot() +
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +

  # species niche in stable open area
  geom_point(data = primary_sp, aes(x = x, y = y), color = 'green', alpha = 0.25) +
  # species niche in unstable open area
  geom_point(data = unstable_sp, aes(x = x, y = y), color = 'brown', alpha = 0.25) +
  
  guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
  labs(x="", y="") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size = 20),
        legend.justification = c(1, 1), legend.position = c(0.4, 0.9),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

##################################################################
### Plot climates with primary open ares in one panel
##################################################################

png("Y://Sp_curernt_LGM_primaryOpen.png", width = 1200, height = 650)

# Plot in multiple panels
grid.arrange(map, pMain, widths = c(3,5))

dev.off()


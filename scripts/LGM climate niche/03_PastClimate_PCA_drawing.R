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

# Resolution of rasters
reso = 5

# Worldclim version
worldclim=1

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

genus_name <- "Acaena"

# Genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

########################################
### Data preparation
########################################

# LGM climate
load(paste(".\\LGM_mainisland_worldclim",
           worldclim, "_", reso, "km_scores.data", sep = "")
)
# Current climate
load(paste(".\\Scores_", genus_tag,"_landcover_worldclim",
           worldclim, "_", reso, "km.data", sep = ""))

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]


#################################################################################
### Plot Climate space
#################################################################################

#################################
### Plot LGM Climate space 
#################################
pMain <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = newdf, aes(PC1, PC2), color = 'lightpink', alpha = 0.25) +
  ggtitle("LGM")

ggsave(paste("Y://",time, "_and_current.png", sep = ""), pMain, width = 100, height = 80, units = "mm")


##################################################################
### Plot primary open habitat and LGM climate space 
##################################################################

p <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  # point of each sp
  geom_point(data = primary, aes(PC1, PC2), color = 'brown', alpha = 0.05) +
  ggtitle("Primary open habitat")

ggsave(paste("Y://", time, "_primaryOpenHabitat.png", sep = ""), p, width = 100, height = 80, units = "mm")


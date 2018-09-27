#################################################################################
### Habitat and Climate Persistence 
#################################################################################

###############
### Libraries
###############

library(dplyr)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(grid)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
source(".//functions//F_plotAnalysis_clade_niche.R")

genus_name = "Chionochloa"

# Resolution of geographical grid cells
reso = 5

# Worldclim version
Worldclim <- 1
# Choose past data
time <- "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)


########################################
### Data preparation
########################################
# Create genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

# Load current PCA scores with land cover change history
load(paste(".\\Scores_", genus_tag,"_landcover_worldclim",
           Worldclim, "_", reso, "km.data", sep = ""
)
)

# Load LGM PCA scores
load(paste(".\\LGM_mainisland_worldclim",
           Worldclim, "_", reso, "km_scores.data", sep = "")
)

#################################################################################
### Calculate climate similarity levels
#################################################################################

# Radius size
a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.005))

# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)

for(i in a){
  # Load past PCA scores with the LGM neighbourhood data
  load(paste("Y://3rd chpater_niche filling//meta data//Assessment of climate persistence//currentNicheSimilarToLGM_", 
           i, "_chion_",reso, "km.data", sep = ""))
  
  # Add availability of climate niche in LGM to current PCA scores
  scores[, paste("lgm", i, sep="")] <- ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0)
  
}

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]

# Species name
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]



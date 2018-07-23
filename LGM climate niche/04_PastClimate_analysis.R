#################################################################################
### Habitat and Climate Persistence 
#################################################################################

genus_name = "Chionochloa"

# Radius size "a" mustn't set in this script!
# This script will be used in other scripts!

# Resolution of geographical grid cells
# reso = 5

########################################
### Data preparation
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

source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")


# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)
#"ccmidbi_30s"

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

# Load past PCA scores with the LGM neighbourhood data
load(paste(".//currentNicheSimilarToLGM_", a,"_", genus_tag, "_", reso, "km.data", sep = ""))

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
### Habitat and Climate Persistence 
#################################################################################

# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current PCA scores
# neighbours$dat2cellID has cell ID of the "scores" object.
# Thus, rows of "scores" whose cell ID are found in "neighbours$dat2cellID" are considered to be similar to LGM climate

scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))

# Persistent climate; climate that is shared between LGM and the present. 
persistentClimate <- scoresLGM[scoresLGM$lgm == 1, ]

# Persistent climate of open habitat; persistent climate in primary open habitat
persistentHabitatPersistentClimate <- persistentClimate[persistentClimate[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scoresLGM[scoresLGM[, "landCoverChange"] == "nonF-nonF", ]

# Species name
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]

#################################################################################
### Calculate occurrence ratioin Persistent habitat with persistent climate
#################################################################################

# Persistent occurrence ratio = species occurrences in primary open habitat with persistent climate / occurrences in open habitats

persistent_occurrence_ratio <- function(speciesnumber,
                                        scoresLGM
){

  spOpenOcc <- scoresLGM[scoresLGM[, spname[speciesnumber]] == 1, ] %>% filter(landCoverChange == "NF-nonF" | landCoverChange == "nonF-nonF" )
  persistentOcc <- filter(spOpenOcc, lgm == 1)

  ratio <- nrow(persistentOcc) / nrow(spOpenOcc)
  return(ratio)
}

persistentRatio <- sapply(1:length(spname), persistent_occurrence_ratio, scoresLGM)
persistentRatioData <- data.frame(cbind(spname, persistentRatio))

# Load species age
ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

persistentRatioAge <- merge(persistentRatioData, ageVolData, by = "spname")

persistentRatioAge <- mutate(persistentRatioAge, persistentRatio = as.numeric(as.character(persistentRatioAge$persistentRatio)))

write.csv(persistentRatioAge, file = paste("persistentRatio_age_", genus_tag, reso, "km", a, ".csv", sep = ""))


#################################################################################
### Proportion of current climate with persistent climate
#################################################################################

# Number of 1 km cells in land areas of currnet NZ 
n.nz <- nrow(scores)
# Number of 1 km cells in areas with persistent climate in current NZ
n.persistent <- nrow(persistentClimate)

print("Ratio of current areas with persistent climate")
print(n.persistent/n.nz)

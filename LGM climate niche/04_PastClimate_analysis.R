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

genus_name <- "Chionochloa"

# Choose LGM neighbourhood cell size
a = 0.01
# Load past PCA scores with the LGM neighbourhood data
load(paste(".//currentNicheSimilarToLGM_", a,".data", sep = ""))

# Create genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

# Load current PCA scores with land cover change history
load( paste(".\\Scores_", genus_tag,"_landcover.data", sep = ""))


# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current PCA scores
# neighbours$dat2cellID has cell ID of the "scores" object, thus, rows of "scores" whose cell ID are in "neighbours$dat2cellID"

scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))

# Persistent climate; climate that is shared between LGM and the present. 
scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

# Persistent climate of open habitat; persistent climate in primary open habitat
persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scoresLGM[scoresLGM[, "landCoverChange"] == "nonF-nonF", ]

# Load LGM climate scores
load(".\\LGM_mainisland_scores.data")

# Species name
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]

#################################################################################
### Calculate Persistent occurrence ratio
#################################################################################

# Persistent occurrence ratio = species occurrences in persistent climate of open habitat / occurrences in open habitats

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

write.csv(persistentRatioAge, file = paste("persistentRatio_age_", genus_tag, ".csv", sep = ""))



###############################################################
### Ratio of persistent habitat with persistent climate
###############################################################

# Number of 1 km cells in land areas of currnet NZ 
n.nz <- nrow(scores)
# Number of 1 km cells in land areas of Zealandia in LGM
n.lgm <- nrow(newdf)
# Number of 1 km cells in primary open habitat
n.primary <- nrow(primary)
# Number of 1 km cells in primary open habitat
n.secondary <- nrow(scoresLGM[scoresLGM[, "landCoverChange"] == "NF-nonF", ])

# Number of 1 km cells in persistent habitat (primary open) with persitent climate
n.persistentOpen <- nrow(persistentOpenHabitat)
# Number of 1 km cells in areas with persistent climate in current NZ
n.persistent <- nrow(scoresLGM2)

### Ratio

# Ratio of current areas where has persistent climate
n.persistent/n.nz

# Ratio of areas where has climate that became available after LGM
(n.nz - n.persistent)/n.nz

# Ratio of current areas where has climate that isn't available now
(n.lgm - n.persistent)/n.lgm

# Ratio of persistent habitat with persistent climate
n.persistentOpen / n.primary

# Ratio of persistent habitat with persistent climate among primary open habitat
n.persistentOpen / n.primary

# Ratio of secondary oepn habitat with persistent climate among secondary open habitat
persistent2ndaryOpen <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "NF-nonF", ] 
nrow(persistent2ndaryOpen)/ n.secondary

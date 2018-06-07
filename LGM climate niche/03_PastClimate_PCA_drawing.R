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

load( paste(".\\Scores_", genus_tag,"_landcover.data", sep = ""))

a = 0.001
load(paste(".//currentNicheSimilarToLGM_", a,".data", sep = ""))

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current climate data frame
# neighbours$dat2cellID has "scores" cell ID. SO, rows of "scores" whose cell ID are in "neighbours$dat2cellID"

scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))

# Persistent climate; climate that is shared between LGM and the present. 
scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

# Persistent climate of open habitat; persistent climate in primary open habitat
persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scoresLGM[scoresLGM[, "landCoverChange"] == "nonF-nonF", ]

# Load LGM climate scores
load(".\\LGM_mainisland_scores.data")

#################################################################################
### Plot Climate space
#################################################################################

#################################
### Plot LGM Climate space 
#################################
pMain <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = newdf, aes(PC1, PC2), color = 'red', alpha = 0.25) +
  ggtitle("LGM")

ggsave(paste(time, "_and_current.png", sep = ""), pMain)

#################################
### Plot persisitent Climate
#################################
pMain <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = scoresLGM2, aes(PC1, PC2), color = 'red', alpha = 0.25) +
  ggtitle("LGM")

ggsave(paste(time, "_persistentClimate.png", sep = ""), pMain)

##################################################################
### Plot primary open habitat and LGM climate space 
##################################################################

p <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  # point of each sp
  geom_point(data = primary, aes(PC1, PC2), color = 'blue', alpha = 0.05)

ggsave("primaryOpenHabitat.png", p)

##################################################################
### Plot persistent climate space Where has been available since LGM 
##################################################################

p2 <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = persistentOpenHabitat, aes(PC1, PC2), color = 'red', alpha = 0.25) +
  ggtitle("Persistent climate of open habitat")

ggsave(paste(time, "_persistentOpenHabitat.png", sep = ""), p2)

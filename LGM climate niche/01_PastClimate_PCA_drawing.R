########################################
### Data preparation
########################################

library(raster)
library(dplyr)

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)
#"ccmidbi_30s"

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")
load(".//currentNicheSimilarToLGM130418.data")

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current climate data frame 
scoresLGM <- mutate(scores,  lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1,0))

scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]
persistentHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 


#################################################################################
### Plot Climate space change from past to the present
#################################################################################
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

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

ggsave(paste(time, ".png", sep = ""), pMain)

##################################################################
### Plot primary open habitat and LGM climate space 
##################################################################

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", c("PC1", "PC2", "landCoverChange")]

p <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  # point of each sp
  geom_point(data = primary, aes(PC1, PC2), color = 'blue', alpha = 0.05) +
  geom_point(data = newdf, aes(PC1, PC2), color = 'red', alpha = 0.005) 

ggsave(paste(time, "_primaryOpenHabitat_LGM.png", sep = ""), p)

##################################################################
### Plot persistent climate space Where has been available since LGM 
##################################################################

p2 <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = persistentHabitat, aes(PC1, PC2), color = 'red', alpha = 0.25) +
  ggtitle("Persistent climate since LGM")

ggsave(paste(time, "_persistentHabitat.png", sep = ""), p2)

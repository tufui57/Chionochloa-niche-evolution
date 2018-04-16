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

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")
load(".//currentNicheSimilarToLGM130418.data")

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current climate data frame 
scoresLGM <- mutate(scores,  lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1,0))

scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

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

#################################################################################
### Linear regression
#################################################################################

##################################################
### Species age - Persistent occurrence ratio
##################################################

myplot <- plotAnalysis(data = persistentRatioAge, 
                       xv = "speciesAge", yv = "persistentRatio", 
                       nodeNumbercol = "nodeID", showStats = T,
                       xlabname = "Species age", 
                       ylabname = "Persistent occurrence ratio",
                       label.point = TRUE,
                       genus_name = genus_name
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\persistentRatio_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)



# Choose past data. Interglacial; "lig_30s_bio", 6k year ago; "ccmidbi_30s"
time <- "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)

# Resolution of rasters
reso = 5

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)


load(paste(".\\LGM_mainisland_worldclim",
           Worldclim, "_", reso, "km_scores.data", sep = "")
     )
load(paste(".\\Scores_", genus_tag,"_landcover_worldclim",
           Worldclim, "_", reso, "km.data", sep = ""))

#################################################################################
### Find current 1km grid cells within neighbourhood of past available climate niche
#################################################################################
source(".\\Chionochloa niche evolution\\LGM climate niche\\F_nearestNeighbourDistance.R")

# The size of neighbourhood cells = a

# How do you decide the size?
# 1. Calculte how original calimte values would change in a response to PC1 +a and PC2 + a
# Use the following script
#source(".\\Chionochloa niche evolution\\LGM climate niche\\06_ReversePCvalues_to_originalClimateVlues.R")
#
# 2. Calculate ratio of persistent climate generated from a=0.001, 0.005, 0.01, 0.025, 0.05

### NOTE; the following takes time circa. half an hour.
# The following function finds past climate cells within a x a squires whose centre is current climate
neighbours <- neighbours_within_a_squire(dat1 = newdf, dat2 = scores, a = a, coordinateNames = c("PC1", "PC2"))

save(neighbours, file = paste(".//currentNicheSimilarToLGM_",a, "_", genus_tag, "_", reso, "km.data", 
                              sep = ""))




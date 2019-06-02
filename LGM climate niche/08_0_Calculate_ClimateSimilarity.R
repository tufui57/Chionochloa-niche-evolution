
genus_name="Acaena"

# Choose past data. Interglacial; "lig_30s_bio", 6k year ago; "ccmidbi_30s"
time <- "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)

# Worldclim data for current climate used for comparison with LGM must be ver.1.
# Because ver.2 doesn't have LGM climate yet (2.10.2018)
Worldclim = 1

# The resolution of LGM climate raster must be 5km, because it shouldn't be downscaled. 
reso = 5

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)


load(paste(".\\LGM_mainisland_worldclim1_5km_scores.data", sep = "")
     )

file.exists()
load(paste(".\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = ""))

#################################################################################
### Find current 1km grid cells within neighbourhood of past available climate niche
#################################################################################
source(".\\Chionochloa niche evolution\\scripts\\LGM climate niche\\F_nearestNeighbourCells.R")

# The size of neighbourhood cells = a
a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.005))

# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)


# How do you decide the radius size?
# 1. Calculte how original calimte values would change in a response to PC1 +a and PC2 + a
# Use the following script
#source(".\\Chionochloa niche evolution\\LGM climate niche\\06_ReversePCvalues_to_originalClimateVlues.R")
#
# 2. Calculate ratio of persistent climate generated from a=0.001, 0.005, 0.01, 0.025, 0.05

for(i in a){
  ### NOTE; the following takes time circa. half an hour.
  # The following function finds past climate cells within a x a squires whose centre is current climate
  neighbours <- neighbours_within_a_squire(dat1 = newdf, dat2 = scores, a = i, coordinateNames = c("PC1", "PC2"))
  
  write.csv(neighbours, file = paste("Y://currentNicheSimilarToLGM_", i, "_5km.csv", sep = ""))
}




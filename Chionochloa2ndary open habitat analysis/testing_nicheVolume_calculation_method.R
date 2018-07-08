#########################################################################
### Test the method of niche volume calculation
#########################################################################

genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Niche filling//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Niche volume from probability
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

# Create imaginary speices occurring at all cells in NZ
prob1 <- get_occurrenceProbability_to_scores(spname[1])
ras1 <- raster_from_dataframe(prob1)
rasall <- ras1
values(rasall) <- ifelse(is.na(values(ras1)), NA, 1000)

# Create imaginary speices occurring nowhere in NZ
rasno <- ras1
values(rasno) <- ifelse(is.na(values(ras1)), NA, 0)

# Calculate niche volume
prob1 <- get_occurrenceProbability_to_scores(i)

# dismo::nicheOverlap() requires raster of probability
rassp <- raster_from_dataframe(prob1)


# niche volume should be 0 for the species occurring nowhere
tryCatch(
  nicheOverlap(rassp, rasall, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)

# niche volume should be close to the sum of probability?
tryCatch(
  nicheOverlap(rassp, rassp, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)
sum(values(rassp), na.rm = T) / 1000




# Create imaginary speices occurring nowhere in NZ
rasno <- ras1
values(rasno) <- NA

# niche volume should be 0 for the species occurring nowhere
tryCatch(
    nicheOverlap(rassp, rasno, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
    }
)

rasno <- ras1
values(rasno) <- ifelse(is.na(values(ras1)), NA, 0)

# niche volume should be 0 for the species occurring nowhere
tryCatch(
  nicheOverlap(rassp, rasno, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)

########################################################################################
### Niche volume from presence/absence (0/1) data
########################################################################################

### Original method in ecospat to calculate Schonner's D
library(ecospat)

SchoenerD <- function(scores, # PCA score data frame
                      gen1, # species name
                      scores.gen2 # PCA score to compare with the speices "gen1" 
                      ) {
  
  # Extract data of two target species
  scores.gen1 <-  scores[scores[, gen1] == 1,c("PC1", "PC2")]
  scores.clim <- scores[, c("PC1", "PC2")]
  
  # calculation of occurence density and test of niche equivalency and similarity
  z1 <- ecospat.grid.clim.dyn(scores.clim, scores.clim, scores.gen1, R = 100)
  z2 <- ecospat.grid.clim.dyn(scores.clim, scores.clim, scores.gen2, R = 100)
  
  res <- list()
  ## Schoener D
  res[[1]] <- unlist(ecospat.niche.overlap(z1, z2, cor = T))
  res[[2]] <- unlist(ecospat.niche.overlap(z1, z2, cor = F))
  # Name
  names(res) <- c(paste(gen1, "corrected"), paste(gen1, "not corrected"))
  
  return(res)
}

# virtual species occurring all cells
scores.all <- scores[, c("PC1", "PC2")]

# virtual species occurring at just 1 cell, approximation of speices occurring nowhere
scores.no <- scores[1:10, c("PC1", "PC2")]


SchoenerD(scores, spname[2], scores.all)
SchoenerD(scores, spname[2], scores.no)

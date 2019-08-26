#########################################################################
### Test the method of niche volume calculation
#########################################################################
### Test for the following;
### 1. if niche volume can be 0 or 1?
### 2. niche volume should be close to the sum of probability?


genus_name <- "Chionochloa"

library(ecospat)
library(dismo)
library(nichePlot)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Niche filling//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Niche volume from probability
####################################################################

# Create imaginary speices occurring at all cells in NZ
prob1 <- get_occurrenceProbability_to_scores(spname[1], pred)
ras1 <- raster_from_dataframe(prob1)
rasall <- ras1
values(rasall) <- ifelse(is.na(values(ras1)), NA, 1000)

# Calculate niche volume
prob1 <- get_occurrenceProbability_to_scores(spname[1], pred)

# dismo::nicheOverlap() requires raster of probability
rassp <- raster_from_dataframe(prob1)


### niche volume should be 1
tryCatch(
  nicheOverlap(rassp, rassp, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)


# Create imaginary speices occurring nowhere in NZ
rasno <- ras1
values(rasno) <- 0

# niche volume should be 0 for the species occurring nowhere
tryCatch(
  nicheOverlap(rassp, rasno, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)


### niche volume should be close to the sum of probability?
tryCatch(
  nicheOverlap(rassp, rasall, stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) {print(i)
    warning()
  }
)

sum(values(rassp), na.rm = T)




########################################################################################
### Niche volume from presence/absence (0/1) data
########################################################################################

# virtual species occurring all cells
scores.all <- scores[, c("PC1", "PC2")]

# virtual species occurring at just 1 cell, approximation of speices occurring nowhere
scores.no <- scores[1:10, c("PC1", "PC2")]

scores.sp <- scores[scores[, colnames(scores) == spname[2]] == 1, ]

### Normal species niche volume
SchoenerD_ecospat(scores, "PC1", "PC2", scores.sp, scores.all)

### niche volume should be 1
SchoenerD_ecospat(scores, "PC1", "PC2", scores.sp, scores.sp)
### niche volume should be 0
SchoenerD_ecospat(scores, "PC1", "PC2", scores.sp, scores.no)

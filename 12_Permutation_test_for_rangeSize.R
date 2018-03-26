##############################################################################
### Data preparation
##############################################################################

# genus_name <- "Acaena"
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))


randomID <- sample(nodeID, 100)


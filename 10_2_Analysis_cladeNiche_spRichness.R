###################################################
### Clade niche ~ species richness analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

# genus_name <- "Acaena"
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

## Count species richness within clades
nodeSp <- sapply(1:(length(tree$edge.length) + 1), count_spRichness, tree = tree)
names(nodeSp) <- 1:(length(tree$edge.length) + 1)


ageVolSprich <- mutate(ageVolData, nodeSpRichness = nodeSp[names(nodeSp) %in% ageVolData$node1],
                       nicheVolumePerSp = ageVolSprich$nicheVolume/ageVolSprich$nodeSpRichness)

# Compare the following two.
summary(
  lm(nicheVolume ~ speciesAge + nodeSpRichness, data = ageVolSprich)
)


summary(
  lm(nicheVolumePerSp ~ speciesAge, data = ageVolSprich)
)

summary(
  lm(nicheVolume ~ speciesAge, data = ageVolSprich)
)

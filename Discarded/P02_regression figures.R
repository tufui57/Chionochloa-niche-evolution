
genus_name <- "Acaena"
# genus_name <- "Chionochloa"

# neighbourhood cell size
a=0.045
# Resolution of grid cells
reso=5
# Worldclim version
worldclim=1

source(".//Chionochloa niche evolution//00_DataPreparation.R")

########################################################################################################
### Ratio of open habitat with persistent climate ~ species age
########################################################################################################

library(ggplot2)
source(".//functions//F_plotAnalysis_clade_niche.R")
# persistentRatioAge <- read.csv(paste("persistentRatio_age_", genus_tag, reso, "km", a, ".csv", sep = ""))

Age <- read.csv(paste("persistentRatio_age_", genus_tag, reso, "km", a, ".csv", sep = ""))
persistentOpen <- read.csv(paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))

persistentRatioAge <- merge(Age, persistentOpen, by.x = "spname", by.y = "X")

summary(
  lm(D ~ speciesAge, data = persistentRatioAge)
)

myplot <- plotAnalysis(data = persistentRatioAge, 
                       xv = "speciesAge", yv = "D", 
                       nodeNumbercol = "nodeID", showStats = T,
                       xlabname = "Species age", 
                       ylabname = "Niche overlap between species occurrences in open habitat and LGM climate",
                       label.point = TRUE,
                       genus_name = genus_name
) +
  theme(text = element_text(size=10))

ggsave(paste("Y://spAge_persistentRatio", genus_tag, ".png"), myplot, width = 100, height = 80, units = "mm")

########################################################################################################
### Data import
########################################################################################################

# Species age - niche volume
ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
# Time since divergence - niche overlap
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

########################################################################################################
### Niche overlap ~ Divergence time 
########################################################################################################

sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]

### Eliminate outlier
outlier <- which(max(sisOverlapPd$divergenceTime) == sisOverlapPd$divergenceTime)

myplot <- plotAnalysis(data = sisOverlapPd, genus_name = genus_name,
                       yv = "nicheOverlap", xv = "divergenceTime", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap between sister species", 
                       xlabname = "Divergence time",
                       label.point = TRUE
) + 
  theme(text = element_text(size=10)) +
  # Niche overlap ranges 0 - 1, so I delimited y axis but it can get rid of confidense interval from this figure.
  scale_y_continuous(limits = c(0, 1))

ggsave(paste("Y://", genus_tag, "_nicheOverlap_divergenceTime.png", sep=""), myplot, width = 100, height = 80, units = "mm")

rm(myplot)

########################################################################################################
### Niche volume ~ Species age 
########################################################################################################

myplot <- plotAnalysis(data = ageVolData, genus_name = genus_name,
                       yv = "nicheVolume", xv = "speciesAge",
                       nodeNumbercol = "nodeID", showStats = T,
                       ylabname = "Niche volume",
                       xlabname = "Taxon age",
                       label.point = TRUE
                       ) +
  theme(text = element_text(size=10)) +
  # Niche overlap ranges 0 - 1, so I delimited y axis but it can get rid of confidense interval from this figure.
  scale_y_continuous(limits = c(0, 1))

ggsave(paste("Y://", genus_tag, "_nicheVolume_age.png", sep=""), myplot, width = 100, height = 80, units = "mm")

rm(myplot)

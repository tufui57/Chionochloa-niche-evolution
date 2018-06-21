
#genus_name <- "Acaena"
genus_name <- "Chionochloa"

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
source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")
persistentRatioAge <- read.csv(paste("persistentRatio_age_", genus_tag, reso, "km", a, ".csv", sep = ""))


summary(
  lm(persistentRatio ~ speciesAge, data = persistentRatioAge)
)

myplot <- plotAnalysis(data = persistentRatioAge, 
                       xv = "speciesAge", yv = "persistentRatio", 
                       nodeNumbercol = "nodeID", showStats = T,
                       xlabname = "Species age", 
                       ylabname = "Ratio of open habitat with persistent climate",
                       label.point = TRUE,
                       genus_name = genus_name
) +
  
  theme(text = element_text(size=10)) +
  scale_y_continuous(breaks = seq(0.45, 1.1, by = 0.1), limits = c(0.45, 1.1))

ggsave(paste("Y://", genus_tag, a, reso, "km.png"), myplot, width = 100, height = 80, units = "mm")

########################################################################################################
### Data import
########################################################################################################

# Species age - niche volume
ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
# Time since divergence - niche overlap
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))
# Proportion of secondary open habitat
secondaryOpen <- read.csv(paste("Y://Acaena project//", genus_name, "_data_analyses.csv", sep = ""))

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
                       xlabname = "Time since divergence",
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
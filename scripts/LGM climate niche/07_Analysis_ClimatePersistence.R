#################################################################################
### Habitat and Climate Persistence 
#################################################################################
library(dplyr)
library(ggplot2)
source(".//functions//F_plotAnalysis_clade_niche.R")

genus_tag = "acaena"

persistentRatio <- read.csv(paste("PersistentOccurrences_", genus_tag, ".csv", sep = ""))
speciesAge <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

persistence <- merge(persistentRatio, speciesAge, by.x = "X", by.y = "spname")

##################################################
### Species age - Niche volume
##################################################

summary(
  lm(D ~ speciesAge, data = persistence)
)

myplot <- plotAnalysis(data = persistence, 
                       xv = "speciesAge", yv = "D", 
                       nodeNumbercol = "nodeID", showStats = T,
                       xlabname = "Taxon age", 
                       ylabname = "Occurrence ratio in persistent environment",
                       label.point = TRUE,
                       genus_name = genus_tag
) +
  ylim(0, 0.5)

# save
ggsave(paste("Y:\\taxonVol_taxonAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

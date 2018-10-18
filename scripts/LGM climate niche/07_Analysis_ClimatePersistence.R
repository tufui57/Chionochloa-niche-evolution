#################################################################################
### Habitat and Climate Persistence 
#################################################################################
library(dplyr)
library(ggplot2)
source(".//functions//F_plotAnalysis_clade_niche.R")

genus_tag = "acaena"

### Import species age, niche volume and prevalence in persistent environment
persistentRatio <- read.csv(paste("PersistentOccurrences_", genus_tag, ".csv", sep = ""))
persistentRatio$X <- as.character(persistentRatio$X)
speciesAge <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

colnames(persistentRatio)[1] <- "spname" 
persistence <- merge(persistentRatio, speciesAge[!is.na(speciesAge$spname),], by = "spname", all.y = T)
colnames(persistence)[colnames(persistence)=="D"] <- "persistentNiche"

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
                       ylabname = "Prevalence in persistent environment",
                       label.point = TRUE,
                       genus_name = genus_tag, 
                       cex=22
) +
  ylim(0, 0.5)

# save
ggsave(paste("Y:\\taxonVol_taxonAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 220, height = 150, units = 'mm')

rm(myplot)

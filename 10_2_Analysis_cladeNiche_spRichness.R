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


ageVolSprich <- mutate(ageVolData, nodeSpRichness = nodeSp[names(nodeSp) %in% ageVolData$nodeID])

ageVolSprich <- mutate(ageVolSprich,
                       nicheVolumePerSp = ageVolSprich$nicheVolume/ageVolSprich$nodeSpRichness)


#################################################################################
### Linear regression
#################################################################################

##################################################
### Species age - Persistent occurrence ratio
##################################################

summary(
  lm(nicheVolume ~ speciesAge + nodeSpRichness, data = ageVolSprich)
)

myplot <- plotAnalysis(data = ageVolSprich, 
                       xv = "speciesAge", yv = "nicheVolume", 
                       nodeNumbercol = "nodeID", showStats = T,
                       xlabname = "Taxon age", 
                       ylabname = "Taxon niche volume",
                       label.point = TRUE,
                       genus_name = genus_name
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\taxonVol_taxonAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)
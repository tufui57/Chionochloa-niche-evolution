###################################################
### Clade niche ~ species richness analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

genus_name <- "Acaena"
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

## Count species richness within clades
nodeSp <- sapply(1:(length(tree$edge.length) + 1), count_spRichness, tree = tree)
names(nodeSp) <- 1:(length(tree$edge.length) + 1)

# Add species richness column
ageVolSprich <- mutate(ageVolData, nodeSpRichness = nodeSp[names(nodeSp) %in% ageVolData$nodeID])

#################################################################################
### Linear regression
#################################################################################

##################################################
### Species age - Niche volume
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

#########################################################################
### Sister species pairs' divergence time ~ niche overlap
#########################################################################

# Subset sister species pairs
sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]

# Save the sister sepcies data
write.csv(sisOverlapPd, file = paste(".//NicheOverlap_sister_", genus_tag, ".csv", sep=""))

summary(
  lm(nicheOverlap ~ divergenceTime, data = sisOverlapPd)
)

# Plot
myplot <- plotAnalysis(data = sisOverlapPd, 
                       yv = "nicheOverlap", xv = "divergenceTime", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap of occurrence records", 
                       xlabname = "Time since divergence",
                       label.point = TRUE,
                       genus_name = genus_name
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\sister_divergenceTime_nicheoverlap_legend_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)
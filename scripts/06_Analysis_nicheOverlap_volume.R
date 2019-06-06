###################################################
### Clade niche ~ species richness analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

genus_name <- "Acaena"
#genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

### Import data
ageVolData <- read.csv(paste("Y://NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Y://Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

##############################################################################
### Count species richness within clades
##############################################################################

nodeSp <- sapply(1:(length(tree$edge.length) + 1), count_spRichness, tree = tree)
names(nodeSp) <- 1:(length(tree$edge.length) + 1)

# Add species richness column
ageVolSprich <- mutate(ageVolData, nodeSpRichness = nodeSp[names(nodeSp) %in% ageVolData$nodeID])

#################################################################################
### Linear regression
#################################################################################

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


#########################################################################
### Climatic niche similarity between sister vs. random species pairs
#########################################################################

# If the "cladePairData_5km" data was absent, run the following script again.
if(file.exists(paste(".//Random_sp_pair_nicheOverlap_", genus_name, ".csv", sep=""))){
  ran <- read.csv(paste(".//Random_sp_pair_nicheOverlap_", genus_name, ".csv", sep=""))
}else{
  source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\scripts\\04_Random_species_nicheOverlap.R")

}

t.test(sisOverlapPd[, 4], ran[,4])

png(paste(".//NicheOverlap_sister_random_species_", genus_name, ".png", sep=""))
boxplot(sisOverlapPd[, 4], ran[,4],
        main = genus_name,
        ylab = "Climatic niche overlap",
        names = c("Sister species", "Random species")
)

dev.off()

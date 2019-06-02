###################################################
### Clade niche analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

genus_name <- "Acaena" # "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

ageVolData <- read.csv(paste("Y://NicheVolume_5km_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Y://Nicheovrlap_PD_5km_", genus_tag, ".csv", sep = ""))

# Get species name tag
ageVolData2 <- ageVolData[!is.na(ageVolData$spname), ]

ageVolData2 <- cbind(ageVolData2, as.data.frame(makeTag_separate(ageVolData$spname, genus_name, separate = "_")[[2]])
      )
colnames(ageVolData2)[ncol(ageVolData2)] <- "tag"

#########################################################################
### Species niche volume ~ species ages 
#########################################################################
# LM test
summary(lm(ageVolData2$nicheVolume ~ ageVolData2$speciesAge))

myplot <- plotAnalysis(data = ageVolData2, genus_name = genus_name,
                       yv = "nicheVolume", xv = "speciesAge", 
                       nodeNumber = "tag", showStats = F,
                       ylabname = "Species niche volume",
                       xlabname = "Species age",
                       label.point = TRUE
)+
  theme(text = element_text(size=10),
        axis.text = element_text(size=10))

# save
ggsave(paste("Y:\\Species_age_nicheVolume5km_", genus_name, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)

summary(lm(ageVolData2$nicheVolume ~ ageVolData2$speciesAge))

####################################################################################################
### Sister species pairs' divergence time ~ niche overlap between sister species
####################################################################################################

### Node numbers of sister species pairs

sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]
# LM test
summary(lm(sisOverlapPd$nicheOverlap ~ sisOverlapPd$divergenceTime))

myplot <- plotAnalysis(data = sisOverlapPd,
                       genus_name = genus_name,
                       yv = "nicheOverlap", xv = "divergenceTime", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap between sister species", 
                       xlabname = "Time since divergence",
                       label.point = TRUE
) +
  ylim(0, 1) +
  theme(text = element_text(size=10),
        axis.text = element_text(size=10))

# save
ggsave(paste("Y:\\sister_divergenceTime_nicheoverlap_5km_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)

####################################################################################################
### Species prevalences in persistent environments ~ species age
####################################################################################################

pers <- read.csv(paste(".//PersistentOccurrences_", genus_name, ".csv", sep=""))

sppers <- merge(ageVolData2, pers, by.x = "spname", by.y="X")

# LM test
summary(lm(sppers$D ~ sppers$speciesAge))

# Plot
myplot <- plotAnalysis(data = sppers,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "D", 
                       nodeNumbercol = "tag", showStats = T,
                       ylabname = "Species prevalences in persistent environments", 
                       xlabname = "Sepcies age",
                       label.point = TRUE
)+
  theme(text = element_text(size=10),
        axis.text = element_text(size=10))

# Turn off clipping plot areas
gt <- ggplot_gtable(ggplot_build(myplot))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid::grid.draw(gt)


# save
ggsave(paste("Y:\\persistentOccbySchoennersD_spAge5km_", genus_tag, ".png", sep = ""), plot = gt,
       width = 100, height = 80, units = 'mm')

rm(myplot)

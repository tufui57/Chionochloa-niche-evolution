###################################################
### Clade niche analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

genus_name <- "Acaena"
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

#########################################################################
### Plots
#########################################################################

ageVolData <- read.csv(paste("Y://NicheVolume_age_", genus_tag, ".csv", sep = ""))


#########################################################################
### Clade niche volume ~ clade ages 
#########################################################################

### Eliminate outlier
outlier <- which(max(ageVolData$speciesAge) == ageVolData$speciesAge)

myplot <- plotAnalysis(data = ageVolData[-outlier,], genus_name = genus_name,
                       xv = "nicheVolume", yv = "speciesAge", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche volume of occurrence records",
                       ylabname = "Clade age",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\clade_age_nicheVolume_legend_", genus_name, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


### Leave outlier
myplot <- plotAnalysis(data = ageVolData, genus_name = genus_name,
                       xv = "nicheVolume", yv = "speciesAge", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche volume of occurrence records",
                       ylabname = "Clade age",
                       label.point = TRUE
) +
  ylim(0, 1)
# save
ggsave(paste("Y:\\clade_age_nicheVolume_outlier_", genus_name, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

##############################################################################################
### Clade niche volume ~ diversification rate (clade age / number of speices within clade)
##############################################################################################




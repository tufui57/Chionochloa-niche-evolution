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

spdat <- ageVolData[!is.na(ageVolData$spname),]


#########################################################################
### Clade niche volume ~ clade ages 
#########################################################################

# ### Eliminate outlier
# outlier <- which(max(spdat$speciesAge) == spdat$speciesAge)
# 
# myplot <- plotAnalysis(data = spdat[-outlier,], genus_name = genus_name,
#                        yv = "nicheVolume", xv = "speciesAge", 
#                        nodeNumber = "nodeID", showStats = T,
#                        ylabname = "Species niche volume",
#                        xlabname = "Species age",
#                        label.point = TRUE
# ) 
# 
# # save
# ggsave(paste("Y:\\Species_age_nicheVolume_", genus_name, ".png", sep = ""), plot = myplot,
#        width = 300, height = 210, units = 'mm')
# 
# rm(myplot)


### Leave outlier
myplot <- plotAnalysis(data = spdat, genus_name = genus_name,
                       yv = "nicheVolume", xv = "speciesAge", 
                       nodeNumber = "nodeID", showStats = T,
                       ylabname = "Species niche volume",
                       xlabname = "Species age",
                       label.point = TRUE
)

# save
ggsave(paste("Y:\\Species_age_nicheVolume_outlier_", genus_name, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

summary(lm(spdat$nicheVolume ~ spdat$speciesAge))

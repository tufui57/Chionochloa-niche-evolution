########################################################
### Regression figures for ratio of 2ndary open habitat 
########################################################

###################################################################################
### Reproducible data set for analyses (on construction at 3.July.2018) 
###################################################################################

genus_name = "Acaena"
genus_tag = "acaena"

# # Import data
# if(
#   file.exists(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
# ){
#   d <- read.csv(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
# }else{
#   source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_table_of_landcoverHistory_for_analyses.R")
#   source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_2_calculate_indices_for_analyses.R")
#   d <- read.csv(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
# }
# 
# library(dplyr)
# source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")
# d$tag <- makeTag_separate(d$spname, genus_name, "_")
# d$tag <- d$tag$tag %>% toupper
# 
library(ggplot2)
source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")

#################################################################################
### Use original data set (but unreproducible) for regression figures
#################################################################################
dat <- read.csv("Y://Acaena project//Acaena_elevation.csv")

summary(glm(proportionSecondaryHabitat ~ log10.current_range + 
              mean_elevation + X10km_average_abailability + Preference_open_habitat +
              niche_volume + shoenners_D + 
              type + unlist.med1. + unlist.med2.,
            data = dat)
)

########################################################
###  Proportion of Secondary Open Habitat - current range size
########################################################

myplot <- plotAnalysis(dat, genus_name = genus_name,
                       xv = "log10.current_range", yv = "proportionSecondaryHabitat", showStats = T,
                       xlabname = "log10(Current range size)", ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T
                       )
  

# save
ggsave(paste("Y:\\log10CurrentRangeSize_proportionSecondary.png", sep = ""), plot = myplot,
       width = 200, height = 140, units = 'mm')

rm(myplot)


#########################################################################
### Proportion of Secondary Open Habitat ~ niche volume
#########################################################################

myplot <- plotAnalysis(data = dat, genus_name = genus_name,
                       xv = "niche_volume", yv = "proportionSecondaryHabitat",  showStats = T,
                       xlabname = "Species niche volume", ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T
                       )
# Save
ggsave(paste("Y:\\proportionSecondary_NicheVolume.png", sep = ""), plot = myplot,
       width = 200, height = 140, units = 'mm')

rm(myplot)


#############################################################################################
###   Proportion of secondary open habitat - availability of secondary open habitat   ###
#############################################################################################

### Local(10km) availability

myplot <- plotAnalysis(dat, genus_name,
                       xv = "X10km_average_abailability", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Availability of secondary open habitat within 10 km neighbourhood", ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T
                       )

# save
ggsave(paste("Y:\\ProportionSecondary_10kmAvailability.png", sep = ""), plot = myplot,
       width = 200, height = 140, units = 'mm')

rm(myplot)


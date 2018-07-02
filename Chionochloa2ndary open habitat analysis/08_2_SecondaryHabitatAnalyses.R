########################################################
### Regression figures for ratio of 2ndary open habitat 
########################################################

genus_name = "Acaena"
genus_tag = "acaena"

# Import data
if(
  file.exists(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
){
  d <- read.csv(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
}else{
  source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_table_of_landcoverHistory_for_analyses.R")
  source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_2_calculate_indices_for_analyses.R")
  d <- read.csv(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))
}

library(dplyr)
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")
d$tag <- makeTag_separate(d$spname, genus_name, "_")
d$tag <- d$tag$tag %>% toupper

library(ggplot2)
source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")

#########################################################################
### Proportion of Secondary Open Habitat - Preference for Open Habitat
#########################################################################

myplot <- plotAnalysis(data = d,
                       genus_name = genus_name,
                       xv = "PreferenceOpen", yv = "proportionSecondaryHabitat", showStats = T,
                       xlabname = expression("closed" %<-% "Preference for open habitat" %->% "open"),
                       ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T)

myplot2 <- myplot + 
  xlim(0, 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed")

# save
ggsave(paste("Y:\\preferenceOpen_proportionSecondary_nolegend.png", sep = ""), plot = myplot2,
       width = 300, height = 210, units = 'mm')

rm(myplot)

########################################################
###  Proportion of Secondary Open Habitat - current range size
########################################################

myplot <- plotAnalysis(d, genus_name = genus_name,
                       xv = "log10.total", yv = "proportionSecondaryHabitat", showStats = T,
                       xlabname = "log10(Current range size)", ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T
                       )

# save
ggsave(paste("Y:\\log10CurrentRangeSize_proportionSecondary.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


#########################################################################
### Proportion of Secondary Open Habitat ~ niche volume
#########################################################################

myplot <- plotAnalysis(data = d, genus_name = genus_name,
                       xv = "proportionSecondaryHabitat", yv = "nicheVolume", showStats = T,
                       xlabname = "Proportion of secondary open habitat", ylabname = "Species niche volume",
                       nodeNumbercol = "tag", label.point = T
                       )
# Save
ggsave(paste("Y:\\proportionSecondary_NicheVolume.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


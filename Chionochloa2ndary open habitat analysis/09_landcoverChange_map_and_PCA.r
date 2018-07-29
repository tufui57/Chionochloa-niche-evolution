##############################################################################################
###     Draw maps for each sp coloured by land cover change     ###
##############################################################################################
library(dplyr)
genus_name <- "Chionochloa"

source(".\\functions\\F_plot_map_and_PCA.r")
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//11_Data_preparation_for_drawing_EnvSpace_and_Maps.R")

# PCA scores
load(".//scores_chion.data")
# Species occurrences
dat <- read.csv(paste("Y://2nd chapter_phylogentic niche conservation//meta data//", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv", sep=""))
dat <- dat[is.na(dat$landCoverChange) == F, ]

spname <-  grepl(paste("^", genus_name, sep=""), colnames(dat)) %>% colnames(dat)[.]
  
###############################################################
## Point map for landcover change of sp 
###############################################################

## Plot map for all species of target genus
dat$allsp <- ifelse(rowSums(dat[, spname]) > 0, 1, 0)

map_plot_colourByLandcover("allsp", dat)
map_plot_monoColour("allsp", dat)

## Plot map for a species
dat2 <- lapply(spname, function(i){
  dat[dat[,i] == 1,]
  }
  )

maps <- lapply(1:length(spname), function(i){
  map_plot_monoColour(spname[i], dat2[[i]])
  })


###############################################################
## Plot niche space for landcover change
###############################################################

## Plot niche space for all species of target genus
scores$allsp <- ifelse(rowSums(scores[, spname]) > 0, 1, 0)

niche_plot_colourByLandcover("allsp", scores)
niche_plot_monoColour("allsp", scores)

## Plot niche space for all species of target genus
scores2 <- lapply(spname, function(i){
  scores[scores[,i] == 1,]
}
)

pcas <- lapply(1:length(spname), function(i){
  map_plot_monoColour(spname[i], scores2[[i]])
})


###############################################################
## Plot niche space of two genera in one panel
###############################################################

genus_name <- "Chionochloa"
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
## Plot niche space for all species of target genus
scores$allsp <- ifelse(rowSums(scores[, spname]) > 0, 1, 0)
chion <- niche_plot_monoColour("allsp", scores)


genus_name <- "Acaena"
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
## Plot niche space for all species of target genus
scores$allsp <- ifelse(rowSums(scores[, spname]) > 0, 1, 0)
aca <- niche_plot_monoColour("allsp", scores)

# Plot the two figures in a panel
png("Y://monocolour_niche_Acaena_Chion.png", width = 1300, height = 600)
# Plot in multiple panels
grid.arrange(aca, chion, nrow = 1)
dev.off()

###############################################################
## Plot maps of two genera in one panel
###############################################################

genus_name <- "Chionochloa"
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
## Plot niche space for all species of target genus
dat$allsp <- ifelse(rowSums(dat[, spname]) > 0, 1, 0)
chion_m <- map_plot_monoColour("allsp", dat)


genus_name <- "Acaena"
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
## Plot niche space for all species of target genus
dat$allsp <- ifelse(rowSums(dat[, spname]) > 0, 1, 0)
aca_m <- map_plot_monoColour("allsp", dat)

# Plot the two figures in a panel
png("Y://monocolour_map_Acaena_Chion.png", width = 1000, height = 800)
# Plot in multiple panels
grid.arrange(aca_m, chion_m, nrow = 1)
dev.off()


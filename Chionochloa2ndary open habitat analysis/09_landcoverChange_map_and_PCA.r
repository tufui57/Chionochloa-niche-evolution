##############################################################################################
###     Draw maps for each sp coloured by land cover change     ###
##############################################################################################

genus_name <- "Chionochloa"

source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")

###############################################################
## Point map for landcover change of sp 
###############################################################

## Plot map for all species of target genus
chdata$allsp <- ifelse(rowSums(chdata[, spname]) > 0, 1, 0)

map_plot_colourByLandcover("allsp", chdata)
map_plot_monoColour("allsp", chdata)

## Plot map for a species
chdata2 <- lapply(spname, function(i){
  chdata[chdata[,i] == 1,]
  }
  )

maps <- lapply(1:length(spname), function(i){
  map_plot_monoColour(spname[i], chdata2[[i]])
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
chdata$allsp <- ifelse(rowSums(chdata[, spname]) > 0, 1, 0)
chion_m <- map_plot_monoColour("allsp", chdata)


genus_name <- "Acaena"
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
## Plot niche space for all species of target genus
chdata$allsp <- ifelse(rowSums(chdata[, spname]) > 0, 1, 0)
aca_m <- map_plot_monoColour("allsp", chdata)

# Plot the two figures in a panel
png("Y://monocolour_map_Acaena_Chion.png", width = 1000, height = 800)
# Plot in multiple panels
grid.arrange(aca_m, chion_m, nrow = 1)
dev.off()



####################################################################
### Niche volume of potential niche
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))

probAll <- get_BIOMOD_probability_by_nodeID(sispairs[1,1])
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1000)

# Calculate niche volume
vol <- list()

for(i in 1:length(spname)){
  
  prob1 <- get_BIOMOD_probability_by_nodeID(i)
  
  ### Use dismo::nicheOverlap
  tryCatch(
  vol[[i]] <- nicheOverlap(prob1[[1]], probAll[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE),
  error = function(e) print(i)
  )
}


# Acaena
if(genus_name == "Acaena"){
  volume <- unlist(vol)
}
# Chionochloa 
if(genus_name == "Chionochloa"){
  volume <- unlist(vol)[-length(unlist(vol))]
}
vols <- cbind(actualvol, volume)
colnames(vols)[colnames(vols) == "volume"] <- "potentialNicheVolume"


vols$actualVolume <- actualvol$ecospat.corrected.D

# Save the data
write.csv(vols, paste("potential_niche_ovrlap_volume", genus_tag, ".csv", sep = ""))


#########################################################################
### Niche volume of predictions ~ species age
#########################################################################

myplot <- plotAnalysis(data = vols,
                       yv = "volume", xv = "", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       ylabname = "Species age", xlabname = "Niche volume of model prediction"
)

# save
ggsave(paste("Y:\\potential_nicheVolume_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

#########################################################################
### Niche filling rate
#########################################################################

mean(vols$volume/vols$volume)


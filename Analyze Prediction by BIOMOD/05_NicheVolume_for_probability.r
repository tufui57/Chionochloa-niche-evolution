
genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Potential niche volume
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))

# Create imaginary speices occurring at all cells in NZ
probAll <- pred[[1]]
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1000)

# Calculate niche volume
vold <- list()
voli <- list()

for(i in spname){
  
  # Get probability of the species
  prob1 <- pred[names(pred) == gsub("_",".", i)]
  
  ### Use dismo::nicheOverlap
  tryCatch(
  {
    vold[[i]] <- nicheOverlap(prob1[[1]], probAll[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE)
    voli[[i]] <- nicheOverlap(prob1[[1]], probAll[[1]], stat = 'I', mask = TRUE, checkNegatives = TRUE)
  }
  ,
  error = function(e) print(i)
  )
}


### Get vector of potential niche values
volumed <- as.numeric(as.character(vold))
volumei <- as.numeric(as.character(voli))

vols <- merge(actualvol, data.frame(cbind(spname, volumed, volumei)), by ="spname")

colnames(vols)[colnames(vols) == "volumed"] <- "potentialNicheVolume.D"
colnames(vols)[colnames(vols) == "volumei"] <- "potentialNicheVolume.I"

# Save the data
write.csv(vols, paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))


#########################################################################
### Niche volume of predictions ~ species age
#########################################################################

vols <- read.csv(paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))

myplot <- plotAnalysis(data = vols,
                       yv = "potentialNicheVolume.I", xv = "", 
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

# Mean of niche filling rate
(vols$ecospat.corrected.D / as.numeric(as.character(vols$potentialNicheVolume.D))) %>% mean

(vols$ecospat.corrected.I / as.numeric(as.character(vols$potentialNicheVolume.I))) %>% mean


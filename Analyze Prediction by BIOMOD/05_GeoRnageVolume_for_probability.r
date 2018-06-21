######################################################################################################
### Calculate geographical potential range overlap between sister sepcies
######################################################################################################

genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Potential range volume
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

# Create imaginary speices occurring at all cells in NZ
probAll <- pred[1]
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


### Get vector of potential range values
volumed <- as.numeric(as.character(vold))
volumei <- as.numeric(as.character(voli))

vols <- merge(actualvol, data.frame(cbind(spname, volumed, volumei)), by ="spname")

colnames(vols)[colnames(vols) == "volumed"] <- "potentialRangeVolume.D"
colnames(vols)[colnames(vols) == "volumei"] <- "potentialRnageVolume.I"

# Save the data
write.csv(vols, paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))

#########################################################################
### Potential range ~ species age
#########################################################################

vols <- read.csv(paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))

myplot <- plotAnalysis(data = vols,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "potentialNicheVolume.I", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Species age", ylabname = "Potential niche volume"
) +
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\predNicheVolume_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)

############################################################################################################
##### Compare potential range and actual niche volume
############################################################################################################

myplot <- plotAnalysis(data = vols,
                       xv = "ecospat.corrected.I", yv = "potentialNicheVolume.I", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Actual niche volume", ylabname = "Potential niche volume"
)+
  theme(text = element_text(size=10))


# save
ggsave(paste("Y:\\nicheVolume_potential_actual_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)

#########################################################################
### Niche filling rate
#########################################################################

# Mean of niche filling rate
(vols$ecospat.corrected.D / as.numeric(as.character(vols$potentialNicheVolume.D))) %>% mean

(vols$ecospat.corrected.I / as.numeric(as.character(vols$potentialNicheVolume.I))) %>% mean


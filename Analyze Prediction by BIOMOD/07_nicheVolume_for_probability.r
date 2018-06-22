######################################################################################################
### Calculate geographical potential range overlap between sister sepcies
######################################################################################################

genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Analyze Prediction by BIOMOD//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Potential range volume
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

# Create imaginary speices occurring at all cells in NZ
prob1 <- get_occurrenceProbability_to_scores(i)
ras1 <- raster_from_dataframe(prob1)
rasall <- ras1
values(rasall) <- ifelse(is.na(values(ras1)), NA, 1000)

# Calculate niche volume
vold <- list()
voli <- list()

for(i in spname){
  
  prob1 <- get_occurrenceProbability_to_scores(i)
  
  # dismo::nicheOverlap() requires raster of probability
  rassp <- raster_from_dataframe(prob1)
  
  tryCatch(
  {
    vold[[i]] <- nicheOverlap(rassp, rasall, stat = 'D', mask = TRUE, checkNegatives = TRUE)
    voli[[i]] <- nicheOverlap(rassp, rasall, stat = 'I', mask = TRUE, checkNegatives = TRUE)
  }
  ,
  error = function(e) print(i)
  )
}


### Get vector of potential range values
volumed <- as.numeric(as.character(vold))
volumei <- as.numeric(as.character(voli))

vols <- merge(actualvol, data.frame(cbind(spname, volumed, volumei)), by ="spname")

colnames(vols)[colnames(vols) == "volumed"] <- "potentialNicheVolume.D"
colnames(vols)[colnames(vols) == "volumei"] <- "potentialNicheVolume.I"

# Save the data
write.csv(vols, paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))

#########################################################################
### Potential range ~ species age
#########################################################################

vols <- read.csv(paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))

myplot <- plotAnalysis(data = vols,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "potentialNicheVolume.D", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Species age", ylabname = "Potential niche volume"
) +
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\potentialNicheVolume_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)

############################################################################################################
##### Compare potential range and actual niche volume
############################################################################################################

myplot <- plotAnalysis(data = vols,
                       xv = "nicheVolume", yv = "potentialNicheVolume.D", 
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
(vols$nicheVolume / as.numeric(as.character(vols$potentialNicheVolume.D))) %>% mean


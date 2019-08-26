###########################################################################################################################
### Compare proportion of current climate with persistent climate that is calculated with different radius
###########################################################################################################################

# The following data preparation script loads Chionochloa's data.
source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")


#################################################################################
### Proportion of current climate with persistent climate
#################################################################################

# Number of 1 km cells in land areas of currnet NZ 
n.nz <- nrow(scores)

# Radius size is set in "08_1_Assess_radius_for_PersitentClimate_DataPreparation.R"
# a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.005))

# Add cell ID to current PCA scores
scores$cellID <- 1:nrow(scores)

for(i in a){
  n.persistent <- nrow(scores[ (scores[, paste("lgm", i, sep="")] == 1) , ])
  
  print(paste("Radius =", i , "Ratio of current areas with persistent climate"))
  print(n.persistent/n.nz)
}

#################################################################################
### Proportion of current climate with persistent climate
#################################################################################

# Persistent occurrence ratio = species occurrences in primary open habitat with persistent climate / occurrences in open habitats
persistent_occurrence_ratio <- function(speciesnumber,
                                        scores,
                                        x # neighbourhood radius
){
  spOpenOcc <- scores[scores[, spname[speciesnumber]] == 1, ] %>% filter(landCoverChange == "NF-nonF" | landCoverChange == "nonF-nonF" )
  persistentOcc <- spOpenOcc[(spOpenOcc[, paste("lgm", x, sep="")] == 1), ]
  ratio <- nrow(persistentOcc) / nrow(spOpenOcc)
  return(ratio)
}

persistentdata <- lapply(a, 
       function(x){

  persistentRatio <- sapply(1:length(spname), persistent_occurrence_ratio, scores, x = x)
  persistentRatioData <- data.frame(cbind(spname, persistentRatio))
  
  return(persistentRatioData)
       }
)

names(persistentdata) <- paste("Radius =", a)

# Name tag
tag <- makeTag_separate(spname, genus_name, "_")

###########################################################################################################################
### Correlation between proportion of species occurrences within open habitat with persistent climate and Schonner's D
###########################################################################################################################
schoD <- read.csv(paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))

persistentdata2 <- lapply(persistentdata, function(x){
  x[, "persistentRatio"] <- x[, "persistentRatio"] %>% as.character %>% as.numeric
  merge(x, schoD, by.x="spname", by.y="X")
  }
)

lapply(persistentdata2, function(d){
  m <- lm(d$D ~ d$persistentRatio)
  summary(m)
}
)









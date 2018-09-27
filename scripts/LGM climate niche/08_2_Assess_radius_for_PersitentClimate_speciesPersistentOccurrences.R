###########################################################################################################################
### Compare proportion of current climate with persistent climate that is calculated with different radius
###########################################################################################################################

# The following data preparation script loads Chionochloa's data.
source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")


#################################################################################
### Proportion of current climate with persistent climate
#################################################################################

# Number of 1 km cells in land areas of currnet NZ 
n.nz <- nrow(scores)

# Radius size
a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.01))

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

persistentdata <- lapply(c(0.01, 0.02, 0.05), 
       function(x){

  persistentRatio <- sapply(1:length(spname), persistent_occurrence_ratio, scores, x = x)
  persistentRatioData <- data.frame(cbind(spname, persistentRatio))
  
  return(persistentRatioData)
       }
)

names(persistentdata) <- paste("Radius =", c(0.01, 0.02, 0.05))

# Name tag
tag <- makeTag_separate(spname, genus_name, "_")

###########################################################################################################################
### Compare proportion of current climate with persistent climate that is calculated with different radius
###########################################################################################################################
### Plot

persistentRatio <- lapply(persistentdata, function(d){
  d[, "persistentRatio"] %>% as.character %>% as.numeric
  }
  )

png("peristent occurrence ratios on 3 radius.png", width = 1000, height = 800)
par(mfrow=c(3,1),oma = c(0, 0, 2, 0), cex=1)
plot(persistentRatio[[1]], persistentRatio[[3]],
     xlab = names(persistentRatio)[1], ylab = names(persistentRatio)[3]
     )
text(persistentRatio[[1]], persistentRatio[[3]], labels = tag$tag, cex = 1.2, pos = 1)


plot(persistentRatio[[1]], persistentRatio[[2]],
     xlab = names(persistentRatio)[1], ylab = names(persistentRatio)[2]
)
text(persistentRatio[[1]], persistentRatio[[2]], labels = tag$tag, cex = 1.2, pos = 1)


plot(persistentRatio[[2]], persistentRatio[[3]],
     xlab = names(persistentRatio)[2], ylab = names(persistentRatio)[3]
)
text(persistentRatio[[2]], persistentRatio[[3]], labels = tag$tag, cex = 1.2, pos = 1)

mtext("Proportion of species occurrences within open area with persistent climate", outer = TRUE, cex = 1.5)

dev.off()










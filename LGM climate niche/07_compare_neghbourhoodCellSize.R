genus_name = "Chionochloa"

# Create genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

reso = 5
worldclim = 1


# cell size should be different depending on the resolution of climate rasters

if(reso==1){
  cellsize <- c(0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1)
}
if(reso==5){
  cellsize <- seq(0.01, 0.1, by=0.005)
}

res <- list()
for(a in cellsize){
  
  cat(paste("cell size =", a))
  
  if(file.exists(paste(".//temporary//persistentRatio_age_",genus_tag, reso, "km", a, ".csv", sep="")) == FALSE
  ){
    source(".//Chionochloa niche evolution//LGM climate niche//02_PCAdataPreparation.R")
    source(".//Chionochloa niche evolution//LGM climate niche//04_PastClimate_analysis.R")
  }else{
    
    library(dplyr)
    
    # LGM climate
    load(paste(".\\LGM_mainisland_worldclim",
               worldclim, "_", reso, "km_scores.data", sep = "")
    )
    # Current climate
    load(paste(".\\Scores_", genus_tag,"_landcover_worldclim",
               worldclim, "_", reso, "km.data", sep = ""))
    
    # Persistent climate
    load(paste(".//temporary//currentNicheSimilarToLGM_", a,"_", genus_tag, "_", reso, "km.data", sep = ""))
    
    # Add cell ID to current PCA scores
    scores$cellID <- 1:nrow(scores)
    
    # Add availability of climate niche in LGM to current PCA scores
    # neighbours$dat2cellID has cell ID of the "scores" object, thus, rows of "scores" whose cell ID are in "neighbours$dat2cellID"
    
    scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))
    
    # Persistent climate; climate that is shared between LGM and the present. 
    persistentClimate <- scoresLGM[scoresLGM$lgm == 1, ]
    
    # Persistent climate of open habitat; persistent climate in primary open habitat
    persistentHabitatPersistentClimate <- persistentClimate[persistentClimate[,"landCoverChange"] == "nonF-nonF", ] 
    
    # Primary open habtat
    primary <- scoresLGM[scoresLGM[, "landCoverChange"] == "nonF-nonF", ]
  }
  
  # Number of grid cells in land areas of currnet NZ 
  n.nz <- nrow(scores)
  # Number of grid cells in land areas of Zealandia in LGM
  n.lgm <- nrow(newdf)
  # Number of grid cells in primary open habitat
  n.primary <- nrow(primary)
  # Number of grid cells in primary open habitat
  n.secondary <- nrow(scoresLGM[scoresLGM[, "landCoverChange"] == "NF-nonF", ])
  
  # Number of grid cells in areas with persistent climate in current NZ
  n.persistent <- nrow(persistentClimate)
  # Number of grid cells in persistent habitat (primary open) with persitent climate
  n.persistentHabitatPersistentClimate <- nrow(persistentHabitatPersistentClimate)
  
  ### Calculate Ratio
  
  print("Ratio of current areas with persistent climate")
  print(n.persistent/n.nz)
  
  res[[length(res) + 1]] <- (n.persistent/n.nz) 
  
  print("Ratio of persistent habitat with persistent climate among primary open habitat")
  print(n.persistentHabitatPersistentClimate / n.primary)
  
}

regression <- cbind(cellsize, unlist(res))
png("cellSizeComparison.png")
plot(regression, xlab="LGM neighbourhood cell size", ylab="Ratio of persistent climate")
dev.off()

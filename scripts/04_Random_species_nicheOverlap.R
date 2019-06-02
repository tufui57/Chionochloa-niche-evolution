###################################################
### Niche overlap between random species pairs 
###################################################

##############################################################################
### Data preparation
##############################################################################

# genus_name <-"Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

# Load PCA scores
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

# Load PCA data and clade paired PCA data
load(paste(".//cladePairData_5km_", genus_tag, ".data", sep = ""))

# Get the number of species in the phylogeny tree
spname <- tree$tip.label
if(genus_name == "Acaena"){removesp <- c(12, 13, 17)}else{
  removesp <- c(2,16)
}

#########################################################################
### Random niche overlap
#########################################################################

scho <- list()
ransplist <- list()
for(i in 1:nrow(sispairs)){
  
  # Get random pairs
  ransp <- sample(1:length(spname)[-removesp], 2)
  
  # Check if the random pair isn't sister pairs. If so, change the random pair.
  if(sum(sispairs == ransp) == 2){
    ransp <- sample(1:length(spname)[-removesp], 2)
  }
  
  ransis <- sum(sispairs == ransp) == 2
  print(paste("The random pair is sister;", ransis))
  print(ransp)
  ransplist[[i]] <- ransp
  
  scho[[i]] <- SchoenerD_ecospat(scores, "PC1", "PC2",
                                 cladedata[[ransp[1]]][[1]],  cladedata[[ransp[2]]][[1]] # Data of target clade pair
  )
  
}

scholist <- lapply(1:length(scho), function(i){
  # Convert list to dataframe
  x <- data.frame(scho[[i]])
  
  # Add colnames
  colnames(x) <- c("ecospat.corrected.D", "ecospat.uncorrected.D")
  
  # Get Shoenner's D. I don't use I.
  return(x["D",])
  
}
)

# Add column in schonner's D dataframe showing clade numbers
scholist <- do.call(rbind, scholist)
nodeNo <- sapply(cladedata, "[[", 5) %>% strsplit(., "\ ")

scholist <- cbind(do.call(rbind, ransplist), scholist)

write.csv(scholist, paste(".//Random_sp_pair_nicheOverlap_", genus_name, ".csv", sep = ""))


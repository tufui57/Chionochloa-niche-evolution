###################################################
### Clade niche overlap/volume
###################################################

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

# Load PCA data and clade paired PCA data
if(genus_name == "Chionochloa"){
  
  load(".//Scores_chion_24sep.data")
  load(".//cladePairData_chion24sep.data")
}

if(genus_name == "Acaena"){  

  load(".//Scores_acaena.data")
  load(".//cladePairData_acaena.data")
  
}

###################################################
# Calculate niche overlap between sister clades
###################################################

scho <- list()

for(i in 1:length(cladedata)){

  scho[[i]] <- SchoenerD_ecospat(scores, "PC1", "PC2",
                                 cladedata[[i]][[1]],  cladedata[[i]][[3]] # Data of target clade pair
                                 )

}

sholist <- list()

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

scholist$node1 <- as.numeric(do.call(rbind, lapply(nodeNo, "[[",1)))
scholist$node2 <- as.numeric(do.call(rbind, lapply(nodeNo, "[[",2)))

write.csv(scholist, paste(".//clade_schoennerD_", genus_tag, "24sep.csv", sep = ""))


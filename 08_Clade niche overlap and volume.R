###################################################
### Clade niche overlap/volume
###################################################

genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")


###################################################
# Calculate niche overlap between sister clades
###################################################

scho <- list()

for(i in 1:length(cladedata)){

  scho[[i]] <- SchoenerD_ecospat(scores, "PC1", "PC2",
                                 cladedata[[i]][[1]],  cladedata[[i]][[3]] # Data of target clade pair
                                 )

}


scholist <- lapply(1:length(scho), function(i){
  # Convert list to dataframe
  x <- data.frame(scho[[i]])

  # Add colnames
  colnames(x) <- c("ecospat.corrected", "ecospat.uncorrected")

  # Get Shoenner's D. I don't use I.
  return(x["D",])

  }
)

# Add column in schonner's D dataframe showing clade numbers
scholist <- do.call(rbind, scholist)
nodeNo <- sapply(cladedata, "[[", 5) %>% strsplit(., "\ ")

scholist$node1 <- as.numeric(do.call(rbind, lapply(nodeNo, "[[",1)))
scholist$node2 <- as.numeric(do.call(rbind, lapply(nodeNo, "[[",2)))

write.csv(scholist, paste(".//clade_schoennerD_", genus_tag, ".csv", sep = ""))

###################################################
### Clade niche volume
###################################################

nichevol <- list()
for(i in 1:length(cladedata)){
  
  nichevol[[i]] <- SchoenerD_ecospat(
    
    # I calculate just one clade of each sister clade pair.
    data1 = cladedata[[i]][[1]], # data of nodes including internal nodes
    background = scores, data2 = scores, "PC1", "PC2"
  )
  
}

nichevoldata <- SchonnerDdataframeFormat(nichevol)

write.csv(nichevoldata, paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))



########################################################################################
### Niche overlap between each of sister species and its sister nodes (aunt of target pair)
########################################################################################

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

###################################################
# Calculate niche overlap between sister clades
###################################################

scho <- list()

for(i in sispairs[, 1]){
  
  i <- (names(cladedata) == i) %>% which
  ansSisScore2 <- get_climatedata_of_auntNode(i)
  
  ## Find parent node of target sister species pair
  ancestor <- tree$edge[which(i == tree$edge[, 2])]
  ancestorSisNode <- distance2[distance2[, "node"] == ancestor, "sisterNode"]
  
  a <- SchoenerD_ecospat(scores, "PC1", "PC2",
                         cladedata[[i]][[1]], # sister species 1
                         ansSisScore2 # Data of aunt of target clade pair
  )
  
  b <- SchoenerD_ecospat(scores, "PC1", "PC2",
                         cladedata[[i]][[3]],   # sister species 2
                         ansSisScore2 # Data of aunt of target clade pair
  )
  
  scho[[i]] <- unlist(a) %>% data.frame %>% t
  colnames(scho[[i]]) <- c("corrected.D", "corrected.I", "uncorrected.D", "uncorrected.I")
  
  scho[[i]] <- unlist(b) %>% data.frame %>% t %>% rbind(scho[[i]], .)
  rownames(scho[[i]]) <- c(cladedata[[i]][[2]], cladedata[[i]][[4]])
  
  ancestorSisNode
}

overlap <- do.call(rbind, scho)

write.csv(overlap, file = paste(genus_tag, "_aunt_nicheOverlap.csv", sep = ""))



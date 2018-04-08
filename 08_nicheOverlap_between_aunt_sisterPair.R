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




##############################################################
# Get list of niche overlap between aunt and sister species
##############################################################

# Time since divergence - niche overlap
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))


ancSisNode <- sapply(1:nrow(sispairs), function(i){
  
  ancestor <- tree$edge[which(sispairs[i, 1] == tree$edge[, 2])]
  ancestorSisNode <- distance2[distance2[,"node"] == ancestor, "sisterNode"]
  
  nodelist <- cbind(sispairs[i, ], ancestor, ancestorSisNode)
  return(nodelist)
  
}
)

ancSisNode <- data.frame(t(ancSisNode))

ancsisOverlapPd <- rbind(
  ((overlapPdData$node1 %in% ancSisNode$ancestorSisNode) %>% overlapPdData[., ]),
  ((overlapPdData$node2 %in% ancSisNode$ancestorSisNode) %>% overlapPdData[., ])
)

targetover <- list()
for(i in 1:nrow(sispairs)){
  node <- (overlapPdData$node1 == ancSisNode$ancestorSisNode[i]) %>% which
  if(overlapPdData$node2[node] == ancSisNode$ancestor[i]){
    target <- overlapPdData[node, ]
    targetover[[i]] <- cbind(target, ancSisNode[i,])
  }
}


ancsisOverlapPd <- do.call(rbind, targetover)

ancsisOverlapPd$nodes<- vapply(ancsisOverlapPd$nodes, paste, collapse = ", ", character(1L))
ancsisOverlapPd$ancestor <- vapply(ancsisOverlapPd$ancestor, paste, collapse = ", ", character(1L))
ancsisOverlapPd$ancestorSisNode <- vapply(ancsisOverlapPd$ancestorSisNode, paste, collapse = ", ", character(1L))
ancsisOverlapPd$sisterNodes <- vapply(ancsisOverlapPd$sisterNodes, paste, collapse = ", ", character(1L))


write.csv(ancsisOverlapPd[, c("nodes", "sisterNodes", "ancestor", "ancestorSisNode", "nicheOverlap", "divergenceTime")], file = paste(genus_tag, "_parent_aunt_nicheOverlap.csv", sep = ""))



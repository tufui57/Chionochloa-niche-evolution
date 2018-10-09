
genus_name <- "Acaena"
genus_tag <- "acaena"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

# Load PCA data and clade paired PCA data
load(
  paste(".\\Scores_", genus_name,"_landcover_worldclim1_1km.data", sep = "")
)
load(paste(".//cladePairData_", genus_tag,".data", sep = ""))


###################################################
### Clade niche volume
###################################################

# Find nodes without occurrence data
naNodes <- sapply(1:max(tree$edge), function(i){
  if(getDescendants(tree, node = i) %>% length <= 1){
    # If the target node has no occurrence records
    if(colnames(scores) %in% rownames(nodes)[i] %>% sum == 0){
      return(i)
    }
  }else{
    descendants <- getDescendants(tree, node = i) %>% rownames(nodes)[.]
    descendantColumn <- colnames(scores) %in% descendants
    
    if(sum(descendantColumn) == 0){
      return(i)
    }
  }
}
)

### Calculate nihce volume
nichevol <- list()

for(i in (1:max(tree$edge))[-unlist(naNodes)]){
  
  targetcladedata <- generateClimateDataOfTargetNode(i, tree, allnodesister, scores, nodes, tips) 
  targetcladedata2 <- (targetcladedata$targetClade == 1) %>%
    targetcladedata[., ]
  
  nichevol[[i]] <- SchoenerD_ecospat(
    # I calculate just one clade of each sister clade pair.
    data1 = targetcladedata2, # data of nodes including internal nodes
    background = scores, data2 = scores, "PC1", "PC2"
  )
}

### Collate the data
nichevoldata <- lapply(nichevol, function(x){
  c(x[[1]], x[[2]])
}
) %>% do.call(rbind, .)

# Add node ID and species name to the data frame
nichevoldata <- nichevoldata %>% 
  data.frame %>%
  mutate(., 
  nodeID = (1:max(tree$edge))[-unlist(naNodes)]
  )


spcol <- sapply(nichevoldata$nodeID, function(x){
  name <- get_spname_from_nodeID(x, tree)
  if(identical(name, character(0))){
    return(x)
  }else{
    return(name) 
  }
})

nichevoldata <- nichevoldata %>% 
  mutate(., 
         spname = spcol
  )

colnames(nichevoldata)[1:4] <- c("ecospat.corrected.D", "ecospat.corrected.I", "ecospat.uncorrected.D", "ecospat.uncorrected.I")

write.csv(nichevoldata, paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))


##############################################################################
### Data preparation
##############################################################################

# genus_name <- "Acaena"
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

# terminal node ID
spnodeID <- ageVolData[!(ageVolData$spname %>% is.na), "node1"]


##############################################################################
### Niche overlap permutation test
##############################################################################
# Generate range niche overlap between random species pairs

testoverlap <- list()
for(i in 1:nrow(sispairs)){
  
  randomIDpair <- sample(spnodeID, 2)
  
  climatedata1 <- generateClimateDataOfTargetNode(randomIDpair[1], tree, allnodesister = allnodesister,
                                  scores = scores, nodes = nodes, tips = tips
                                  )
  climatedata2 <- generateClimateDataOfTargetNode(randomIDpair[2], tree, allnodesister = allnodesister,
                                                  scores = scores, nodes = nodes, tips = tips
  )
  
  testoverlap[[i]] <- SchoenerD_ecospat(scores, "PC1", "PC2",
                                 climatedata1[climatedata1$targetClade == 1,], 
                                 climatedata2[climatedata2$targetClade == 1,] # Data of target clade pair
  )
  
}

testgroup <- lapply(testoverlap, function(x){
  x[[1]][[1]]
}
) %>% unlist

t.test(testgroup, overlapPdData$nicheOverlap[overlapPdData$node1 %in% sispairs$nodes])


##############################################################################
### Range size permutation test
##############################################################################
# Import data
d <- read.csv("Y://Chionochloa_data_analyses.csv")

# Calculate range size ratio of sister species pairs
sisRangeRatio <- list()
for(i in 1:nrow(sispairs)){
  
  range1 <- d[d$spname == get_spname_from_nodeID(sispairs[i,1], tree),"total"]
  range2 <- d[d$spname == get_spname_from_nodeID(sispairs[i,2], tree),"total"]
  
  sisRangeRatio[[i]] <- ifelse(range1 > range2, range2/range1, range1/range2)
  
}

# Generate range size ratio of random species pairs

testRangeRatio <- list()
for(i in 1:nrow(sispairs)){
  
  randomIDpair <- sample(spnodeID, 2)
  
  range1 <- d[d$spname == get_spname_from_nodeID(randomIDpair[1], tree),"total"]
  range2 <- d[d$spname == get_spname_from_nodeID(randomIDpair[2], tree),"total"]
  
  testRangeRatio[[i]] <- ifelse(range1 > range2, range2/range1, range1/range2)

}

t.test(unlist(testRangeRatio), unlist(sisRangeRatio))

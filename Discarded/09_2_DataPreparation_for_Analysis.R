###################################################
### Clade niche overlap/volume
###################################################

genus_name = "Acaena"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
source(".//functions//F_calculate_node_age.R")

# Load PCA data and clade paired PCA data
if(genus_name == "Chionochloa"){
  
  load(".//Scores_chion_24sep.data")
  load(".//cladePairData_chion24sep.data")
}

if(genus_name == "Acaena"){  
  
  load(".//Scores_acaena.data")
  load(".//cladePairData_acaena.data")
  
}

#################################################################################
### Calculate node ages
#################################################################################

agesTipAndNode <- calculateNodeAge(tree)

###################################################################
###  Dataframe of Clade niche volume & age
###################################################################

if(file.exists(paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))){
  volume <- read.csv(paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))
}else{
  source(".//Chionochloa niche evolution//08_Clade_nicheVolume.R")
  volume <- read.csv(paste(".//clade_nicheVolume_", genus_tag, ".csv", sep = ""))
}

extractAges <- agesTipAndNode[rownames(agesTipAndNode) %in% volume$nodeID,]

ageVolData <- cbind(extractAges, volume[order(volume$nodeID), ])

colnames(ageVolData)[c(1,2,4)] <- c("speciesAge", "spname", "nicheVolume")

write.csv(ageVolData[, c("nodeID", "spname", "nicheVolume", "speciesAge")], 
          paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

################################################################################
### Dataframe of Clade niche overlap & divergence time
################################################################################

dis <- data.frame(distance2)
overlap <- read.csv(paste(".//clade_schoennerD_", genus_tag, ".csv", sep = ""))

overlapPdData <- cbind(overlap, dis[dis$node %in% overlap$node1, ])[ ,c("node1", "node2", 
                                                                        "ecospat.corrected", "distance")]

colnames(overlapPdData)[3:4] <- c("nicheOverlap", "phyloDistance")

# Species name of node
overlapspnames <- (nodes$nodelabel %in% overlapPdData$node1) %>% rownames(nodes)[.]
c(overlapspnames, rep(NA, nrow(overlapPdData) - length(overlapspnames)))

overlapPdData <- mutate(overlapPdData, node1name = 
                          c(overlapspnames, rep(NA, nrow(overlapPdData) - length(overlapspnames)))
)

# Show sister species list
for(i in overlapPdData$node1){
  print(i)
  print(rownames(nodes)[allnodesister[[i]]])
}


###################################################
### Add divergence time
###################################################

# Divergence time; branch length of older branch within sister clade pair. 
#                  What should be compared is lengths of time since the two clades diverged from their ancestral species.


overlap <- read.csv(paste(".//clade_schoennerD_", genus_tag, ".csv", sep = ""))

library(phylobase)

get.ancestorNodeLength <- function(tree, node){
  tree2 <- extractTree(tree)
  ancNode <- ancestor(tree2, node)
  
  dis <- dist.nodes(tree)
  ancNodeAge <- dis[min(getDescendants(tree, ancNode)), ancNode]
  
  return(ancNodeAge)
}

### Calculate time since divergence, branch length of the clade pair's closest ancestor
leng <- sapply(1:nrow(overlapPdData), function(i){
  get.ancestorNodeLength(tree, overlap[i, "node1"])
}
)

dist <- data.frame(distance2)

overlapPdData <- cbind(overlap, dist[dist$node %in% overlap$node1, ])[ ,c("node1", "node2", 
                                                                        "ecospat.corrected", "distance")]

colnames(overlapPdData)[3:4] <- c("nicheOverlap", "phyloDistance")
overlapPdData <- mutate(overlapPdData, divergenceTime = leng)

# Species name of node
node1name <- sapply(overlapPdData$node1, get_spname_from_nodeID, tree) %>% 
lapply(., function(x){
  ifelse(identical(x, character(0)), "NA", x)
  }
) %>% unlist

node2name <- sapply(overlapPdData$node2, get_spname_from_nodeID, tree) %>% 
  lapply(., function(x){
    ifelse(identical(x, character(0)), "NA", x)
  }
  ) %>% unlist

# Add species names
overlapPdData <- mutate(overlapPdData, node1name = node1name)
overlapPdData <- mutate(overlapPdData, node2name = node2name)

# Show sister species list
for(i in overlapPdData$node1){
  print(i)
  print(rownames(nodes)[allnodesister[[i]]])
}


write.csv(overlapPdData, 
          paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

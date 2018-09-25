###################################################
### Clade niche overlap/volume
###################################################

genus_name = "Chionochloa"

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

###################################################################
###  Dataframe of Clade niche volume & age
###################################################################

# Import data
if(file.exists(paste(".//clade_nicheVolume_", genus_tag, "24sep.csv", sep = ""))){
  volume <- read.csv(paste(".//clade_nicheVolume_", genus_tag, "24sep.csv", sep = ""))
}else{
  source(".//Chionochloa niche evolution//scripts//04_Clade_nicheVolume.R")
  volume <- read.csv(paste(".//clade_nicheVolume_", genus_tag, "24sep.csv", sep = ""))
}

### Calculate node ages
agesTipAndNode <- calculateNodeAge(tree)
extractAges <- agesTipAndNode[rownames(agesTipAndNode) %in% volume$nodeID,]

# Bind ages and niche volume
ageVolData <- cbind(extractAges, volume[order(volume$nodeID), ])
colnames(ageVolData)[c(1,2,4)] <- c("speciesAge", "spname", "nicheVolume")

write.csv(ageVolData[, c("nodeID", "spname", "nicheVolume", "speciesAge")], 
          paste("NicheVolume_age_", genus_tag, "24sep.csv", sep = ""))

################################################################################
### Dataframe of Clade niche overlap & divergence time
################################################################################

# Import data

if(file.exists(paste(".//clade_nicheVolume_", genus_tag, "24sep.csv", sep = ""))){
  
  overlap <- read.csv(paste(".//clade_schoennerD_", genus_tag, "24sep.csv", sep = ""))
  
}else{
  
  source(".//Chionochloa niche evolution//scripts//04_Clade_nicheVolume.R")
  overlap <- read.csv(paste(".//clade_schoennerD_", genus_tag, "24sep.csv", sep = ""))

}

# Phylogenetic distances
dis <- data.frame(distance2)

overlapPdData <- cbind(overlap, dis[dis$node %in% overlap$node1, ]) %>% 
  .[ ,c("node1", "node2", "ecospat.corrected.D", "distance")]

colnames(overlapPdData)[3:4] <- c("nicheOverlap", "phyloDistance")

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
overlapPdData <- mutate(overlapPdData, node1name = node1name) %>% 
  mutate(., node2name = node2name)

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


### Calculate time since divergence; branch length of the clade pair's closest ancestor
leng <- sapply(1:nrow(overlapPdData), function(i){
  get.ancestorNodeLength(tree, overlap[i, "node1"])
}
)

# Add divergence time
overlapPdData <- mutate(overlapPdData, divergenceTime = leng)

write.csv(overlapPdData, 
          paste("Nicheovrlap_PD_", genus_tag, "24sep.csv", sep = ""))

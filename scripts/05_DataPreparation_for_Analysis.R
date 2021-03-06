###################################################
### Clade niche overlap/volume
###################################################

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
source(".//functions//F_calculate_node_age.R")

# Load PCA data and clade paired PCA data
load(paste(".//cladePairData_5km_", genus_tag,".data", sep = ""))
load(
  paste(".\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")
)

###################################################################
###  Dataframe of Clade niche volume & age
###################################################################

# Import data
if(file.exists(paste(".//clade_nicheVolume_5km_", genus_tag, ".csv", sep = ""))){
  volume <- read.csv(paste(".//clade_nicheVolume_5km_", genus_tag, ".csv", sep = ""))
}else{
  source(".//Chionochloa niche evolution//scripts//04_Clade_nicheVolume.R")
  volume <- read.csv(paste(".//clade_nicheVolume_5km_", genus_tag, ".csv", sep = ""))
}

### Calculate node ages
agesTipAndNode <- calculateNodeAge(tree)
extractAges <- agesTipAndNode[rownames(agesTipAndNode) %in% volume$nodeID,]

# Bind ages and niche volume
ageVolData <- cbind(extractAges, volume[order(volume$nodeID), ])
colnames(ageVolData)[c(1,2,4)] <- c("speciesAge", "spname", "nicheVolume")

write.csv(ageVolData[, c("nodeID", "spname", "nicheVolume", "speciesAge")], 
          paste("Y://NicheVolume_5km_age_", genus_tag, ".csv", sep = ""))

################################################################################
### Dataframe of Clade niche overlap & divergence time
################################################################################

# Import data

if(file.exists(paste(".//clade_schoennerD_5km_", genus_tag, ".csv", sep = ""))){
  
  overlap <- read.csv(paste(".//clade_schoennerD_5km_", genus_tag, ".csv", sep = ""))
  
}else{
  
  source("Y://Chionochloa niche evolution//scripts//04_Clade_nicheOverlap.R")
  overlap <- read.csv(paste(".//clade_schoennerD_5km_", genus_tag, ".csv", sep = ""))

}

# Phylogenetic distances
dis <- data.frame(dist.nodes(tree))

# Get distances between sister node
dis.sister.node <- sapply(1:nrow(overlap), function(i){
  dis[overlap[i,"node1"], overlap[i,"node2"]]
  }
       )

overlapPdData <- cbind(overlap[ ,c("node1", "node2", "ecospat.corrected.D")], dis.sister.node)

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
          paste("Y://Nicheovrlap_PD_5km_", genus_tag, ".csv", sep = ""))


###################################################
### Merge data
###################################################
ageVolData <- read.csv(paste("Y://NicheVolume_5km_age_", genus_tag, ".csv", sep = ""))

splist <- read.csv(paste(genus_name, "_species_list.csv", sep = ""))
pers <- read.csv(paste(".//PersistentOccurrences_", genus_name, ".csv", sep=""))

dat <- merge(splist, ageVolData, by="spname", all.x = T)
dat2 <- merge(dat, pers, by.x="spname", by.y="X", all.x = T)

write.csv(dat2, paste("Y://spdata_5km_", genus_name, ".csv", sep = ""))


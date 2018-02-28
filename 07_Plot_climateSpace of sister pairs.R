###################################################
### Data preparation
###################################################

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//09_DataPreparation.R")
source(".//Chionochloa niche evolution//06_Clade_pairing.R")

###################################################
###  Climate data preparation
###################################################

load(".//Scores_chion.data")

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

########################################################################################
### Get climate dta of sister clades
########################################################################################

# Check if one of their species has no occurrence rescords.
scores[, grepl(genus_name, colnames(scores))] %>% colSums

# Check which species are not shared between phylogenetic tree and occurrence record data
rownames(nodes)[!(rownames(nodes) %in% colnames(scores))]
colnames(scores)[!(colnames(scores) %in% rownames(nodes))]

# No occurrence data for the following nodes
noOccSpNo <- which(!(rownames(nodes) %in% colnames(scores)))
# No occurrence data for their sister clades
noOccSisSpNo <- which(allnodesister %in% noOccSpNo)
  
## The following nodes must be eliminated. Because no occurrence data for them or their sister clades.
number <- (1:max(tree$edge))[-c(noOccSpNo,
                                 noOccSisSpNo,
                                 35 # Oldest ancestor node
                                 )]

cladedata <- lapply(number, function(i){
  
  clades <- generateClimateDataOfClades(i, tree, allnodesister, scores, 
                                        nodes = nodes, tips = tips, spnameCodes = codes)
  
  return(clades)
  }
)
# Name clade data list with their node number
names(cladedata) <- number

save(cladedata, file = ".//cladePairData_chion.data")


########################################################################################
### Plot clade niche of internal nodes
########################################################################################

load(".//cladePairData_chion.data")

# Rrmove duplicated clade pairs
spPairs <- sapply(1:length(cladedata), function(i){
   strsplit(cladedata[[i]][[5]]," ")[[1]] %>% as.numeric %>% sort
  }
)

targetnodes <- spPairs[1, duplicated(spPairs[1,])]

for(i in targetnodes){
  
  clades <- cladedata[[which(names(cladedata) == i)]]
  
  ploTwoGroupWithSpNames(background = scores,
                         axis1 = "PC1", axis2 = "PC2", # Names of coordinates
                         data1 = clades[[1]], data2 = clades[[3]], # Dataframes of two groups of points
                         col1 = "red", col2 = "blue",
                         nodeName = clades[[2]],
                         sisnodeName = clades[[4]],
                         nodeNumber = strsplit(clades[[5]]," ")[[1]][1],
                         extent_x, extent_y,
                         save = TRUE
  )
}



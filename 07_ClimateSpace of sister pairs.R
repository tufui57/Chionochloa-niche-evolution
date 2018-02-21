###################################################
### Clade niche
###################################################

source(".//Chionochloa niche evolution//06_Clade_pairing.R")

library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)

###################################################
###  Climate data preparation
###################################################

load(".//Scores_chion.data")

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

########################################################################################
### Node niche
########################################################################################

# Make species name tag
source(".//Chionochloa niche evolution//makeTag.R")
makeTag(colnames(scores), "Chionochloa")


########################################################################################
### Plot clade niche of internal nodes
########################################################################################

source(".//Acaena niche evolution//generateClimateDataOfClades.R")
source(".//Acaena niche evolution//plotClimateSpaceWithSpNameList.R")

# Check if one of their species has no occurrence rescords.
scores[, grepl("Chion", colnames(scores))] %>% colSums

# Check which species are not shared between phylogenetic tree and occurrence record data
rownames(nodes)[!(rownames(nodes) %in% colnames(scores))]
colnames(scores)[!(colnames(scores) %in% rownames(nodes))]

# No occurrence data for the following nodes
noOccSpNo <- which(!(rownames(nodes) %in% colnames(scores)))
# No occurrence data for their sister clades
noOccSisSpNo <- which(allnodesister %in% noOccSpNo)
  
## The following nodes must be eliminated. Because no occurrence data for them or their sister clades.
number <- (1:max(chion$edge))[-c(noOccSpNo,
                                 noOccSisSpNo,
                                 39, # Oldest ancestor node
                                 74, 75, # Outgroup nodes
                                 40, 41 # Sister nodes of Outgroup nodes
                                 )]

cladedata <- lapply(number, function(i){
  
  clades <- generateClimateDataOfClades(i, chion, allnodesister, scores, 
                                        nodes = nodes, tips = tips, spnameCodes = spname)
  
  return(clades)
  }
)
# Name clade data list with their node number
names(cladedata) <- number

save(cladedata, file = ".//cladePairData_chion.data")

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
                         col1 = "green", col2 = "purple",
                         nodeName = clades[[2]],
                         sisnodeName = clades[[4]],
                         nodeNumber = strsplit(clades[[5]]," ")[[1]][1],
                         extent_x, extent_y,
                         save = TRUE
  )
}



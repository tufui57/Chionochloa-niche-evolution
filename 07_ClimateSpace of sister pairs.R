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

### Import species name codes
spname <- grepl("Chion", colnames(scores)) %>% colnames(scores)[.]
codes <- gsub("Chionochloa_", "", spname) %>% 
  gsub("subsp._", "", .) %>% 
  gsub("var._", "", .)

spname <- (codes %>% substring(., 1, last = 3) %>% mutate(as_tibble(spname), tag = .))
colnames(spname)[1] <- "X"
subsp <- codes %>% 
  strsplit(., "_") %>% 
  lapply(., function(x){
    ifelse(is.na(x[2]), "", x[2])
  }) %>% 
  substring(., 1, last = 3)

spname <- lapply(1:length(subsp), function(i){
    paste(spname[i,"tag"], subsp[i], sep = "_")
}
) %>% unlist %>% 
  gsub("_$", "", .) %>% 
  mutate(spname, tag = .) 


########################################################################################
### Plot clade niche of internal nodes
########################################################################################

source(".//Acaena niche evolution//generateClimateDataOfClades.R")
source(".//Acaena niche evolution//plotClimateSpaceWithSpNameList.R")

# Check if one of their species has no occurrence rescords.
scores[, grepl("Chion", colnames(scores))] %>% colSums
rownames(nodes) <- gsub("subsp", "subsp.", rownames(nodes)) %>% 
  gsub("Chionochloa_flavicans", "Chionochloa_flavicans_f._flavicans", .) %>% 
  gsub("Chionochloa_rubra_subsp._rubra", "Chionochloa_rubra_var._rubra", .) %>% 
  gsub("var_", "var._", .)

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



save(cladedata, file = ".//cladePairData_chion.data")

for(i in 1:length(cladedata)){
  
  clades <- cladedata[[i]]
  
  ploTwoGroupWithSpNames(background = scores,
                         axis1 = "PC1", axis2 = "PC2", # Names of coordinates
                         data1 = clades[[1]], data2 = clades[[3]], # Dataframes of two groups of points
                         col1 = "green", col2 = "purple",
                         nodeName = clades[[2]],
                         sisnodeName = clades[[4]],
                         nodeNumber = i,
                         extent_x, extent_y,
                         save = TRUE
  )
}


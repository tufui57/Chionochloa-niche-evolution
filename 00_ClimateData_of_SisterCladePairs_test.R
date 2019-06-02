###########################################################################################
### Get ordination scores of spcies climatic niche and pair them to their closest clade 
###########################################################################################

###################################################
### Data preparation
###################################################

genus_name <- "Chionochloa"
source(".//Chionochloa niche evolution//00_DataPreparation_sisterPairs.R")
load(
  paste(".\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")
)

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
                                 ifelse(genus_name == "Acaena", 20, 35) # Oldest ancestor node
                                 )]
### Make species name codes
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]
codes <- makeTag_separate(spname, genus_name, "_")


cladedata <- lapply(number, function(i){
  
  clades <- try(
    generateClimateDataOfClades(i, tree, allnodesister, scores, 
                                        nodes = nodes, tips = tips, spnameCodes = codes)
  )
  
  return(clades)
  }
)

# Name clade data list with their node number
names(cladedata) <- number

cladedata <- cladedata[lapply(cladedata, class) == "list"]

save(cladedata, file = paste(".//cladePairData_5km_", genus_tag, ".data", sep = ""))


# ########################################################################################
# ### Plot clade niche of internal nodes
# ########################################################################################
# 
# load(paste(".//cladePairData_", genus_tag, ".data", sep = ""))
# 
# # Rrmove duplicated clade pairs
# spPairs <- sapply(1:length(cladedata), function(i){
#    strsplit(cladedata[[i]][[5]]," ")[[1]] %>% as.numeric %>% sort
#   }
# )
# 
# targetnodes <- spPairs[1, duplicated(spPairs[1,])]
# 
# for(i in targetnodes){
#   
#   clades <- cladedata[[which(names(cladedata) == i)]]
#   
#   ploTwoGroupWithSpNames(background = scores,
#                          axis1 = "PC1", axis2 = "PC2", # Names of coordinates
#                          data1 = clades[[1]], data2 = clades[[3]], # Dataframes of two groups of points
#                          col1 = "red", col2 = "blue",
#                          nodeName = clades[[2]],
#                          sisnodeName = clades[[4]],
#                          nodeNumber = strsplit(clades[[5]]," ")[[1]][1],
#                          extent_x, extent_y,
#                          save = TRUE
#   )
# }
# 
# 

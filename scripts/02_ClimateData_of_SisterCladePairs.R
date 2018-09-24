###########################################################################################
### Get ordination scores of spcies climatic niche and pair them to their closest clade 
###########################################################################################

###################################################
### Data preparation
###################################################
library(phytools)
library(nichePlot)
library(dplyr)

source(".//functions//F_generateClimateDataOfClades.R")
source(".//functions/F_speciseNameCleaning_spnameFromPhylogenyTree.r")

genus_name <- "Chionochloa"

if(genus_name == "Chionochloa"){
  # Import phylogenetic tree data
  org.tree <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")
  tree <- extract.clade(org.tree, 41)
  
  genus_tag <- "chion"
  
  # Get sister pairs
  sislist <- list_sisterSpPairs(tree) %>% as.data.frame %>% na.omit
  duppair <- apply(sislist, 1, sort)[1,] %>% duplicated
  sispairs <- sislist[!duppair, ]
  # Omit no occurrence record species
  sispairs <- sispairs[-c(1,8), ]
  
  # Load PCA data and clade paired PCA data
  load(".//Scores_chion_24sep.data")
}

if(genus_name == "Acaena"){
  # Import phylogeny tree data
  org.tree <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\From Angela\\NZ_Acaena_BEAST_output_6gene.tree")
  tree <- extract.clade(org.tree, 28)
  
  genus_tag <- "acaena"
  
  # Get sister pairs
  sislist <- list_sisterSpPairs(tree) %>% as.data.frame %>% na.omit
  duppair <- apply(sislist, 1, sort)[1,] %>% duplicated
  sispairs <- sislist[!duppair, ]
  # Omit no occurrence record species
  sispairs <- sispairs[-c(5,6), ]
  
  # Load PCA data and clade paired PCA data
  load(".//Scores_acaena.data")
}

### Load clade pair data
source(".//Chionochloa niche evolution//F01_Clade_pairing.R")

### Extent of axes for plotting
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))


### Make species name codes
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]
codes <- makeTag_separate(spname, genus_name, "_")


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

save(cladedata, file = paste(".//cladePairData_", genus_tag, "24sep.data", sep = ""))



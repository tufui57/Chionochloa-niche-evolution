########################################################################################
### Libraries and functions
########################################################################################

library(phylobase)
library(phytools)
library(nichePlot)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggtree)

source(".//Acaena niche evolution//F_generateClimateDataOfClades.R")
source(".//Acaena niche evolution//F_SchonnerDdataframeFormat.r")
source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")
source(".//Acaena niche evolution//F_plotClimateSpaceWithPhylogenyTree.R")

##############################################################################
### Data preparation
##############################################################################

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
  load(".//Scores_chion.data")
  load(".//cladePairData_chion.data")
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
  load(".//cladePairData_acaena.data")
  
  
}

source(".//Chionochloa niche evolution//06_Clade_pairing.R")

# Extent of axes for plotting
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))


### Make species name codes
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]
codes <- makeTag_separate(spname, genus_name, "_")


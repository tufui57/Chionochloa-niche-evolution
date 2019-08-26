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

source(".//functions//F_generateClimateDataOfClades.R")
source(".//functions//F_SchonnerDdataframeFormat.r")
source(".//functions//F_plotAnalysis_clade_niche.R")
source(".//functions/F_speciseNameCleaning_spnameFromPhylogenyTree.r")
source(".//functions//F_plotClimateSpaceWithPhylogenyTree.R")

##############################################################################
### Data preparation
##############################################################################

genus_name = "Acaena"

if(genus_name == "Chionochloa"){
  # Import phylogenetic tree data
  org.tree <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")
  tree <- extract.clade(org.tree, 41)
}

if(genus_name == "Acaena"){
  # Import phylogeny tree data
  org.tree <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\From Angela\\NZ_Acaena_BEAST_output_6gene.tree")
  tree <- extract.clade(org.tree, 28)
  }
  
########################################################################################
### Plot phylogeny trees with barplot
########################################################################################

# Tip label change to species name
tree$tip.label <- clean_speciesname(tree$tip.label) %>% gsub(genus_name, 
                                                             ifelse(genus_name == "Chionochloa", "C.", "A."),
                                                             .)
tree$tip.label <- gsub("subsp.","subsp", tree$tip.label)

# Load niche volume data that are plotted in barplot
range <- read.csv(paste("Y://", genus_name,"_nicheInStable.csv", sep=""))

mat <- data.frame(
  cbind(gsub(genus_name, ifelse(genus_name == "Chionochloa", "C.", "A."), range$spname),
        range$propNicheVolInStable
        ) 
      )
mat1 <- data.frame(
  cbind(tree$tip.label, 0)
  )

mat2 <- merge(mat1, mat, by="X1", all = T)

values <- as.numeric(as.character(mat2[,3])) 
values[is.na(values)] <-  0
names(values) <- as.character(mat2[,1])

######################################################
# the following barplot shows wrong values
######################################################

png(paste("Y:\\", genus_name , "wrong_phylogeny_tree_barplot.png", sep = ""), width = 1800, height = 1250)

layout(matrix(c(1,2),1,2), widths = c(0.7,0.3))
par(cex=3)
plotTree.barplot(tree, values,
                 #args.plotTree = list(fsize=3),
                 args.barplot = list(
  xlab = "Proportion of niche volume\nin stable environments"),
  add = T
  )
dev.off()

######################################################
# the following barplot function works right
######################################################

png(paste("Y:\\", genus_name , "_phylogeny_tree_barplot.png", sep = ""), width = 1800, height = 1250)
par(cex=3)
plotTree.wBars(tree, values, tip.labels = T
               )
dev.off()


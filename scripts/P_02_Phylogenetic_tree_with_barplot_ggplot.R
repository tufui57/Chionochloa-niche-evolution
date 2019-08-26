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
### Plot phylogeny trees with barplot by ggtree
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

val <- tibble(sp = names(values),
              val = values
                )
taca <- ggtree(tree) + geom_tiplab()

taca2 <- facet_plot(taca, panel="bar", data = val, geom = geom_barh,
           mapping = aes(x = values),
           stat = "identity")

### Change plot layout of ggtree

# load the packages
library(grid)
library(gtable)

gt = ggplot_gtable(ggplot_build(taca2))
gtable_show_layout(gt) # will show you the layout - very handy function
gt # see plot layout in table format
gt$layout$l[grep('panel-2', gt$layout$name)] # you want to find the column specific to panel-2
gt$widths[7] = 0.3*gt$widths[7] # in this case it was colmun 7 - reduce the width by a half
grid.draw(gt) # plot with grid draw


### Another way to plot barplot with ggtree

facet_plot(taca, panel="bar", data=val,geom=geom_segment, 
aes(x=0, xend=val, y=y, yend=y), size=3, color='blue4') 


########################################################################################
### Phylogeny tree drawing
########################################################################################
genus_name = "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

########################################################################################
### Plot edge length on trees
########################################################################################

tree$tip.label <- clean_speciesname(tree$tip.label)

png(paste("Y:\\", genus_name , "_phylogeny_tree2.png", sep = ""), width = 1800, height = 1250)
plotTree(tree, adj = 0, label.offset = 0.75, no.margin = T, cex = 1.5)
edgelabels(round(tree$edge.length, 5),
           adj = 1,
           cex = 1
           )
nodelabels(cex = 1.5, adj = 1)
tiplabels(cex = 2,adj = 1)
dev.off()

########################################################################################
### Phylogeny tree drawing
########################################################################################
genus_name = "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

tree<-
########################################################################################
### Plot edge length on trees
########################################################################################

tree$tip.label <- clean_speciesname(tree$tip.label)

png(paste("Y:\\", genus_name , "_phylogeny_tree.png", sep = ""), width = 1800, height = 1250)
plotTree(tree, adj = 0, label.offset = 0.75, no.margin = T, cex = 1.5)
# edgelabels(round(extracted.tree$edge.length, 5),
#            adj = 2,
#            cex = 1.5
#            )
nodelabels(cex = 1.5, adj = 1)
tiplabels(cex = 2,adj = 1)
dev.off()

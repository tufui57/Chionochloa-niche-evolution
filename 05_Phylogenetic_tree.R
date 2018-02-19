########################################################################################
### Phylogeny tree drawing
########################################################################################

library(adegenet)
library(geiger)
library(phytools)

# Import tree data
chion <- read.nexus("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")

########################################################################################
### Plot edge length on trees
########################################################################################

png("Y:\\chion phylogeny tree.png", width = 1800, height = 1250)
plotTree(chion, adj = 0, label.offset = 0.75, no.margin = TRUE, cex = 1.5)
# edgelabels(round(chion$edge.length, 5),
#            adj = 2,
#            cex = 1.5
#            )
nodelabels(cex = 1.5, adj = 2)
tiplabels(cex = 1.5,adj = 1)
dev.off()


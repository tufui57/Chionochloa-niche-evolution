########################################################################################
### Phylogeny tree drawing
########################################################################################
genus_name = "Acaena" # "Chionochloa"

source(".//Chionochloa niche evolution//scripts//00_DataPreparation.R")

########################################################################################
### Plot trees
########################################################################################

# # Get species name tags for tip labels
# spname <- clean_speciesname(tree$tip.label) %>% 
#   makeTag_separate(., genus_name) %>% as.data.frame
# 
# tree$tip.label <- spname$tag

# Species name
tree$tip.label <- clean_speciesname(tree$tip.label) %>% gsub(genus_name, 
                                                             ifelse(genus_name=="Chionochloa", "C.", "A."),
                                                             .)

# Plot trees
png(paste("Y:\\", genus_name , "_phylogeny_tree.png", sep = ""), width = 1800, height = 1250)
plotTree(tree, fsize = 2)
# Write branch lengths
edgelabels(round(tree$edge.length, 5),
           adj = 1,
           cex = 2.5
)
# Write node IDs
nodelabels(cex = 2.5, adj = 1)
# Write tip node IDs
tiplabels(cex = 2.5,adj = 1)
dev.off()

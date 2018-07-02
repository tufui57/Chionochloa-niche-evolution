###################################################################
### Phylogenetic distances between all combinations of species
###################################################################
# Phylogenetic distance is branch lengths connecting two nodes

source(".//Chionochloa niche evolution//00_DataPreparation.R")

## Distance between all combinations of tips
distances <- dist.nodes(tree)

dis <- distances[nodes$nodelabel, nodes$nodelabel]

# Name columns and rows of the phylogentic distance matrix between species
rownames(dis) <- rownames(nodes)
colnames(dis) <- rownames(nodes)


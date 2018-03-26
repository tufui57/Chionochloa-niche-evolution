###################################################################
### Phylogenetic distances between all combinations of species
###################################################################
# Phylogenetic distance is branch lengths connecting two nodes

## Distance between all combinations of tips
distances <- dist.nodes(tree)

dis <- distances[nodes$nodelabel, nodes$nodelabel] %>% cbind(nodes)

rownames(dis) <- rownames(nodes)

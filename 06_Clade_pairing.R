
########################################################################################
# Import phylogenetic tree data before running this script
########################################################################################


### Extract the set of terminal edge lengths associated with these tips.
# Extract names of edges (i.e. tips and taxa)
tips <- tree$tip.label

## first get the node numbers of the tips
nodes <- data.frame(sapply(tips, function(x,y) which(y == x), y = tree$tip.label))
colnames(nodes) <- "nodelabel"

# Modify names
rownames(nodes) <- clean_speciesname(rownames(nodes))

## Second, find sister node of a target species

tipssister <- findSisterNode(tree)

# Name a result list
names(tipssister) <- rownames(nodes)

### Get a sister group of internal nodes as well as terminal nodes

# Get node numbers of internal nodes
allnodesister <- GetInternalNodeNumber(tree)

allnodesister[1:length(tipssister)] <- tipssister



########################################################################################
### Phylogenetic distance between nodes
########################################################################################
# Phylogenetic distance between internal nodes is branch lengths connecting the nodes.

# The order of the node pair doesn't matter. 
# dist.nodes(tree)[i, getSisters(tree, i)] == dist.nodes(tree)[getSisters(tree, i), i]

distance <- sapply(1:max(tree$edge), function(i){
  c(i, getSisters(tree, i), dist.nodes(tree)[i, getSisters(tree, i)])
})

distance2 <- do.call(rbind, distance)
colnames(distance2) <- c("node", "sisterNode","distance")


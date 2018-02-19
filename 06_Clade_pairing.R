########################################################################################
### Node ages
########################################################################################
########################################################################################
library(dplyr)
library(phytools)
library(nichePlot)

# Import phylogenetic tree data
chion <- read.nexus("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")

### Extract the set of terminal edge lengths associated with these tips.
# Extract names of edges (i.e. tips and taxa)
tips <- chion$tip.label

## first get the node numbers of the tips
nodes <- data.frame(sapply(tips, function(x,y) which(y == x), y = chion$tip.label))
colnames(nodes) <- "nodelabel"

# Modify names
scores[, grepl("Chion", colnames(scores))] %>% colSums
rownames(nodes) <- gsub("subsp", "subsp.", rownames(nodes)) %>% 
  gsub("Chionochloa_flavicans", "Chionochloa_flavicans_f._flavicans", .) %>% 
  gsub("Chionochloa_rubra_subsp._rubra", "Chionochloa_rubra_var._rubra", .) %>% 
  gsub("var_", "var._", .)

## Second, find sister node of a target species

tipssister <- findSisterNode(chion)

# Name a result list
names(tipssister) <- rownames(nodes)

### Get a sister group of internal nodes as well as terminal nodes

# Get node numbers of internal nodes
allnodesister <- GetInternalNodeNumber(chion)

allnodesister[1:length(tipssister)] <- tipssister



########################################################################################
### Phylogenetic distance between nodes
########################################################################################

# The order of the node pair doesn't matter. 
# dist.nodes(chion)[i, getSisters(chion, i)] == dist.nodes(chion)[getSisters(chion, i), i]

distance <- sapply(1:max(chion$edge), function(i){
  c(i, getSisters(chion, i), dist.nodes(chion)[i, getSisters(chion, i)])
})

distance2 <- do.call(rbind, distance)
colnames(distance2) <- c("node", "sisterNode","distance")


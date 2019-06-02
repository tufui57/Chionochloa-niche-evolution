########################################################################################
### Plot clade/sister species pair niche 
########################################################################################

source(".//functions//F_plotClimateSpaceWithPhylogenyTree.R")

library(ggplot2)
library(grid)
library(ggtree)

# Load clade paired climate data
load(paste(".//cladePairData_", genus_tag, ".data", sep = ""))

# Remove duplicated clade pairs
spPairs <- sapply(1:length(cladedata), function(i){
  strsplit(cladedata[[i]][[5]]," ")[[1]] %>% as.numeric %>% sort
}
)

targetnodes <- spPairs[1, duplicated(spPairs[1,])]

for(i in targetnodes){
  
  clades <- cladedata[[which(names(cladedata) == i)]]
  
  ploTwoGroupWithSpNames(background = scores,
                         axis1 = "PC1", axis2 = "PC2", # Names of coordinates
                         data1 = clades[[1]], data2 = clades[[3]], # Dataframes of two groups of points
                         col1 = "red", col2 = "blue",
                         nodeName = clades[[2]],
                         sisnodeName = clades[[4]],
                         nodeNumber = strsplit(clades[[5]]," ")[[1]][1],
                         extent_x, extent_y,
                         save = TRUE
  )
}



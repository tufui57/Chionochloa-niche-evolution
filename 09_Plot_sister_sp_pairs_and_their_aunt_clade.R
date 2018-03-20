
genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

########################################################################################
### Plot niche of sister species pair and its sister nodes (aunt of target pair)
########################################################################################

plot_sister_ancestor <- function(i, # target node ID
                                 spnameCodes, # list if species name codes
                                 cols, # Colours of points for c(aunt clade, sister species 1, sister species 2) 
                                 pl = FALSE # Plot 
){
  
  ## Find parent node of target sister species pair
  ancestor <- tree$edge[which(i == tree$edge[, 2])]
  ancestorSisNode <- distance2[distance2[, "node"] == ancestor, "sisterNode"]
  
  print(paste("Sister node of parent node of target sister species pair is", ancestorSisNode))
  
  ## Niche of sister node of ancestor node
  ansSisScore <- generateClimateDataOfTargetNode(
    ancestorSisNode, tree, allnodesister, scores, nodes, tips)
  ansSisScore2 <- ansSisScore[ansSisScore[, "targetClade"] == 1, c("PC1", "PC2", "targetClade")]
  
  ## Target species niche
  cladesp <- colnames(scores) %in% rownames(nodes)[i]
  
  cladeScore <- generateClimateDataOfTargetNode(
    i, tree, allnodesister, scores, nodes, tips)
  cladeScore2 <- cladeScore[cladeScore[, "targetClade"] == 1, c("PC1", "PC2", "targetClade")]
  
  
  ## Niche of sister species
  sisCladeSp <- colnames(scores) %in% rownames(nodes)[allnodesister[[i]]]
  sisnodeNumber <- distance2[distance2[, "node"] == i, "sisterNode"]
  
  sisCladeScore <- generateClimateDataOfTargetNode(
    sisnodeNumber, tree, allnodesister, scores, nodes, tips)
  sisCladeScore2 <- sisCladeScore[sisCladeScore[, "targetClade"] == 1, c("PC1", "PC2", "targetClade")]
  
  ## Collate data
  nodeNumber = i
  
  # Species name codes
  nodeName = pull(spnameCodes[spnameCodes$X %in% colnames(scores)[cladesp], ], tag)
  sisnodeNumber = distance2[i, "sisterNode"]
  sisnodeName = pull(spnameCodes[spnameCodes$X %in% rownames(nodes)[allnodesister[[i]]], ], tag)
  
  ancestorSisName = ancestorSisNode
  
  ### Plot niche
  
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', stroke = 0, alpha = 0.25) +
    
    # point of sister node of ancestor
    geom_point(data = ansSisScore2, aes(PC1, PC2), color = cols[1], stroke = 0, alpha = 0.3, size = 2.5) +
    
    # point of each sp
    geom_point(data = cladeScore2, aes(PC1, PC2), color = cols[2], stroke = 0, alpha = 0.3, size = 2.5) +
    
    # point of each sp
    geom_point(data = sisCladeScore2, aes(PC1, PC2), color = cols[3], stroke = 0, alpha = 0.3, size = 2.5) +
    
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    
    # legend position inside plot
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),
          panel.background = element_rect(fill = 'gray96')
    )
  
  
  ### Make phylogenetic tree of plotted clades
  granma <- tree$edge[which(ancestor == tree$edge[, 2])]
  extree <- extract.clade(tree, granma)
  
  extree$tip.label <- makeTag_separate(clean_speciesname(extree$tip.label), genus_name, "_") %>% pull(., tag)
  
  # Create dataframe to tell colours of tip labels
  dd <- data.frame(taxa  = extree$tip.label,
                   place = rep("aunt", length(extree$tip.label))
  )
  dd <- mutate(dd, place = as.character(dd$place))
  
  sis1 <- (dd$taxa == makeTag_separate(get_spname_from_nodeID(i, tree), genus_name, "_") %>% pull(., tag))
  dd$place[sis1] <- "sis1"
  
  sis2 <- (dd$taxa == makeTag_separate(get_spname_from_nodeID(sisnodeNumber, tree), genus_name, "_") %>% pull(., tag))
  dd$place[sis2] <- "sis2"
  
  
  # Plot extracted phylogeny tree
  phylotree <- ggtree(extree) %<+% dd + 
    geom_tiplab(aes(color=place), size = 5, hjust=-.3) +
    ggplot2::xlim(0, 0.0025) +
    scale_color_manual(values = cols) # colour is ordered by alphabet of the factors in dataframe dd.
  
  
  gg <- grid.arrange(pMain, phylotree,
                     widths = c(3,1))
  
  
  if(pl == T){
    ### Plot tow species niche on one figure 
    png(filename = paste("Y:\\niche_", ancestorSisName, nodeName, sisnodeName, ".png"), width = 900, height = 630)
    grid.newpage()
    grid.draw(gg)
    dev.off()
    
  }else{
    
    return(gg)
    
  }
  
}


for(i in sispairs[, 1]){
  plot_sister_ancestor(i, spnameCodes = codes, cols = c("gray60", "red", "blue"), pl = TRUE)
}



########################################################################################
### Data preparation
########################################################################################

load(".//Scores_chion.data")
load(".//cladePairData_chion.data")

setwd(".//Chionochloa niche evolution")

# tidyverse loads too much DLLs. Load tidyr instead.
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

source("..//Acaena niche evolution//generateClimateDataOfClades.R")
source("..//Acaena niche evolution//SchonnerDdataframeFormat.r")

source("..//Chionochloa niche evolution//06_Clade_pairing.R")
### Import species name codes

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

########################################################################################
### Plot niche of sister species pair and its sister nodes (aunt of target pair)
########################################################################################

# First clade in sister pairs
sispairs <- c(9,29,20,33,15,12,4,6,31,30)


### Make species name codes
spname <- grepl("Chion", colnames(scores)) %>% colnames(scores)[.]
codes <- gsub("Chionochloa_", "", spname) %>% 
  gsub("subsp._", "", .) %>% 
  gsub("var._", "", .)

spname <- (codes %>% substring(., 1, last = 3) %>% mutate(as_tibble(spname), tag = .))
colnames(spname)[1] <- "X"
subsp <- codes %>% 
  strsplit(., "_") %>% 
  lapply(., function(x){
    ifelse(is.na(x[2]), "", x[2])
  }) %>% 
  substring(., 1, last = 3)

spname <- lapply(1:length(subsp), function(i){
  paste(spname[i,"tag"], subsp[i], sep = "_")
}
) %>% unlist %>% 
  gsub("_$", "", .) %>% 
  mutate(spname, tag = .) 


plot_sister_ancestor <- function(i,
                                 spnameCodes,
                                 pl = FALSE # Plot 
                                 ){

  ## Find parent node of target sister species pair
  ancestor <- chion$edge[which(i == chion$edge[,2])]
  ancestorSisNode <- distance2[distance2[,"node"] == ancestor, "sisterNode"]
  
  print(paste("Sister node of parent node of target sister species pair is", ancestorSisNode))
  
  ## Niche of sister node of ancestor node
  ansSisScore <- generateClimateDataOfTargetNode(
    ancestorSisNode, chion, allnodesister,scores, nodes, tips)
  
  ansSisScore2 <- ansSisScore[ansSisScore[,"targetClade"] == 1, c("PC1", "PC2", "targetClade")]
  
  ## Target species niche
  cladesp <- colnames(scores) %in% rownames(nodes)[i]
  scores$targetClade <- scores[,cladesp]
  cladeScore <- scores[scores[,"targetClade"] == 1, c("PC1", "PC2", "targetClade")]
  
  
  ## Niche of sister species
  sisCladeSp <- colnames(scores) %in% rownames(nodes)[allnodesister[[i]]]
  scores$sisClade <- scores[,sisCladeSp]
  sisCladeScore <- scores[scores[,"sisClade"] == 1, c("PC1", "PC2", "sisClade")]
  
  
  ## Data
  nodeNumber = i
  # Species name codes
  nodeName = pull(spnameCodes[spnameCodes$X %in% colnames(scores)[cladesp], ], tag)
  sisnodeNumber = distance2[i, "sisterNode"]
  sisnodeName = pull(spnameCodes[spnameCodes$X %in% rownames(nodes)[allnodesister[[i]]], ], tag)
  
  ancestorSisName = ancestorSisNode
  
  ### Plot niche
  
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of sister node of ancestor
    geom_point(data = ansSisScore2, aes(PC1, PC2), color = "red", alpha = 0.3) +
    
    # point of each sp
    geom_point(data = cladeScore, aes(PC1, PC2), color = "green", alpha = 0.3) +
    # point of each sp
    geom_point(data = sisCladeScore, aes(PC1, PC2), color = "purple", alpha = 0.3) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    # legend position inside plot
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),
          panel.background = element_rect(fill = 'gray96')
    )
  
  ### Make multiple coloured title
  grobs <- grobTree(
    gp = gpar(fontsize = 14, fontface = "bold"), 
    textGrob(label = ancestorSisName, name = "title1",
             x = unit(0.2, "lines"), y = unit(1.4, "lines"), 
             hjust = 0, vjust = 0, gp = gpar(col = "red")),
    textGrob(label = nodeName, name = "title2",
             x = grobWidth("title1") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = "green")),
    textGrob(label = sisnodeName, name = "title3",
             x = grobWidth("title1") + grobWidth("title2") + unit(0.2, "lines"), y = unit(1.4, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = "purple"))
  )
  
  gg <- arrangeGrob(pMain, top=grobs, padding = unit(2.6, "line"))

  
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


anssisplots <- lapply(sispairs, plot_sister_ancestor, spnameCodes = spname, pl = TRUE)






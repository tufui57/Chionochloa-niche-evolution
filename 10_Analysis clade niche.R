###################################################
### Clade niche analysis
###################################################

library(phytools)
library(ape)

##############################################################################
### Data preparation
##############################################################################

source(".//Chionochloa niche evolution//06_Clade_pairing.R")

#################################################################################
### Calculate node ages
#################################################################################

### Distance between all combinations of tips
distances <- dist.nodes(chion)

# Calculate species ages
ages <- sapply(nodes[[1]], function(i){
  # Phylogenetic distance list has 0 (distance to themselves)
  (distances[,i] > 0) %>% distances[., i] %>% min
  }
)

ages <- as_tibble(ages) %>% mutate(., spname = row.names(nodes))

### Calculate internal node ages
nodeage <- cbind(rep(NA, length(branching.times(chion))), branching.times(chion))
colnames(nodeage) <- c("spname", "value")


agesTipAndNode <- rbind(ages, nodeage)


########################################
###  Dataframe of Clade niche volume & age
########################################

volume <- read.csv(".//clade_nicheVolume_chion.csv")
extractAges <- agesTipAndNode[rownames(agesTipAndNode) %in% volume$node1,]

ageVolData <- cbind(extractAges, volume[order(volume$node1), ])


###################################################
### Dataframe of Clade niche overlap & phylogenetic distances
###################################################

dis <- data.frame(distance2)
overlap <- read.csv(".//clade_schoennerD_chion.csv")

overlapPdData <- cbind(overlap, dis[dis$node %in% overlap$node1, ])[ ,c("node1", "node2", 
                                                      "ecospat.corrected", "ecospat.uncorrected", "distance")]




#########################################################################
### Plots
#########################################################################

source(".//Acaena niche evolution//plotAnalysis_clade_niche.R")

#########################################################################
### Sister species pairs' Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

### Node numbers of sister species pairs
sispairs <- c(9,29,20,33,15,12,4,6,31,30)
sisOverlapPd <- (overlapPdData$node1 %in% sispairs) %>% overlapPdData[., ]

m <- lm(distance ~ ecospat.corrected, sisOverlapPd)

myplot <- plotAnalysis(data = sisOverlapPd, 
                       m = m, 
                       xv = "ecospat.corrected", yv = "distance", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche overlap of occurrence records", ylabname = "Phylogenetic distances between sister species pairs"
)

# save
ggsave(paste("Y:\\chion_sister_pd_nicheoverlap_legend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

#########################################################################
### Sister species pair - their ancestor's sister node
### Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

### Node numbers of sister species pairs
sispairs <- c(9,29,20,33,15,12,4,6,31,30)

ancSisNode <- sapply(sispairs, function(i){
  
  ancestor <- chion$edge[which(i == chion$edge[, 2])]
  ancestorSisNode <- distance2[distance2[,"node"] == ancestor, "sisterNode"]
  return(ancestorSisNode)
  
  }
)


ancsisOverlapPd <- rbind(
  ((overlapPdData$node1 %in% ancSisNode) %>% overlapPdData[., ]),
  ((overlapPdData$node2 %in% ancSisNode) %>% overlapPdData[., ])
)

dup <- duplicated(ancsisOverlapPd$ecospat.corrected) %>% which
ancsisOverlapPd <- ancsisOverlapPd[-dup, ]

m <- lm(distance ~ ecospat.corrected, ancsisOverlapPd)

myplot <- plotAnalysis(data = ancsisOverlapPd, 
                       m = m, 
                       xv = "ecospat.corrected", yv = "distance", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche overlap of occurrence records", ylabname = "Phylogenetic distances between sister species pairs"
)

# save
ggsave(paste("Y:\\chion_sisterAunt_pd_nicheoverlap_legend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

#########################################################################
### Clade Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

m <- lm(distance ~ ecospat.corrected, overlapPdData)

myplot <- plotAnalysis(data = overlapPdData, m = m, xv = "ecospat.corrected", yv = "distance",
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche overlap of occurrence records", ylabname = "Phylogenetic distances between clades")

# save
ggsave(paste("Y:\\chion_clade_pd_nicheoverlap_outlier.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

#########################################################################
### Clade ages ~ niche volume of occurrence records
#########################################################################

m <- lm(value ~ ecospat.corrected, ageVolData)

myplot <- plotAnalysis(data=ageVolData, m=m, xv = "ecospat.corrected", yv = "value", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche volume of occurrence records", ylabname = "Clade age")

# save
ggsave(paste("Y:\\chion_clade_age_nicheVolume_outlier.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

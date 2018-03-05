###################################################
### Clade niche analysis
###################################################

##############################################################################
### Data preparation
##############################################################################

genus_name <- "Acaena"
# genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

#########################################################################
### Plots
#########################################################################

ageVolData <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

#########################################################################
### Sister species pairs' Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

### Node numbers of sister species pairs

sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]

myplot <- plotAnalysis(data = sisOverlapPd, 
                       yv = "nicheOverlap", xv = "phyloDistance", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap of occurrence records", 
                       xlabname = "Phylogenetic distances between sister species pairs",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\sister_pd_nicheoverlap_legend_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

#########################################################################
### Sister species pair - their ancestor's sister node
### Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

ancSisNode <- sapply(sispairs[,1], function(i){
  
  ancestor <- tree$edge[which(i == tree$edge[, 2])]
  ancestorSisNode <- distance2[distance2[,"node"] == ancestor, "sisterNode"]
  return(ancestorSisNode)
  
}
)

ancsisOverlapPd <- rbind(
  ((overlapPdData$node1 %in% ancSisNode) %>% overlapPdData[., ]),
  ((overlapPdData$node2 %in% ancSisNode) %>% overlapPdData[., ])
)

dup <- duplicated(ancsisOverlapPd$nicheOverlap) %>% which
ancsisOverlapPd <- ancsisOverlapPd[-dup, ]

myplot <- plotAnalysis(data = ancsisOverlapPd,
                       yv = "nicheOverlap", xv = "phyloDistance", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap of occurrence records", 
                       xlabname = "Phylogenetic distances between sister species pair and its closest clade",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\sisterAunt_pd_nicheoverlap_legend_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

#########################################################################
### Clade Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

### Omit duplicated pairs
dup <- duplicated(overlapPdData$phyloDistance) %>% which
overlapPdData <- overlapPdData[-dup, ]


### Eliminate sister species pairs
overlapPdclade <- (!(overlapPdData$node1 %in% sispairs[ , 1])) %>% overlapPdData[.,]


### Eliminate outlier of clade age

outlier <- which(max(overlapPdclade$phyloDistance) == overlapPdclade$phyloDistance)

overlapPd <- overlapPdclade[-outlier, ]

myplot <- plotAnalysis(data = overlapPd,
                       yv = "nicheOverlap", xv = "phyloDistance", 
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap of occurrence records",
                       xlabname = "Phylogenetic distances between clades",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\clade_pd_nicheoverlap_legend_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


### Leave outlier of clade age in data

myplot <- plotAnalysis(data = overlapPdclade, 
                       yv = "nicheOverlap", xv = "phyloDistance",
                       nodeNumbercol = "node1", showStats = T,
                       ylabname = "Niche overlap of occurrence records", 
                       xlabname = "Phylogenetic distances between clades",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\clade_pd_nicheoverlap_outlier_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

#########################################################################
### Clade ages ~ niche volume of occurrence records
#########################################################################

### Eliminate outlier
outlier <- which(max(ageVolData$speciesAge) == ageVolData$speciesAge)

myplot <- plotAnalysis(data = ageVolData[-outlier,],
                       yv = "nicheVolume", xv = "speciesAge", 
                       nodeNumber = "node1", showStats = T,
                       ylabname = "Niche volume of occurrence records",
                       xlabname = "Clade age",
                       label.point = TRUE
) +
  ylim(0, 1)

# save
ggsave(paste("Y:\\clade_age_nicheVolume_legend_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


### Leave outlier
myplot <- plotAnalysis(data = ageVolData,
                       yv = "nicheVolume", xv = "speciesAge", 
                       nodeNumber = "node1", showStats = T,
                       ylabname = "Niche volume of occurrence records",
                       xlabname = "Clade age",
                       label.point = TRUE
) +
  ylim(0, 1)
# save
ggsave(paste("Y:\\clade_age_nicheVolume_outlier_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)

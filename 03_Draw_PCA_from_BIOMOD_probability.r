############################################################################################################
############################   Get Schoenner's D from two groups of probability
############################################################################################################

library(dplyr)
library(dismo)
library(raster)
source(".\\Chionochloa niche evolution\\makeTag.R")
source(".//Acaena niche evolution//plotAnalysis_clade_niche.R")

genus_name <- "Chionochloa"

# Load prediction of ensemble model
load("Y://ensemblePrediction_chion.data")

source(paste(".\\", genus_name, " niche evolution\\06_Clade_pairing.R", sep = ""))

spname <- gsub("\\.", "\\_", names(pred)) %>% 
  gsub("var_", "var.", .) %>% 
  gsub("subsp_", "subsp.", .)

code <- makeTag_separate(spname, genus_name, "_")

# First clade in sister pairs
sispairs <- c(9,29,20,33,15,12,4,6,31,30)

probD <- list()
for(i in sispairs){
  # Species name codes
  nodeName = pull(code[code$X %in% rownames(nodes)[i], ], X)
  sisnodeName = pull(code[code$X %in% rownames(nodes)[allnodesister[[i]]], ], X)
  
  probANS <- (spname == nodeName) %>% pred[.]
  probDUM <- (spname == sisnodeName) %>% pred[.]
  ### Use dismo::nicheOverlap
  probD[[i]] <- nicheOverlap(probANS[[1]], probDUM[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE) 
  
}



############################################################################################################
##### Compare Schoenner's D from two groups of probability and the one from occurrence records
############################################################################################################

# Import data
overlapPdData <- read.csv("Nicheovrlap_PD_chion.csv")

### Node numbers of sister species pairs
sisOverlapPd <- (overlapPdData$node1 %in% sispairs) %>% overlapPdData[., ]

overlaps <- cbind(sisOverlapPd, unlist(probD))
colnames(overlaps)[ncol(overlaps)] <- "probD"


m <- lm(probD ~ nicheOverlap, overlaps)
myplot <- plotAnalysis(data = overlaps, 
                       m = m, 
                       xv = "nicheOverlap", yv = "probD", 
                       nodeNumber = "node1", showStats = T,
                       xlabname = "Niche overlap of occurrence records", ylabname = "Niche overlap of model prediction"
)

# save
ggsave(paste("Y:\\sister_nicheoverlap_chion.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


#########################################################################
### Sister species pairs' Phylogenetic distances ~ niche overlap of predictions
#########################################################################

m <- lm(phyloDistance ~ probD, overlaps)
myplot <- plotAnalysis(data = overlaps, 
                       m = m, 
                       yv = "phyloDistance", xv = "probD", 
                       nodeNumber = "node1", showStats = T,
                       ylabname = "Phylogenetic distances between sister species pairs", xlabname = "Niche overlap of model prediction"
)

# save
ggsave(paste("Y:\\sister_predNicheoverlap_phyloDistance_chion.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


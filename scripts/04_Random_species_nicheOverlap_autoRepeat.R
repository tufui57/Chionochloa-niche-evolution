#####################################################################################
##  speccies 
#####################################################################################

library(ecospat)
library(nichePlot)
library(dplyr)
source(".//functions//F_SchonnerDdataframeFormat.r")

genus_name <- "Chionochloa" #"Acaena" 
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

res <- list()
for(j in 1:100){
  
  source(".//Chionochloa niche evolution//scripts//04_Random_species_nicheOverlap.R")
  
  #########################################################################
  ### Climatic niche similarity between sister vs. random species pairs
  #########################################################################
  
  # Load niche similarity between random species pairs
  ran <- read.csv(paste(".//Random_sp_pair_nicheOverlap_", genus_name, ".csv", sep=""))
  
  # Load niche similarity between sister species pairs
  sisOverlapPd <- read.csv(paste(".//NicheOverlap_sister_", genus_tag, ".csv", sep=""))
  
  res[[j]] <- t.test(sisOverlapPd$nicheOverlap, ran[,4])
  
}

# Plot niche similarity between sister vs. random species pairs 
boxplot(sisOverlapPd$nicheOverlap, ran[,4],
        main = genus_name,
        ylab = "Climatic niche overlap",
        names = c("Sister species", "Random species")
)

# Show how many times the climate similarity between sister species is significantly different from random species.  
(sapply(res, function(x){x$p.value}) > 0.05) %>% sum



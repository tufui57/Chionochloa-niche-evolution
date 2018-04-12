
genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

# Extract probability by node number
get_BIOMOD_probability_by_nodeID <- function(i # node ID number
){
  # Species name codes
  nodeName <- pull(codes[codes$X %in% rownames(nodes)[i], ], X)
  
  prob <- (spname == nodeName) %>% pred[.]
  
  return(prob)
}

####################################################################
### Get Schoenner's D between two sipecies occurrence probability
####################################################################

# Extract probability by node number
probD <- list()

for(i in sispairs[,1]){
  
  prob1 <- get_BIOMOD_probability_by_nodeID(i)
  prob2 <- get_BIOMOD_probability_by_nodeID(allnodesister[[i]])
  
  ### Use dismo::nicheOverlap
  probD[[i]] <- nicheOverlap(prob1[[1]], prob2[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE)
}


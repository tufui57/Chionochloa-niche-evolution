genus_name = "Chionochloa"

# Create genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

reso = 5
worldclim = 1

library(dplyr)
source(".//Chionochloa niche evolution//LGM climate niche//08_1_Assess_radius_for_PersitentClimate_DataPreparation.R")


# Neighbourhood radius size should be different depending on the resolution of climate rasters
# Radius size
a = c(0.001, 0.005, seq(0.01, 0.1, by = 0.005))

res <- list()
for(i in a){
  
  cat(paste("Radius size =", i))
  # Persistent climate; climate that is shared between LGM and the present. 
  persistentClimate <- scores[scores[, paste("lgm", i, sep = "")] == 1, ]
  
  # Number of grid cells in land areas of currnet NZ 
  n.nz <- nrow(scores)
  # Number of grid cells in areas with persistent climate in current NZ
  n.persistent <- nrow(persistentClimate)
  
  ### Calculate Ratio
  res[[length(res) + 1]] <- (n.persistent/n.nz) 
  
}

# Plot percentage of area with persistent cliamte over NZ against neighbourhood radius
regression <- cbind(a, unlist(res))
png("cellSizeComparison.png")
plot(regression, xlab="Neighbourhood radius", ylab="Ratio of current areas with persistent climate")
dev.off()


# Test the persistent occurrence ratio and niche overlap between occurrences in primary open and LGM climate 
lapply(persistentdata2, function(d){
  m <- lm(d$D ~ d$persistentRatio)
  summary(m)
}
)



###########################################################################################################################
### Compare proportion of current climate with persistent climate that is calculated with different radius
###########################################################################################################################

library(dplyr)
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")

# Load persistent occurrence ratio at each radius

if(file.exists(paste(".//", genus_name, "persistentOccurrenceRatio.data", sep=""))
   ){
  load(paste(".//", genus_name, "persistentOccurrenceRatio.data", sep=""))
}else{
  source("08_2_Assess_radius_for_PersitentClimate_speciesPersistentOccurrences.R")
}
persistent <- persistentdata[["0.045"]]
persistent$persistentRatio <- as.numeric(persistent$persistentRatio)

# Load Shoenner's D
schoD <- read.csv(paste(".//PersistentOccurrences_", genus_tag, ".csv", sep=""))

persistent2 <- merge(persistent, schoD, by.x="spname", by.y="X")

persistentdata2 <- lapply(persistentdata, function(x){
  x[, "persistentRatio"] <- x[, "persistentRatio"] %>% as.character %>% as.numeric
  merge(x, schoD, by.x="spname", by.y="X")
  }
)

lapply(persistentdata2, function(d){
  m <- lm(d$D ~ d$persistentRatio)
  summary(m)
}
)


# ### Plot
# 
# # Name tag
# tag <- makeTag_separate(persistent2$spname, genus_name, "_")
# 
# png(paste(".//persistentRatio_chion_0.045_vs_schonnerD_", genus_tag, ".png", sep=""), width = 800, height = 500)
# plot(persistent2$D, persistent2$persistentRatio,
#      xlab = "Schoener's D", ylab = "Radius = 0.045",
#      main = "Proportion of species occurrences within open area with persistent climate"
# )
# text(persistent2$D, persistent2$persistentRatio, labels = tag$tag, cex = 1.2, pos=1)
# dev.off()
# 
# summary(lm(persistent2$D ~ persistent2$persistentRatio))







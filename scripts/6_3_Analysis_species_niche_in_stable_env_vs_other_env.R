
# If the "PersistentOccurrences" data was absent, run the following script again.
# "C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\scripts\\LGM climate niche\\08_get_PersitentClimate_bySchonnersD.R"

####################################################################################################
### Species niche volume in persistent environments vs. Species niche volume in other environments
####################################################################################################

stableniche <- function(genus_name){
  
  ageVolData <- read.csv(paste("Y://NicheVolume_5km_age_", genus_tag, ".csv", sep = ""))
  
  # Get species name tag
  ageVolData2 <- ageVolData[!is.na(ageVolData$spname), ]
  
  ageVolData2 <- cbind(ageVolData2, as.data.frame(makeTag_separate(ageVolData$spname, genus_name, separate = "_")[[2]])
  )
  colnames(ageVolData2)[ncol(ageVolData2)] <- "tag"
  
  pers <- read.csv(paste(".//PersistentOccurrences_", genus_name, ".csv", sep=""))
  
  sppers <- merge(ageVolData2, pers, by.x = "spname", by.y="X")
  
  sppers$nicheInUnstableEnv <- sppers$nicheVolume - sppers$D
  
  return(sppers)

}

### Get Species climatic niche volume in stable and less stable environments
genus_name = "Acaena"
source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
sppersA <- stableniche("Acaena")
genus_name = "Chionochloa"
source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
sppersC <- stableniche("Chionochloa")

####################################################################################################
### Plot
####################################################################################################

png("NicheVolume_in_stable.png")
par(mfrow=c(1,2),las = 3)
boxplot(sppersA$nicheInUnstableEnv, sppersA$D,
             main = "Acaena",
             ylab = "Species climatic niche volume",
             names = c("Stable\nenvironments", "Less stable\nenvironments")
  )
boxplot(sppersC$nicheInUnstableEnv, sppersC$D,
        main = "Chionochloa",
        ylab = "Species climatic niche volume",
        names = c("Stable\nenvironments", "Less stable\nenvironments")
)
dev.off()
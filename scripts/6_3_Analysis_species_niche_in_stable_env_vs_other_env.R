
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
  
  # Load species climatic niche volume in stable environments
  pers <- read.csv(paste(".//PersistentOccurrences_", genus_name, ".csv", sep=""))
  
  pers$X <- gsub("subsp.", "subsp", pers$X)
  
  sppers <- merge(ageVolData2, pers, by.x = "spname", by.y="X")
  
  # Species niche volume in unstable environment is defined as; (the total species niche volume) - (species niche volume in stable env)
  sppers$nicheInUnstableEnv <- sppers$nicheVolume - sppers$D
  
  return(sppers)

}

### Get Species climatic niche volume in stable and less stable environments
genus_name = "Acaena"
source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
sppersA <- stableniche("Acaena")

# Proportions of nihce volume in stable environments
mean(sppersA$D/sppersA$nicheVolume)

genus_name = "Chionochloa"
source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")
sppersC <- stableniche("Chionochloa")

# Proportions of nihce volume in stable environments
mean(sppersC$D/sppersC$nicheVolume)

# t.test(sppersC$D, sppersC$nicheInUnstableEnv)

# cannot test the proportions of niche volumes in stable environments between genera, because of sample size.
prop.test(sppersA$D/sppersA$nicheVolume, sppersC$D/sppersC$nicheVolume)


####################################################################################################
### Plot proportions of niche volume in stable environments
####################################################################################################

### Load species range size
range <- read.csv("Y://spdata_5km_Acaena.csv")
rangeA <- merge(range[, c("spname","X5kmGridCell")], sppersA, by = "spname")

rangeA$propNicheVolInStable <- sppersA$D/sppersA$nicheVolume

write.csv(rangeA, "Y://Acaena_nicheInStable.csv")

range <- read.csv("Y://spdata_5km_Chionochloa.csv")
range$spname <- gsub("subsp.","subsp", range$spname)
rangeC <- merge(range[, c("spname","X5kmGridCell")], sppersC, by = "spname")

rangeC$propNicheVolInStable <- sppersC$D/sppersC$nicheVolume

write.csv(rangeC, "Y://Chionochloa_nicheInStable.csv")

### Plots
png("Acaena_PropNicheVolume_in_stable.png")
par(mfrow=c(2,2), mar=c(2.1,6.1,2.1,1.1), cex=1)

# Proportions of niche volume in stable environments
boxplot(sppersA$D/sppersA$nicheVolume, 
        ylim=c(0,1),
        ylab = "Proportions of species niche volume\nin stable environments"
)

# Species niche volume
boxplot(sppersA$nicheVolume, ylim=c(0,1), ylab = "Species niche volume",
        main=paste("Average =",  round(mean(rangeC$nicheVolume), digits = 2 )))

par(mar=c(4.1,6.1,2.1,1.1))
# Range size vs. Proportions of niche volume in stable environments
plot(sppersA$nicheVolume, sppersA$D/sppersA$nicheVolume,
     xlab = "Species niche volume",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
)

# Niche volume vs. Proportions of niche volume in stable environments
plot(rangeA$X5kmGridCell, sppersA$D/sppersA$nicheVolume,
     xlab = "Species range size",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
     )


dev.off()





png("Chionochloa_PropNicheVolume_in_stable.png")
par(mfrow=c(2,2), mar=c(2.1,6.1,2.1,1.1), cex=1)

boxplot(sppersC$D/sppersC$nicheVolume, 
        ylim=c(0,1),
        ylab = "Proportions of niche volume\nin stable environments"
)
boxplot(sppersC$nicheVolume, ylim=c(0,1), ylab = "Species niche volume",
        main=paste("Average =",  round(mean(rangeC$nicheVolume), digits = 2 )))

par(mar=c(4.1,6.1,2.1,1.1))
plot(sppersC$nicheVolume, sppersC$D/sppersC$nicheVolume,
     xlab = "Species niche volume",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
)

plot(rangeC$X5kmGridCell,sppersC$D/sppersC$nicheVolume,
     xlab = "Species range size",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
)

dev.off()



####################################################################################################
### Plot two genera together
####################################################################################################

png("PropNicheVolume_in_stable_nicheVol.png", width = 1000, height = 500)
par(mfrow=c(1,2), las=3 , mar=c(6.1,6.1,2.1,1.1), cex=1.5)
# Proportions of niche volume in stable environments
boxplot(sppersA$nicheVolume, sppersC$nicheVolume, 
        ylim=c(0,1),
        ylab = "Species niche volume",
        names=c("Acaena", "Chionochloa")
)
# Proportions of niche volume in stable environments
boxplot(sppersA$D/sppersA$nicheVolume, sppersC$D/sppersC$nicheVolume, 
        ylim=c(0,1),
        ylab = "Proportions of species niche volume\nin stable environments",
        names=c("Acaena", "Chionochloa")
)

dev.off()

### Scatterplots
png("Scatter_PropNicheVolume_in_stable.png")
par(mfrow=c(2,1),mar=c(4.1,6.1,2.5,1.1), cex=1)
plot(sppersA$nicheVolume, sppersA$D/sppersA$nicheVolume,
     xlab = "Species niche volume",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
)
points(sppersC$nicheVolume, sppersC$D/sppersC$nicheVolume, col="red")
legend("bottomright", legend=c("Acaena", "Chionochloa"),
       col=c("black", "red"), pch=c(1,1))

smoothingSpline = smooth.spline(sppersA$nicheVolume, sppersA$D/sppersA$nicheVolume, spar=0.35)
lines(smoothingSpline)
smoothingSpline = smooth.spline(sppersC$nicheVolume, sppersC$D/sppersC$nicheVolume, spar=0.35)
lines(smoothingSpline, col="red")

plot(rangeA$X5kmGridCell, sppersA$D/sppersA$nicheVolume,
     xlab = "Species range size",
     ylab = "Proportions of species niche volume\nin stable environments",
     ylim = c(0,0.6)
)

points(rangeC$X5kmGridCell, sppersC$D/sppersC$nicheVolume, col="red")

legend("bottomright", legend=c("Acaena", "Chionochloa"),
       col=c("black", "red"), pch=c(1,1))

smoothingSpline = smooth.spline(rangeA$X5kmGridCell, sppersA$D/sppersA$nicheVolume, spar=0.35)
lines(smoothingSpline)
smoothingSpline = smooth.spline(rangeC$X5kmGridCell, sppersC$D/sppersC$nicheVolume, spar=0.35)
lines(smoothingSpline, col="red")
dev.off()



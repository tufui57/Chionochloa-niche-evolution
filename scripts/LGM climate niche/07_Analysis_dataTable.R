#################################################################################
### Generate table of data analized
#################################################################################

genus_name = "Chionochloa"
genus_tag = "chion"

### Import species age, niche volume and prevalence in persistent environment
persistentRatio <- read.csv(paste("PersistentOccurrences_", genus_tag, ".csv", sep = ""))
persistentRatio$X <- as.character(persistentRatio$X)
speciesAge <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

colnames(persistentRatio)[1] <- "spname" 
persistence <- merge(persistentRatio, speciesAge[!is.na(speciesAge$spname),], by = "spname", all.y = T)
colnames(persistence)[colnames(persistence)=="D"] <- "persistentNiche"

### Count the number of occurrence cells

dat <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv", sep=""))
spname <- colnames(dat)[grep(paste("^",genus_name, sep=""), colnames(dat))]

occ <- cbind(spname, colSums(dat[,spname], na.rm = T))
persistence2 <- merge(persistence, occ, all.x = T)
colnames(persistence2)[ncol(persistence2)] <- "occ"

### Save the table

write.csv(persistence2[, c("spname", "occ", "nodeID", "persistentNiche", "nicheVolume", "speciesAge")], 
          file = paste("Analysis_", genus_tag, ".csv", sep = "")
          )

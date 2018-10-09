
load( paste("Y:\\Scores_", genus_name,"_landcover_worldclim1_", reso, "km.data", sep = ""
))

# Subset data of the genus
tes <- rowSums(scores[, colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))]], na.rm = TRUE) > 0
genus <- scores[tes, ]

hist(genus$PC1)
hist(genus$PC2)

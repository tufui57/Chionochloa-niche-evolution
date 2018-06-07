########################################################
### Niche overlap between genera
########################################################

genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

load(".//Scores_chion.data")
scores.c <- scores

noOccRow.c <- scores.c[, grepl("^Chionochloa", colnames(scores.c))] %>% rowSums(., na.rm = T) != 0
scores.c.occ <- scores.c[noOccRow.c, ]


load(".//Scores_acaena.data")
scores.a <- scores

noOccRow.a <- scores.a[, grepl("^Acaena", colnames(scores.a))] %>% rowSums(., na.rm = T) != 0
scores.a.occ <- scores.a[noOccRow.a, ]


######################### NOTE ##################################################
# If species has occurrence point with the max values of background's axes,  
# ecospat.grid.clim.dyn() gives the following error;
# Error in quantile.default(spr, th.env) : 
# missing values and NaN's not allowed if 'na.rm' is FALSE

# Get rid of the points from species occurrences

removeThisRow <- which(scores.c.occ$PC2 == max(scores.c.occ$PC2))
scores.c.occ2 <- scores.c.occ[-removeThisRow,]

### Schoenner's D between genera
scho <- SchoenerD_ecospat(scores, "PC1", "PC2", scores.a.occ, scores.c.occ2)


########################################################################################
### Quantification of niche volume
########################################################################################

### Original method in ecospat to calculate Schonner's D
library(ecospat)

SchoenerD <- function(scores, gen1, gen2) {
  
  # Extract data of two target species
  scores.gen1 <-  scores[scores[, gen1] == 1,c("PC1", "PC2")]
  scores.gen2 <-  scores[scores[, gen2] == 1,c("PC1", "PC2")]
  scores.clim <- scores[, c("PC1", "PC2")]
  
  # calculation of occurence density and test of niche equivalency and similarity
  z1 <- ecospat.grid.clim.dyn(scores.clim, scores.clim, scores.gen1, R = 100)
  z2 <- ecospat.grid.clim.dyn(scores.clim, scores.clim, scores.gen2, R = 100)
  
  res <- list()
  ## Schoener D
  res[[1]] <- unlist(ecospat.niche.overlap(z1, z2, cor = T))
  res[[2]] <- unlist(ecospat.niche.overlap(z1, z2, cor = F))
  # Name
  names(res) <- c(paste(gen1, "corrected"), paste(gen1, "not corrected"))
  
  return(res)
}

########################################################################################
### Chionochloa
########################################################################################

# Data import
da1 <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- da1[is.na(da1$bioclim1) == F, ]

# Create species distriobution having full occurrences across NZ
d$chion_allNZ <- 1

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]
# Species that doesn't have < 5 records can't be used in this analysis.
sname2 <- sname[!(sapply(sname, function(i){sum(d[,i]) < 5}))]

# Replace NA to 0
for(i in sname){
  d[is.na(d[,i]),i] <- 0
}

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], sname, "chion_allNZ", "x", "y")], pca$x[, 1:2])

D <- lapply(sname2, SchoenerD, scores = scores, gen2 = "chion_allNZ")
D2 <- unlist(D)

# Collate data
d2 <- D2[grepl("corrected.D$", names(D2))]
d3 <- d2[seq(2,66,2)]
d2.2 <- d2[seq(1,65,2)]
d4 <- D2[grepl("corrected.I$", names(D2))]
d5 <- d4[seq(2,66,2)]
d4.2 <- d4[seq(1,65,2)]
d6 <- data.frame(cbind(d2.2,d3,d4.2,d5))
colnames(d6) <- c("corrected.D","not corrected.D","corrected.I","not corrected.I")
rownames(d6) <- sname2
write.csv(d6, "Y:\\Chionochloa_nicheVolume.csv")

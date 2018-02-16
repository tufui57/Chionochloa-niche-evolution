
library(adehabitatMA)
library(adehabitatHR)
library(raster)

# Data import
alld <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

# Replace NA with 0
for(i in sname){d[is.na(d[,i]),i] <- 0}

# Species that doesn't have < 5 records can't be used in this analysis.
sname2 <- sname[!(sapply(sname, function(i){sum(d[,i]) < 5}))]

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)

scores <- data.frame(d[, c(paste("bioclim", c(1, 6, 12, 15), sep = ""), "x", "y", sname2)], pca$x[, 1:2])

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
  name_genera <- paste(strsplit(gen1, ".csv"),strsplit(gen2, ".csv"), sep="_")
  names(res) <- c(paste(name_genera, "corrected"), paste(name_genera, "not corrected"))
  
  return(res)
}

com <- combn(sname2,2)

result <- list()

for(i in 1:ncol(com)){
  
  gen1=com[,i][1]
  gen2=com[,i][2]
  
  result[[i]] <- SchoenerD(scores, gen1, gen2)
  
}

dat <-list()
for(i in 1:length(result)){
  x <- data.frame(result[[i]])
  colnames(x) <- c("ecospat.corrected", "ecospat.uncorrected")
  dat[[i]] <- x[1,]
}
names(dat)<-sapply(result, function(x){
  a <- gsub("_pa", "", colnames(x)[1])
  gsub(".corrected","", a)
  }
  )


write.csv(do.call(rbind, dat), "Y://schoennerD_Chionochloa.csv")

chion <- read.csv("Y://schoennerD_Chionochloa.csv")
c1 <- data.frame(as.character(t(com[1,])))
c2 <- data.frame(as.character(t(com[2,])))
chion2 <- cbind(c1, c2, chion[,-1])

colnames(chion2)[1:2] <- c("spname1", "spname2")

write.csv(chion2,"Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\schoennerD_Chionochloa.csv")

#########################################################
## Calculate Schoenner's D between all species pairs
#########################################################

library(ecospat)
library(nichePlot)

# Data import
alld <- read.csv("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

# Replace NA with 0
for(i in sname){
  d[is.na(d[,i]),i] <- 0
  }

# Species that doesn't have < 5 records can't be used in this analysis.
spLessThan5 <- sapply(sname, 
       function(i){sum(d[,i]) < 5}
)
spname <- sname[!(spLessThan5)]

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)

scores <- data.frame(d[, c(paste("bioclim", c(1, 6, 12, 15), sep = ""), "x", "y", spname)], 
                     pca$x[, 1:2]
                     )

save(scores, file = ".//Scores_chion.data")


### Claculate Schoenner's D for all combinations of species

# Combination of species

sname2 <- sname[sname %in% colnames(scores)]
  
com <- combn(sname2, 2)

result <- list()

for(i in 1:ncol(com)){

  # Species names
  gen1 = com[,i][1]
  gen2 = com[,i][2]
  
  # Species data
  scores1 <- scores[scores[, gen1] == 1,]
  scores2 <- scores[scores[, gen2] == 1,]

  # Schoner's D
  result[[i]] <- SchoenerD_ecospat(scores, "PC1", "PC2", scores1, scores2)

}

# Collate data frame
dat <-list()
for(i in 1:length(result)){
  x <- data.frame(result[[i]])
  colnames(x) <- c("ecospat.corrected", "ecospat.uncorrected")
  dat[[i]] <- x[1,]
}

# Name data
for(i in 1:ncol(com)){
  
  # Species names
  gen1 = com[,i][1]
  gen2 = com[,i][2]
  
  names(dat)[i] <- paste(gen1, gen2)
}

chion <- do.call(rbind, dat)
c1 <- data.frame(as.character(t(com[1,])))
c2 <- data.frame(as.character(t(com[2,])))
chion2 <- cbind(c1, c2, chion)

colnames(chion2)[1:2] <- c("spname1", "spname2")

write.csv(chion2,"Y:\\schoennerD_Chionochloa.csv")



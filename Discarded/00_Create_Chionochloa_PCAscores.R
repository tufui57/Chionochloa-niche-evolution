#########################################################
## Create PCA axes
#########################################################

# Data import
alld <- read.csv("Y:\\Acaena project\\Chionochloa_bioclim_landcover_history_worldclim1_1km_24sep.csv")
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

save(scores, file = ".//Scores_chion_24sep.data")

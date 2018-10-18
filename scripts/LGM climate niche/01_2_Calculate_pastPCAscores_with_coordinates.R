###############################################################
###   Get PCA scores of LGM climate and coordinates
###############################################################

# Load LGM climate data
load(".//LGMclimate.data")

# Get coordinates and bioclim variables of the raster
lgm.scores <- cbind(coordinates(lgm.mainland[[1]]), sapply(lgm.mainland, values)) %>% data.frame
lgm.scores <- lgm.scores[!is.na(lgm.scores$bi1),]


# get env. corrdinates (PCA axes)
pca <- prcomp(lgm.scores[, paste("bi", c(1,6,12,15), sep = "")],
              center = TRUE,
              scale. = TRUE,
              retx = TRUE
)

# Bind the scores with coodinates and bioclim variables
scores.lgm <- data.frame(lgm.scores, pca$x[, 1:2])

# Save the scores
save(scores.lgm, file = ".\\Scores_LGM_mainisland_worldclim1_5km.data")

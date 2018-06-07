################################################################################
###   Calculate values of original variables from PC values
################################################################################

### Climate data import

da1 <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
d <- da1[is.na(da1$landCoverChange) == F, ]

spname <- grepl(genus_name, colnames(d)) %>% colnames(d)[.]

for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

### Convert climate values to PC axes values

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", vars, sep = "")],
              center = TRUE,
              scale. = TRUE,
              retx = TRUE
)

### Convert PC vales to climate values

# PC values
head(pca$x)
# Original values
head(d[, paste("bioclim", vars, sep = "")])

# Reverse the PC values to original ones
t(t(pca$x %*% t(pca$rotation)) * pca$scale + pca$center) %>% head

### How different original values would be if PC1 + 0.01?
# Extract first row
pc <- pca$x[1:2,]
pc.added <- cbind(pc[,1] + 0.01, pc[, -1])

# Compare
t(t((pc.added) %*% t(pca$rotation)) * pca$scale + pca$center) %>% head
t(t(pca$x[1:2,] %*% t(pca$rotation)) * pca$scale + pca$center)


pc.added2 <- cbind(pc[, 1], pc[,2] + 0.01, pc[, 3:4])

# Compare
t(t((pc.added2) %*% t(pca$rotation)) * pca$scale + pca$center) %>% head
t(t(pca$x[1:2,] %*% t(pca$rotation)) * pca$scale + pca$center)

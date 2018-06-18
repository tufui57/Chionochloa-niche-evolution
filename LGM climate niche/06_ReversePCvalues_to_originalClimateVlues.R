################################################################################
###   Calculate values of original variables from PC values
################################################################################

################################################################################
# Before run this script, set genus (genus_name) and neighbourhood cell size (a)
################################################################################

### Climate data import

da1 <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim",
                                        Worldclim, "_", reso, "km.csv", sep=""
))
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

### How different original values would be if PC1 + a(cell size)?
# Extract first row
pc <- pca$x[1:2,]
pc.added <- cbind(pc[,1] + a, pc[, -1])

print("original bioclim values")
print(t(t(pca$x[1:2,] %*% t(pca$rotation)) * pca$scale + pca$center))

### PC1
# Compare
print(paste("bioclim variables as consequnse of PC1 +", a))
print(t(t((pc.added) %*% t(pca$rotation)) * pca$scale + pca$center))
print("The difference between bioclim values")
dif <- (t(t((pc.added) %*% t(pca$rotation)) * pca$scale + pca$center)- t(t(pca$x[1:2,] %*% t(pca$rotation)) * pca$scale + pca$center))
print(dif)

### PC2
pc.added2 <- cbind(pc[, 1], pc[,2] + a, pc[, 3:4])

# Compare
print(paste("bioclim variables as consequnse of PC2 +", a))
print(t(t((pc.added2) %*% t(pca$rotation)) * pca$scale + pca$center))
print("The difference between bioclim values")
dif2 <- (t(t((pc.added2) %*% t(pca$rotation)) * pca$scale + pca$center)- t(t(pca$x[1:2,] %*% t(pca$rotation)) * pca$scale + pca$center))
print(dif2)



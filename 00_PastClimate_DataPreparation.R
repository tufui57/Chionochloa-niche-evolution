library(raster)
library(dplyr)

################################################################################
### Climatic niche 
### in Last Glacial Maximum(22,000 years ago), Last inter-glacial(120,000 - 140,000 years ago), Mid Holocene(6000 years ago)
################################################################################

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m"
  #"ccmidbi_30s"

## Data import
pathbio <- paste("Y:\\Niche change of lineages\\WORLDCLIM\\", time, sep = "")

files <- list.files(pathbio)

### Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"

# If the raster file is tif file
if(sum(grepl("tif$", files)) > 0){
  ext <- ".tif"
}

# If the raster file is bil file
if(sum(grepl("bil$", files)) > 0){
  ext <- ".bil"
}

string <- paste("bi", c(1,6,12,15), ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
  gsub('.{1}$', '', .)

if(time == "lig_30s_bio"){
  string <- paste("bio_", c(1,6,12,15), ext, "$|", sep = "") %>% paste(., sep = "", collapse = "") %>% 
    gsub('.{1}$', '', .) # Remove last letter
}

### Note! The order of variables is automatically changed into alphabetically and numerically.
rasters <- lapply(paste(pathbio, files[grepl(string, files)], sep = "\\"),
                  raster
)

rasters <- rasters[c(1,4,2,3)]

# Load reference raster for NZ long-lat
nz <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")

# Resample
#nzras <- lapply(rasters, resample, nz)
nzras2 <- lapply(rasters, crop, extent(c(160,185), c(-50,-30)))

lgmdata <- lapply(nzras2, values) %>% do.call(cbind, .)

lgmdata2 <- lgmdata[!is.na(lgmdata[, 1]), ]

### Get PCA axes of current climate
# Data import
alld <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

# Replace NA with 0
for(i in sname){d[is.na(d[,i]),i] <- 0}

# Get PC axes
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE,
              retx = TRUE
              )

scores <- data.frame(pca$x[, 1:2])



# Center and scale the data
c.fun <- function(df, center, scale) {
  return((df-center)/scale )
}

centeredData <- apply(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
                      MARGIN = 1, FUN = c.fun, pca$center, pca$scale)

# compute the principal components
pcs <- t(pca$rotation) %*% centeredData

# compare with results of prcom (option retx=TRUE gives ^cs in x)
head(t(pcs))
head(pca$x)
# check if results are the same
sum(abs(t(pcs)-pca$x))

# If you want to compute the components for new data, then you do:
newdata <- data.frame(lgmdata2)
centeredNewData <- apply(newdata, MARGIN = 1, FUN = c.fun, pca$center, pca$scale)
pcsnew <- t(pca$rotation) %*% centeredNewData

newdf <- data.frame(t(pcsnew))
colnames(newdf) <- paste("PC", 1:4, sep = "")

### Plot
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

png(paste(time, ".png", sep = ""))
plot(scores, xlim = extent_x, ylim = extent_y)
title(time)
points(newdf[,1:2], col = "red")
dev.off()

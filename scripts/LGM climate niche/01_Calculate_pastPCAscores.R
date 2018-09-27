###################################################################################
#############   Prepare past climate data   #######################################
###################################################################################

# Choose past data. Interglacial; "lig_30s_bio", 6k year ago; "ccmidbi_30s"
time <- "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)

# Resolution of rasters
reso = 1

# Worldclim version
Worldclim <- 1

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

# Choose genus
genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//LGM climate niche//00_PastDataPreparation.R")

#####################################
# 1. Crop & project past rasters
#####################################
# Terrestrial areas in the past is different from the current NZ.
# Thus, current climate rasters need to be cropped by a polygon of the largest island of Zealandia in LGM.
# crop() can't crop unneccesary polygons out, because the polygons are within the same extent as the largest island of LGM.

# Reference raster of coordinate system & extent
# This raster mustn't be used for resampling for 5km resolution.
ref.extent <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411nztm\\bio1_411.bil")
ref5 <- raster(paste("Y:\\GIS map and Climate data\\current_landcover", reso, "km.bil", sep=""))
ref5.2 <- extend(ref5, ref.extent) 

# Import current outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of current polygon to decrease data size
nzland2 <- crop(nzland, ref.extent)

if(reso==1){
  # Project -> resample
  nzras <- lapply(rasters, projectRaster, ref.extent) %>% lapply(., resample, ref.extent) 
}

if(reso==5){
  # Project past climate rasters
  nzras <- lapply(rasters, projectRaster, ref5.2)
}

# Crop rasters
# Past terrestrial area of Zealandia is different from the one of current NZ
nzras2 <- lapply(nzras, crop, 
                 extent(extent(nzland2)[1] - 300000, extent(nzland2)[2] + 300000, 
                        extent(nzland2)[3] - 200000, extent(nzland2)[4] + 200000)
                 )

# Check extent and coordinates of the LGM raster 
ras.lgm <- nzras2[[1]]
values(ras.lgm)[!is.na(values(ras.lgm))] <- 0

# plot(ras.lgm, col = "lightpink3")
# plot(nzland2, add = TRUE)

######################################
# 2. Polygonize past climate raster
######################################

### Convert raster to polygon in order to extract polygon of the largest island of LGM
# Takes a few min
poly.lgm <- rasterToPolygons(ras.lgm, dissolve = TRUE)

# Convert SpatialPolygonDataFrame to SpatialPolygon
spPoly.lgm <- SpatialPolygons(poly.lgm@polygons,
                      proj4string = poly.lgm@proj4string)
# Split a multipart polygon to sigle part polygons
split.poly <- disaggregate(spPoly.lgm)

##########################################################
# 3. Extract past climate rasters by past polygons
##########################################################

### Calculate area of each polygon to find the largest island of LGM

areas <- sapply(slot(split.poly, "polygons"), function(x){
  sapply(slot(x, "Polygons"), slot, "area")}
  )
land.areas <- lapply(areas, max) %>% unlist
max.area <- (land.areas == max(land.areas)) %>% which

# Plot single polygon
plot(split.poly[max.area,])

# Clip rasters by polygon
clip.by.polygon <- function(raster, shape) {
  
  a1_crop<-crop(raster, shape)
  
  step1<-rasterize(shape, a1_crop)
  
  a1_crop*step1
}

lgm.mainland <- lapply(nzras2, clip.by.polygon, shape = split.poly[max.area,])

#############################################################
# 2. Calculate PC axis values from past climate raster
#############################################################

lgm.data <- lapply(lgm.mainland, values) %>% as.data.frame %>% cbind
colnames(lgm.data) <- paste("bioclim", vars, sep = "")
lgm.data2 <- lgm.data[!(is.na(lgm.data$bioclim1)), ]


########################################
### Get PCA axes from current climate
########################################

# Data import
da1 <- read.csv(paste("Y://", genus_name, "_bioclim_landcover_history_worldclim",
                      Worldclim, "_", reso, "km.csv", sep=""
))
                      
d <- da1[is.na(da1$landCoverChange) == F, ]

spname <- grepl(genus_name, colnames(d)) %>% colnames(d)[.]

for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", vars, sep = "")],
              center = TRUE,
              scale. = TRUE,
              retx = TRUE
)

####################################################
### Calculate PC values for past climate
####################################################

# Center and scale the data
c.fun <- function(df, center, scale) {
  return((df-center)/scale )
}

centeredData <- apply(d[, paste("bioclim", vars, sep = "")],
                      MARGIN = 1, FUN = c.fun, pca$center, pca$scale)

# compute the principal components
pcs <- t(pca$rotation) %*% centeredData


centeredNewData <- apply(lgm.data2, MARGIN=1, FUN=c.fun, pca$center, pca$scale)
pcsnew <- t(pca$rotation) %*% centeredNewData
newdf <- data.frame(t(pcsnew))

if(file.exists(paste(".\\LGM_mainisland_worldclim",
                     Worldclim, "_", reso, "km_scores.data", sep = "")
) == FALSE){
  save(newdf, file = paste(".\\LGM_mainisland_worldclim",
                         Worldclim, "_", reso, "km_scores.data", sep = ""))
}

plot(newdf[,1:2], col="red")
points(pca$x)

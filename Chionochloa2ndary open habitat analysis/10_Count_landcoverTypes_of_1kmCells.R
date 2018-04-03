##############################################################################
#### Assign occurrence records to administrative districts (region)
##############################################################################
library(raster)
library(rgdal)
library(dismo)
library(maptools)
library(dplyr)

# Load BIOCLIM data
load("Y:\\GIS map and Climate data\\Acaena_Bioclim_landcover_1km_NZTM.data")

# Read shp file of NZ outline for proj4string (projection code) of NZTM
LAYERS <- ogrListLayers("Y://GIS map and Climate data//lds-nz-coastlines-and-islands-polygons-topo-150k-SHP")
n <- readOGR("Y://GIS map and Climate data//lds-nz-coastlines-and-islands-polygons-topo-150k-SHP", LAYERS)

# Rasterize polygon
rasterizePolygonShapeFile <- function(layerPath, targetShapePath, rasterPath) {
    
  ### Read shp file of district border
    layers <- ogrListLayers(layerPath)
    nzland <- readOGR(targetShapePath, layers)

    # Rasterize polygon
    rast <- raster(ncol = ncol(bio_land), nrow = nrow(bio_land))
    extent(rast) <- extent(bio_land)
    pol <- rasterize(nzland, rast)

    plot(pol, col = rainbow(14))
    writeRaster(pol, filename = rasterPath, format = "EHdr")
}

rasterizePolygonShapeFile(
  "Y://GIS map and Climate data//NZ boundaries//nz-regional-councils-2012-yearly-pattern//nz-regional-councils-2012-yearly-pattern.shp",
  "Y://GIS map and Climate data//NZ boundaries//nz-regional-councils-2012-yearly-pattern//nz-regional-councils-2012-yearly-pattern.shp",
  "Y:/regionRaster.bil")

### Cut buffer areas
# The result raster of the above function includes buffer areas which is out of NZ land area. This buffer must be removed.

pol <- raster("Y://regionRaster.bil")
# Extract cells on land area using bioclim 1km raster. Bioclim raster has no values on cells which aren't on land area.
# Thus, cells with values of bioclim are the cells on land.
bio1 <- bio_land[[1]]
polv <- data.frame(pol[], bio1[])

### Make raster from the above data frame in order to check whether the region column is assigned right.
polv_2 <- polv 
polv_2[is.na(polv_2[, 2]), 1] <- rep(NA, sum(is.na(polv_2[, 2])))

# Rasterize data frame of regions without buffer
rast <- raster(ncol = ncol(bio_land), nrow = nrow(bio_land))
extent(rast) <- extent(bio_land)
proj4string(rast) <- proj4string(n)
rast[] <- polv_2[, 1]
plot(rast)
writeRaster(rast, filename = "Y://regionRaster_noBuffer.bil", format = "EHdr")

# Prepare data frame of pre-human/current landcover
rast2 <- stack(rast, bio_land)
res <- data.frame(cbind(coordinates(rast[[1]]), values(rast2)))
res2 <- res[is.na(res$bioclim1) == F,]

write.csv(res2, "Y:\\alldata_Acaena.csv")


############################################################
### Get the table of district ID number and names in GIS
############################################################

#plot(raster("Y:\\regionRaster_noBuffer.bil"), col = rainbow(17))
distname <- read.csv("Y://GIS map and Climate data//NZ_district_names.csv")
distname <- distname[1:17,1:3]

####################################
### Summary of landcover data for each district
####################################
al1 <- read.csv("Y:\\alldata_Acaena.csv")

al1 <- al1[, c("layer", "pre.human_landcover1km", "current_landcover1km")]
colnames(al1)[1] <- "region"

# Display cell numbers of each district
t1 <- by(al1$pre.human_landcover1km, al1$region, length)
cbind(distname[order(distname[,2]), 2:3], do.call(rbind, as.list(t1)))

# subset data by districts
d1 <- split(al1, al1$region)
names(d1) <- distname[order(distname[1:17, 2]), 3]

# sprehuman landcover within district
currentByDist <- lapply(d1, function(x){
    a <- aggregate(x$current_landcover1km, by = list(current = x$current_landcover1km), FUN = length)
    return(a)
}
)


# Function to bind all species table into one table
cbind.fill4column <- function(nm){ 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  
  newmat <- matrix(NA, n, length(nm))
  
  rownames(newmat) <- rownames(nm[[which(sapply(nm, nrow)==n)[1]]])
  colnames(newmat) <- names(nm)
  
  for(j in 1:length(nm)){
    for(i in 1:nrow(nm[[j]])){
      newmat[which(rownames(nm[[j]])[i] == rownames(newmat)), j] <- nm[[j]][i,]
    }
  }
  return(newmat)
}

# Create summary table of landcover data in district
dd <- lapply(currentByDist, function(x) {
    a <- data.frame(x[, 2])
    rownames(a) <- as.character(x$current)
    return(a)
}
)
d <- data.frame(cbind.fill4column(dd))
d$Nisland <- as.numeric(rowSums(d[, 3:11], na.rm = T))
d$Sisland <- as.numeric(rowSums(d[, c(1, 2, 12:17)], na.rm = T))
d$total <- as.numeric(rowSums(d[, 1:17], na.rm = T))

# Convert landcover ID to name
convert_current_landCover <- function(x){
  ifelse(x %in% c(54,69), "nativeForest",
         ifelse(x %in% c(64,68,71), "exoticForest",
                ifelse(x %in% c(15,40,41,43,44,10,12,16,47,50,51,52,55,56,58), "nonForest", # incl. shrubland and gravely habita
                       ifelse(x %in% c(0,1,2,5,6,14,20,21,22,30,33,45,46), "nonPotentialAcaenaHabitat",
                              NA
                       ))))
}


d$landcover <- convert_current_landCover(rownames(d))

nOpen <- d[d$landcover == "nonForest", "Nisland"] %>% sum
sOpen <- d[d$landcover == "nonForest", "Sisland"] %>% sum
totalOpen <- d[d$landcover == "nonForest", "total"] %>% sum
sOpen/totalOpen

write.csv(d, file = "Y://RegionSummary_currentLandcover.csv")

##############################################################################
#### Count occurrence records and cells in North/South islands
##############################################################################

library(raster)
library(rgdal)
library(dismo)
library(maptools)

# extract points in N/S island raster
path = "Y:\\GIS map and Climate data\\North.shp"
LAYERS <- ogrListLayers(path)
nland <- readOGR(path, LAYERS)

path = "Y:\\GIS map and Climate data\\South.shp"
LAYERS <- ogrListLayers(path)
sland <- readOGR(path, LAYERS)


#######################################################
### Create raster from points of occurrence records
#######################################################

genus_name = "Chionochloa"

source(".\\functions\\F02_clean_up_species_records18Sep.R")

# Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Import reference raster of WGS
refWGS <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")

# Import function
source(".//functions//F04_convert_occurrencePoints_to_raster.R")

ref.raster <- raster(
  # This raster was converted from pre-human raster with ArcGIS using "majority" method for raster value assignment
  "Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil"
)

spRaster <- lapply(dat2, project_and_convert_occurrencePoints_to_raster, refWGS = ref.raster, val = "occurrence")



### Clip occurrence record rasters by polygons

# Rasterize polygon

# The raster to crop polygon must has values
cr <- crop(spRaster[[3]], extent(nland))
n.ras_polygon <- rasterize(nland, cr)  
# replace all non-NA values to 1
n.ras_polygon[!is.na(n.ras_polygon[])] <- 1

# Clip by the rasterized polygon
n.occ <- lapply(spRaster, function(x){
  
  cr <- crop(x, extent(nland))
  res <- cr*n.ras_polygon
  }
  )
# Count number of occurrences in North island
nn <- lapply(n.occ, function(x){sum(x[], na.rm=T)})



# The raster to crop polygon must has values
cr <- crop(spRaster[[3]], extent(sland))
s.ras_polygon <- rasterize(sland, cr)  
# replace all non-NA values to 1
s.ras_polygon[!is.na(s.ras_polygon[])] <- 1

# Clip by the rasterized polygon
s.occ <- lapply(spRaster, function(x){
  
  cr <- crop(x, extent(sland))
  res <- cr*s.ras_polygon
}
)
# Count number of occurrences in North island
sn <- lapply(s.occ, function(x){sum(x[], na.rm=T)})

alln <- lapply(spRaster, function(x){sum(x[], na.rm=T)})


table.occ <- cbind(unlist(nn), unlist(sn), unlist(alln))
rownames(rr) <- spname
colnames(rr) <- c("N", "S", "all")
write.csv(rr, "Y://count_species_occ_NS_island.csv")


##############################################################################
#### Count cells in each habitat of N/S island
##############################################################################

dat <- read.csv("Y:\\Chionochloa_bioclim_landcover_history_worldclim1_1km_24sep.csv")

# summary no. of cells depending on land use history
table(dat$change)

# refernce raster
bio15 <- raster("Y:\\ASCII_files\\bioclim15NZTM.asc")

### Crop raster of land use change by N/S island

points <- dat[, c("NZTMlon", "NZTMlat")]
points$change <- ifelse(dat$change=="nonF-nonF", 1,
                        ifelse(dat$change=="NF-nonF", 2,
                               ifelse(dat$change=="NF-NF",3,
                                      ifelse(dat$change=="nonF-NF",4,
                                             ifelse(dat$change=="nonF-EF",5,
                                                    ifelse(dat$change=="NF-EF",6,
                                                           ifelse(dat$change=="nonF-nonPotentialHabitat",7,
                                                                  8)))))))

# point coordinate setting
coordinates(points) <- dat[, c("NZTMlon", "NZTMlat")]
proj4string(points) <- proj4string(bio15)
# rasterize points
pr <- rasterize(points, bio15, field = points$change, fun = max, background = NA)

###############
### N island
###############
# crop necessary extent of raster
pr_crop <- crop(pr, nland)
# rasterize polygon
rnland <- rasterize(nland, pr_crop)
# replace all non-NA values to 1
rnland[!is.na(rnland[])] <- 1
# raster(0,1 inside of polygon extent, but still has S island raster)*polygon(all 1 inside N island)
resN <- pr_crop*rnland

###############
### S island
###############
# crop necessary extent of raster
pr_crop <- crop(pr, sland)
# rasterize polygon
rsland <- rasterize(sland, pr_crop)
# replace all non-NA values to 1
rsland[!is.na(rsland[])] <- 1
# raster(0,1 inside of polygon extent, but still has S island raster)*polygon(all 1 inside N island)
resS <- pr_crop*rsland



# save
nz <- table(dat$change)
t <- cbind(table(resN[]), table(resS[]))
t<-t[-1,]
t2 <- cbind(nz[c(7,3,2,6,5,1,8,4)], t)
colnames(t2)<-c("NZ","N","S")
write.csv(t2, "Y://count_cells_NZandNS_island.csv")



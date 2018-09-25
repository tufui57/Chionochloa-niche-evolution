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
ras.nland <- lapply(spRaster, mask, mask = nland)
nn <- lapply(ras.nland, function(x){sum(x[], na.rm=T)})

ras.sland <- lapply(spRaster, mask, mask = sland)
sn <- lapply(ras.sland, function(x){sum(x[], na.rm=T)})

alln <- lapply(spRaster, function(x){sum(x[], na.rm=T)})


table.occ <- cbind(unlist(nn), unlist(sn), unlist(alln))
rownames(rr) <- gsub(".asc","", f[grep("^Acaena.*asc$", f)])
colnames(rr) <- c("N","S")
write.csv(rr, "Y://count_species_occ_NS_island.csv")


##############################################################################
#### Count cells in each habitat of N/S island
##############################################################################

da1 <- read.csv("Y:\\2nd category\\alldata_Acaena1kmGrid_inclNAonland_2ndCategory.csv")

# summary no. of cells depending on land use history
table(da1$change)

# refernce raster
bio15 <- raster("Y:\\ASCII_files\\bioclim15NZTM.asc")

### Crop raster of land use change by N/S island

points <- da1[, c("NZTMlon", "NZTMlat")]
points$change <- ifelse(da1$change=="nonF-nonF", 1,
                        ifelse(da1$change=="NF-nonF", 2,
                               ifelse(da1$change=="NF-NF",3,
                                      ifelse(da1$change=="nonF-NF",4,
                                             ifelse(da1$change=="nonF-EF",5,
                                                    ifelse(da1$change=="NF-EF",6,
                                                           ifelse(da1$change=="nonF-nonPotentialHabitat",7,
                                                                  8)))))))

# point coordinate setting
coordinates(points) <- da1[, c("NZTMlon", "NZTMlat")]
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
nz <- table(da1$change)
t <- cbind(table(resN[]), table(resS[]))
t<-t[-1,]
t2 <- cbind(nz[c(7,3,2,6,5,1,8,4)], t)
colnames(t2)<-c("NZ","N","S")
write.csv(t2, "Y://count_cells_NZandNS_island.csv")



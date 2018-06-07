########################################
### Prepare past climate data
########################################

# Choose past data. Interglacial; "lig_30s_bio", 6k year ago; "ccmidbi_30s"
time <- "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

# Choose genus
genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_PastDataPreparation.R")
#############################
# Filter polygons
#############################
# crop() can't crop unneccesary polygons out, because the polygons are within the same extent as the main island

# Resample raster
nz <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411nztm\\bio1_411.bil")

# Project -> resample
nzras <- lapply(rasters, projectRaster, nz) %>% lapply(., resample, nz) 

# Crop rasters
# Past terrestrial area of Zealandia is different from the one of current NZ
nzras2 <- lapply(nzras, crop, 
                 extent(extent(nzland2)[1] - 300000, extent(nzland2)[2] + 300000, 
                        extent(nzland2)[3] - 200000, extent(nzland2)[4] + 200000)
)

#############################
# Prepare raster data
#############################
ras.lgm <- nzras2[[1]]
values(ras.lgm)[!is.na(values(ras.lgm))] <- 0

# Check extent and coordinates of the LGM raster
plot(ras.lgm, col = "lightpink3")
plot(nzland2, add = TRUE)

### Convert raster to polygon
# Takes a few min
poly.lgm <- rasterToPolygons(ras.lgm, dissolve = TRUE)

# writeOGR(poly.lgm, "test", layer = "poly.lgm", driver="ESRI Shapefile")
# 
# lgm.shp <- readOGR("test.shp")



# Convert SpatialPolygonDataFrame to SpatialPolygon
spPoly.lgm <- SpatialPolygons(poly.lgm@polygons,
                      proj4string = poly.lgm@proj4string)
# Split a multipart polygon to sigle part polygons
split.poly <- disaggregate(spPoly.lgm)

# Plot single polygon
plot(split.poly[1,])

### Calculate area of each polygon
sapply(slot(split.poly, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area")) %>% 
  unlist %>% sum

sapply(slot(nzland2, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area")) %>% 
  unlist %>% sum




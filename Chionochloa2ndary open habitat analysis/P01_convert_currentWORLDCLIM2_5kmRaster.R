
library(raster)
library(rgdal)
library(SDMTools)

# Give genus name
genus_name <- "Acaena"

# Give raster resolution (km)
reso <- 5

# Give path of WORLDCLIM raster
path <- "Y:\\GIS map and Climate data\\worldclim\\wc2.0_2.5m_bio"


# ############################################################################################################
# # Resample pre-human land cover raster from resolution 100m to 5km
# ############################################################################################################
# ### Create a reference raster with 1km2 resolution and NZTM Geographic Coordinate System (GCS)
# # Import (100m)^2 raster data
# pre <- raster("Y:\\GIS map and Climate data\\lris-new-zealand-potential-vegetation-grid-version-GTiff\\new-zealand-potential-vegetation-grid-version.tif")
# # WARNING; this reference raster doesn't have enough values when you run this code in computers with insufficient disc space.
# # But it works just for reference.
# ref.raster <- aggregate(pre, fun = modal, na.rm = T, fact = reso*10)
# 
# writeRaster(ref.raster, paste("Y:\\GIS map and Climate data\\pre-human_landcover", reso, "km.bil", sep=""), format = "EHdr")

ref.raster <- raster(
  paste("Y:\\GIS map and Climate data\\pre-human_landcover", reso,"km.bil", sep=""
        )
  )

############################################################################################################
# Convert WORLDCLIM tiff to raster
# Project coordinate system of bioclim rasters from WGS84 to NZTM
############################################################################################################

proj4stringNZTM <- proj4string(current_ras)

source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F01_Change_resolutionOfWORLDCLIM.R")

############################################################################################################
# Resample current land cover polygon to raster at resolution 5km
############################################################################################################

# # Import current land cover polygon
# lay <- ogrListLayers("Y:\\GIS map and Climate data\\lris-lcdb-v41-land-cover-database-version-41-mainland-new-zealand-SHP")
# current <- readOGR("Y:\\GIS map and Climate data\\lris-lcdb-v41-land-cover-database-version-41-mainland-new-zealand-SHP", lay)
# 
pre <- ref.raster
# current_ras <- rasterize(current, pre, "Class_2012")
# 
# classnames <- cbind(unique(current$Class_2012), levels(current$Name_2012))
# 
# writeRaster(current_ras, "Y:\\GIS map and Climate data\\current_landcover5km.bil", format = "EHdr")

current_ras <- raster(
  paste("Y:\\GIS map and Climate data\\current_landcover", reso,"km.bil", sep=""
  )
)

############################################################################################################
# Resample species occurrence raster on 1km to 5km resolution 
############################################################################################################

# Import Acaena occurrence data
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F02_Change_resolutionOfWORLDCLIM.R")

### Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Import reference raster of WGS
refWGS <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")

raster_conversion_and_extractBIOCLIM <- function(data){
  
  # Extract target species occurrence records
  points <- data[,c("lon","lat")]
  # Set coordinates
  coordinates(points) <- data[,c("lon","lat")]
  # Put values in point object
  points$sp <- rep(1,nrow(data))
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  sp_raster <- rasterize(points, refWGS, field = 1)
  
  # project raster from WGS84 to NZTM
  projected_raster <- projectRaster(sp_raster, crs = proj4stringNZTM)
  
  # Resample raster from "arc" to "reso" km.
  # Raster projection from WGS to NZTM doesn't make raster at the desired resolution, because 30sec in NZ is NOT 1km.
  projected_raster2 <- resample(projected_raster, current_ras, method="ngb")
  
  return(projected_raster2)
}

spRaster <- lapply(dat2, raster_conversion_and_extractBIOCLIM)


############################################################################################################
# Convert raster to dataframe
############################################################################################################

# Make raster stack
bio_land <- stack(c(bioNZ, spRaster, pre, current_ras))

res <- data.frame(cbind(coordinates(bio_land[[1]]), values(bio_land)))

write.csv(res, paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_", reso,"km.bil", sep=""
                     )
          )

############################################################################################################
# Create land cover history
############################################################################################################

source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F03_Change_resolutionOfWORLDCLIM.R")

d <- landCoverChange(res, prehuman_landcover="layer.1", current_landcover="layer.2")

# Delete NA rows of BIOCLIM data
d2 <- d[!is.na(d$bioclim1), ]

write.csv(d2, file = paste("Y://", genus_name, "_bioclim_landcover_history", reso, "km.csv", sep=""
                           )
          )

# Import species occurrence data
genus_name = "Acaena"
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F02_clean_up_species_records.R")

### Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Import reference raster of WGS
refWGS <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")
# Import reference raster of NZTM
current_ras <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")
proj4stringNZTM <- proj4string(current_ras)

# Import function
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F04_convert_occurrencePoints_to_raster.R")

spRaster <- lapply(dat2, convert_occurrencePoints_to_raster, refWGS = refWGS)

library(raster)


### Extract bioclim data
# Import bioclim raster data
files <- paste("Y:\\Niche change of lineages\\WORLDCLIM\\wc2.0_10m_bio", 
               list.files("Y:\\Niche change of lineages\\WORLDCLIM\\wc2.0_10m_bio"),
               sep="\\"
)

bio <- lapply(files[grep("tif$", files)], raster)

# Change names to match occurrence data columns
nam <- sapply(sapply(files[grep("bil$", files)], strsplit, "\\\\|.bil"), "[[", 5)
nam2 <- gsub("_411", "", gsub("bio", "bioclim", nam))
names(bio) <- nam2

# Raster stack
bioNZ <- stack(bio)

save(bioNZ, file = "Y:\\Niche change of lineages\\WORLDCLIM\\wc2.0_10m_bio\\wc2.0_10m_19bioclim.data")

############################################################################################################
# Create point opbjects of species occurrence records and convert them to raster
############################################################################################################

# Load raster stack of 19 bioclim variable rasters, named "bioNZ" 
load("Y:\\Niche change of lineages\\WORLDCLIM\\wc2.0_10m_bio\\wc2.0_10m_19bioclim.data")
names(bioNZ) <- paste("bioclim", 1:19, sep = "")

# Import files of grass species occurence records
genera_files <- paste("Y:\\Niche change of lineages", 
                               list.files("Y:\\Niche change of lineages"),
                               sep="\\"
                      )
genera_files2 <- genera_files[grepl("csv$", genera_files)]
genusname=genera_files2[1]
# Import bioclim raster data
refWGS <- raster("Y:\\Niche change of lineages\\WORLDCLIM\\wc2.0_10m_bio\\wc2.0_bio_10m_01.tif")

raster_conversion <- function(genusname){
  
  # Import Acaena occurrence data
  data <- read.csv(genusname)
  dat <- data[is.na(data$decimallatitude) == F,]
  
  # Extract focal species occurrence records
  points <- dat[,c("decimallongitude", "decimallatitude")]
  # Set coordinates
  coordinates(points) <- dat[,c("decimallongitude", "decimallatitude")]
  # Put values in point object
  points$sp <- rep(1,nrow(dat))
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  sp_raster <- rasterize(points, refWGS, field = 1)

  return(sp_raster)
  }

res <- lapply(genera_files2, raster_conversion)

# Raster stack of bioclim and Acaena occurrence records 
grassRaster <- stack(c(bioNZ, res))

# Change raster layer names
names(grassRaster)[20:length(names(grassRaster))] <- list.files("Y:\\Niche change of lineages")[c(4,12)]


############################################################################################################
# Convert raster to dataframe
############################################################################################################

res <- data.frame(cbind(coordinates(grassRaster[[1]]), values(grassRaster)))
res2 <- res[is.na(res$bioclim1)==F,]

write.csv(res2, "Y:\\Niche change of lineages\\grasses_inclNoNA.csv")

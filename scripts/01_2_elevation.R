########################################################################################
### Mean elevation of occurrence records
########################################################################################

### Import data
alt <- read.csv(
  paste("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\", genus_name, "_bio_alt.csv", sep=""
        )
)

# sp names
spname <- grepl(paste("^", genus_name, sep=""), colnames(alt)) %>% colnames(alt)[.]

ras <- project_and_convert_occurrencePoints_to_raster(alt, refWGS = pre, val = "NZL1_alt")

# Make raster stack
bio_land2 <- stack(c(bio_land, ras[[4]]))


########################################################################################
### Elevation of any grid cells in NZ
########################################################################################

library(raster)

elev <- raster("Y:\\GIS map and Climate data\\kx-nz-80m-digital-elevation-model-GTiff\\nztm.tif")
elev.wgs <- projectRaster(elev, pre)

elev.dat <- data.frame(coordinates(elev.wgs), values(elev.wgs))
colnames(elev.dat)[1:2] <- c("lon", "lat", "elev")

ras <- project_and_convert_occurrencePoints_to_raster(elev, refWGS = pre, val = "elev")


##########################################################################################
####################            DATA CLEANING           ##################################
##########################################################################################
## Purpose:
## remove wrong names, geographically and topologically unrealistic occurrence points
##
## Input of this script:
##  "Acaena_bio_alt.csv"
##  "Festuca_bio_alt.csv"
##  "chionochloa_bio_alt.csv"
##
##
## Output of this script:
##  "point-varidated_chion.csv"
##  "point-varidated_acaena.csv"
##  "point-varidated_festuca.csv"
##  "map_validated_Acaena"
##  "map_validated_Festuca"
##
## 
##########################################################################################

### libraries
library(raster)
library(dismo)
library(dplyr)

##########################################################################################
####################           IMPORT DATA           #####################################
##########################################################################################

# Chionochloa
chion <- read.csv("Y:\\Acaena project\\chionochloa_bio_alt.csv")

##########################################################################################
####################   POINT VALIDATION  #################################################
##########################################################################################

### remove rows having NAs
chion2 <- chion %>%
  filter(!is.na( bio1_411 ))

### make sure all NA has been removed
apply(chion2, 2, function(x){sum(is.na(x))})


### in bioclim data, temperature in * 10C and precipitation in mm
### check -> http://www.worldclim.org/formats


### Change data type of taxa
chion2 <- chion2 %>%
  mutate(taxa = as.character(taxa))


### Delete all data out of NZ by coodinate
chion3 <- chion2 %>%
  filter(lat >= -48 & lat <= -33 & lon <= 179 & lon >= 165)

##########################################################################################
####################  NAME CLEANING    ###################################################
##########################################################################################

### Check all names in http://www.nzpcn.org.nz/
levels(as.factor(chion3$taxa))

chnames <- read.csv("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\NZPCN_Flora_Chionochloa.csv")
chnames2 <- as.character(chnames$Species)
chnames3 <- sort(gsub(" ", "_", chnames2))

# Names of Chionochloa in original data
unique(chion3$taxa)
# Accepted scientific names of Chionochloa among them
sum(unique(chion3$taxa) %in% chnames3 == F)
unique(chion3$taxa)[unique(chion3$taxa) %in% chnames3]

####################
# No wrong names
####################

##########################################################################################
####################   Point cleaning     ################################################
##########################################################################################

##### remove rows with 0
chion4 <- chion3 %>% 
  filter(bio12_411 != 0)
      
##### Find obvious wrong occurrences by mapping
chion5 <- split(chion4, chion4$taxa)

# Import NZTM raster data
current1000 <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")

# Import bioclim raster data
refWGS <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")
# Prepare project string of NZTM
proj4stringNZTM <- proj4string(current1000)

raster_conversion_and_extractBIOCLIM <- function(d1){
  
  # Convert occurrence coodinates into points
  points <- d1[,c("lon","lat")]
  # Set coordinates
  coordinates(points) <- d1[,c("lon","lat")]
  # Put values in point object
  points$sp <- rep(1,nrow(d1))
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  sp_raster <- rasterize(points, refWGS, field = 1)
  
  # project raster from WGS84 to NZTM
  projected_raster <- projectRaster(sp_raster, crs = proj4stringNZTM)
  
  # Resample raster from 30sec to 1km.
  # 30sec in NZ is NOT 1km.
  projected_raster2 <- resample(projected_raster, current1000, method="ngb")
  
  return(projected_raster2)
}

tes <- lapply(chion5, raster_conversion_and_extractBIOCLIM)

# Raster stack of bioclim and Acaena occurrence records 
load("Y:\\GIS map and Climate data\\worldclim\\bio_411NZTM\\bioclimNZTM.data")
chionRaster <- stack(c(bioNZ, tes))

############################################################################################################
# Convert raster to dataframe
############################################################################################################

pre1000 <- raster("Y:\\GIS map and Climate data\\pre-human_landcover1km.bil")

# Make raster stack
bio_land <- stack(c(chionRaster, pre1000, current1000))


save(bio_land, file="Y:\\GIS map and Climate data\\Chionochloa_Bioclim_landcover_1km_NZTM.data")

res <- data.frame(cbind(coordinates(bio_land[[1]]), values(bio_land)))

write.csv(res, "Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")

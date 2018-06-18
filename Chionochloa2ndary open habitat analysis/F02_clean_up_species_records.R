##########################################################################################
####################            DATA CLEANING           ##################################
##########################################################################################

### libraries
library(raster)
library(dismo)
library(dplyr)

##########################################################################################
####################           IMPORT DATA           #####################################
##########################################################################################

# Chionochloa
sp <- read.csv(
  paste("Y:\\Acaena project\\", genus_name, "_bio_alt.csv", sep = "")
)

##########################################################################################
####################   POINT VALIDATION  #################################################
##########################################################################################

### remove rows having NAs
sp2 <- sp %>%
  filter(!is.na( bio1_411 ))

### make sure all NA has been removed
apply(sp2, 2, function(x){sum(is.na(x))})


### in bioclim data, temperature in * 10C and precipitation in mm
### check -> http://www.worldclim.org/formats


### Change data type of taxa
sp2 <- sp2 %>%
  mutate(taxa = as.character(taxa))


### Delete all data out of NZ by coodinate
sp3 <- sp2 %>%
  filter(lat >= -48 & lat <= -33 & lon <= 179 & lon >= 165)

##########################################################################################
####################  NAME CLEANING    ###################################################
##########################################################################################

### Check all names in http://www.nzpcn.org.nz/
levels(as.factor(sp3$taxa))

chnames <- read.csv(
  paste("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\NZPCN_Flora_",
                    genus_name, ".csv", sep = "")
  )

chnames2 <- as.character(chnames$Species)
chnames3 <- sort(gsub(" ", "_", chnames2))

# Names of Chionochloa in original data
unique(sp3$taxa)
# Accepted scientific names of Chionochloa among them
sum(unique(sp3$taxa) %in% chnames3 == F)
unique(sp3$taxa)[unique(sp3$taxa) %in% chnames3]

########################################
# Correct wrong names
########################################

# No wrong names for Chionochloa

# Acaena has wrong names

test2 <- sp3[sp3$taxa!="Acaena_microphylla", ]

##########################################################################################
####################   Point cleaning     ################################################
##########################################################################################

##### remove rows with 0
sp4 <- sp3 %>% 
  filter(bio12_411 != 0)
      
##### Get a dataframe for each species
sp5 <- split(sp4, sp4$taxa)

dat <- sp5
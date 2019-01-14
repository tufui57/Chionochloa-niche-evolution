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
if(genus_name == "Chionochloa"){
  sp <- read.csv("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\rdata_Chionochloa_all-occurances.csv")
  
}

if(genus_name == "Acaena"){
  sp <- read.csv("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\rdata_Acaena_all-presence.csv")
  
}

##########################################################################################
####################   POINT VALIDATION  #################################################
##########################################################################################

### Change data type of taxa
sp2 <- sp %>%
  mutate(taxa = as.character(taxa))


### Delete all data out of NZ by coodinate
sp3 <- sp2 %>%
  filter(lat >= -48 & lat <= -33 & lon <= 179 & lon >= 165)

##########################################################################################
####################  NAME CLEANING    ###################################################
##########################################################################################

########################################
# Correct wrong names
########################################

### Check all names in http://www.nzpcn.org.nz/
levels(as.factor(sp3$taxa))

chnames <- read.csv(
  paste("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\NZPCN_Flora_",
                    genus_name, ".csv", sep = "")
  )

chnames2 <- as.character(chnames$Species)
chnames3 <- sort(gsub(" ", "_", chnames2))

# Names of Chionochloa in original data
unique(sp3$taxa)
# Accepted scientific names of Chionochloa among them
sum(unique(sp3$taxa) %in% chnames3 == F)
unique(sp3$taxa)[unique(sp3$taxa) %in% chnames3]

##########################################################################################
####################   Point cleaning     ################################################
##########################################################################################

dat <- split(sp3, list(sp3$taxa))

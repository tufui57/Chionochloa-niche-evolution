##############################################################################################
###     Draw maps for each sp coloured by land cover change     ###
##############################################################################################

library(dplyr)
library(raster)
library(rgdal)
library(maptools)

genus_name <- "Chionochloa"
  
if(genus_name == "Chionochloa"){
    genus_tag <- "chion"
    
    # data import
    chdata <- read.csv(paste("Y://Acaena project//", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
  }

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
  # data import
  chdata <- read.csv(paste("Y://Acaena project//", genus_name, "_bioclim_landcover_history_inclNAonland.csv", sep = ""))
  
}


# species name
spname <- grepl(genus_name, colnames(chdata)) %>% colnames(chdata)[.]

# Reference raster to create point object from coordinates
ref <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

###############################################################
## Point map for landcover change of sp 
###############################################################

pointPlot_sp <- function(data, # Subset data frame for a species
                         title # Title of figure
                         ) {
  
  # Extract data
  data <- data[!is.na(data[, "landCoverChange"]),]
  data <- data[ - which(data$landCoverChange == "nonPotentialHabitat" | data$landCoverChange == "NF-nonPotentialHabitat"| data$landCoverChange == "nonF-nonPotentialHabitat"| data$landCoverChange == "NF-EF" | data$landCoverChange == "NF-NF" | data$landCoverChange == "nonF-EF" | data$landCoverChange == "nonF-NF"),]
  
  # Convert land cover change column to numeric
  data$changeNo <- NA
  data[data$landCoverChange == "nonF-nonF", "changeNo"] <- 1
  data[data$landCoverChange == "NF-nonF", "changeNo"] <- 2
  
  # create point object
  pts <- data[, c("x", "y")]
  
  # point coordinate system setting
  coordinates(pts) <- data[, c("x", "y")]
  proj4pts <- proj4string(ref)
  proj4string(pts) <- CRS(proj4pts)
  
  # Land cover change column
  pts$changeNo <- data$changeNo
  
  #####################
  # Plot
  #####################
  png(filename = paste("Y:\\landcoverChange_", title, ".png", sep = ""),
      width = 500, height = 710)
  par(cex = 0.8)
  plot(pts,
       # colour by group and add alpha
       bg = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5))[pts$changeNo],
       pch = 21, axes = T,
       # no outline
       col = NA,
       main = paste(title, "\n Number of occurrence cells = ", nrow(data)),
       cex.main = 1.8,
       # set extent
       xlim = extent(ref)[1:2], ylim = extent(ref)[3:4]
  )
  # add legend manually
  legend(x = 1100000, y = 6200000,
         pch = 21, col = c("red", "blue"), legend = c("Secondary\n open area", "Primary\n open area"),
         cex = 1.5)
  
  # Outline of NZ
  plot(nzland, add = TRUE)
  
  dev.off()
}

## all sp
# extract sp data
chdataAllsp <- chdata[rowSums(chdata[, spname]) > 0,]
pointPlot_sp(chdataAllsp, "test Chionochloa")

###############################################################
## Point map for land cover change of sp 
###############################################################

chdata2 <- lapply(spname[c(1:16, 18:20, 22:length(spname))], function(i){chadata[chdata[,i]==1,]})
lapply(c(1:16, 18:20, 22:30,32:length(spname)), function(i){pointPlot_sp(chdata2[[i]], title=spname[i])})

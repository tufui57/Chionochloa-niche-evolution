##############################################################################################
###     Draw maps for each sp coloured by land cover change     ###
##############################################################################################

library(raster)
library(rgdal)
library(maptools)

# data import
d1 <- read.csv("Y://Chionochloa_bioclim_landcover_history_inclNAonland.csv")
d <- d1[is.na(d1$landCoverChange) == F, ]

# sp name
spname <- colnames(d1)[grep("^Chion", colnames(d1))]

for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

# Reference raster to create point object from coordinates
ref <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)

###############################################################
## Point map for land use change of sp 
###############################################################

pointPlot_sp <- function(d, # Subset dataframe for a species including land cover change column 
                         title # Title of figure
                         ) {
  
  d <- d[!is.na(d[, "landCoverChange"]),]
  d <- d[ - which(d$landCoverChange == "nonPotentialHabitat" | d$landCoverChange == "NF-nonPotentialHabitat"| d$landCoverChange == "nonF-nonPotentialHabitat"| d$landCoverChange == "NF-EF" | d$landCoverChange == "NF-NF" | d$landCoverChange == "nonF-EF" | d$landCoverChange == "nonF-NF"),]
  
  # Convert land use change column to numeric
  d$changeNo <- NA
  d[d$landCoverChange == "nonF-nonF", "changeNo"] <- 1
  d[d$landCoverChange == "NF-nonF", "changeNo"] <- 2
  
  # create point object
  pts <- d[, c("x", "y")]
  
  # point coordinate system setting
  coordinates(pts) <- d[, c("x", "y")]
  proj4pts <- proj4string(ref)
  proj4string(pts) <- CRS(proj4pts)
  
  # Land cover change column
  pts$changeNo <- d$changeNo
  
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
       main = paste(title, "\n Number of occurrence cells = ", nrow(d)),
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
dall <- d[rowSums(d[, spname]) > -18,]
pointPlot_sp(dall, "All studied sepcies of Chionochloa")

###############################################################
## Point map for land cover change of sp 
###############################################################

d2 <- lapply(spname[c(1:16, 18:20, 22:length(spname))], function(i){d[d[,i]==1,]})
lapply(c(1:16, 18:20, 22:30,32:length(spname)), function(i){pointPlot_sp(d2[[i]], title=spname[i])})

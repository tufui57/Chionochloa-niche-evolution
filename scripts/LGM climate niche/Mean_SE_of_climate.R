###################################################################################
#############    Average of climate data    ######################################
###################################################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)

# Load LGM climate data
load(".//LGMclimate.data")


for (i in 1:4) {
  if(i==1|i==2){
    print(
      mean((values(lgm.mainland[[i]]) / 10), na.rm = "T")
    )
    print(
      sd((values(lgm.mainland[[i]]) / 10), na.rm = "T")
    )
  }else{
       print(
     mean(values(lgm.mainland[[i]]), na.rm = "T")
   )
  print(
    sd(values(lgm.mainland[[i]]), na.rm = "T")
  )
  }

 }


# Load current climate data
load(".\\Scores_Acaena_landcover_worldclim1_5km.data")


for (i in (paste("bioclim", c(1,6,12,15), sep=""))){
  if(i=="bioclim1"|i=="bioclim6"){
    print(
      mean((scores[,i] / 10), na.rm = "T")
    )
    print(
      sd((scores[,i] / 10), na.rm = "T")
    )
  }else{
    print(
      mean(scores[,i], na.rm = "T")
    )
    print(
      sd(scores[,i], na.rm = "T")
    )
  }
  
}

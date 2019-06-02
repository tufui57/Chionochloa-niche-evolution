
#####################################################################################
## Data preparation for drawing species distribution maps and PCA
#####################################################################################

genus_name = "Acaena"

library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)

source(".\\functions\\F_plot_map_and_PCA.r")

# reference rasters
ref <- raster("Y://GIS map and Climate data//pre-human_landcover5km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland <- crop(nzland, ref)

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

### Data import
load(
  paste(".\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")
)

spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]

# Species list for phylogeny chapter
if(genus_name == "Chionochloa"){
  spname <- spname[-c(4, 9)]
}else{
  spname <- spname[-c(1, 11,16)]
}

###############################################################
## Plot map and niche space 
###############################################################

lapply(seq(1,length(spname),3), function(i){
  
  png(paste("Y://monocolour_niche_map_3sp", spname[i], ".png", sep=""), width = 800, height = 1300)
  
  pMain1 <- niche_plot_monoColour(spname[i], scores)
  pMap1 <- map_plot_monoColour(spname[i], scores)
  title1 <- textGrob(gsub("_", " ", spname[i]), gp = gpar(fontface = "bold", cex = 1.75))
  spi <- grid.arrange(pMap1, pMain1, top = title1, ncol = 2, widths = c(3,5))
  
  if(i+1 > length(spname)){
    spi2 <- ggplot(scores, aes(PC1, PC2)) +
      geom_blank() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            line = element_blank(),
            panel.background = element_blank()
      )
  }else{
    pMain2 <- niche_plot_monoColour(spname[i+1], scores)
    pMap2 <- map_plot_monoColour(spname[i+1], scores)
    title2 <- textGrob(gsub("_", " ", spname[i+1]), gp=gpar(fontface="bold", cex=1.75))
    spi2 <- grid.arrange(pMap2, pMain2, top = title2, ncol = 2, widths = c(3,5))
  }
  
  if(i+2 > length(spname)){
    spi3 <- ggplot(scores, aes(PC1, PC2)) +
      geom_blank() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            line = element_blank(),
            panel.background = element_blank()
      )
  }else{
    pMain3 <- niche_plot_monoColour(spname[i+2], scores)
    pMap3 <- map_plot_monoColour(spname[i+2], scores)
    title3 <- textGrob(gsub("_", " ", spname[i+2]), gp=gpar(fontface="bold", cex=1.75))
    spi3 <- grid.arrange(pMap3, pMain3, top = title3, ncol = 2, widths = c(3,5))
    
    }

  # Plot in multiple panels
  grid.arrange(spi, spi2, spi3, nrow = 3)
  
  dev.off()
  
})

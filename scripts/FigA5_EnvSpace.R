
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
  paste(".\\Scores_", genus_name,"_landcover_worldclim1_1km.data", sep = "")
)

spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]

# Species list for phylogeny chapter
if(genus_name == "Acaena"){
  spname <- spname[-c(11,18)]
}

###############################################################
## Plot map and niche space 
###############################################################

library(dplyr)
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")
### Name tag
spname2 <- makeTag_separate(spname, genus_name, "_")
spname3 <- spname2$tag %>% as.character

sp <- lapply(1:length(spname), function(i){
  pMain <- niche_plot_monoColour(spname[i], scores)
  title <- textGrob(spname3[i], gp=gpar(fontface="bold", cex=1.75))
  spi <- grid.arrange(pMain, top = title)
  
  return(spi)
})


for(i in seq(1, length(spname), 8)){
  png(paste("Y://monocolour_niche_10sp", spname[i], ".png", sep=""), width = 800, height = 1300)
  grid.arrange(sp[[i]], sp[[i+1]],sp[[i+2]], sp[[i+3]], sp[[i+4]], sp[[i+5]], sp[[i+6]], sp[[i+7]],
             ncol=2)
dev.off()
  
}

# Empty panel
pEmpty <- ggplot(scores, aes(PC1, PC2)) +
  geom_blank() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        panel.background = element_blank()
  )

#### Acaena
png(paste("Y://monocolour_niche_10sp", spname[17], ".png", sep=""), width = 800, height = 1300)
grid.arrange(sp[[17]], sp[[18]], pEmpty, pEmpty, pEmpty, pEmpty, pEmpty, pEmpty,
             ncol=2)
dev.off()

#### Chionochloa
png(paste("Y://monocolour_niche_10sp", spname[33], ".png", sep=""), width = 800, height = 1300)
grid.arrange(sp[[33]], sp[[34]], pEmpty, pEmpty, pEmpty, pEmpty, pEmpty, pEmpty,
             ncol=2)
dev.off()




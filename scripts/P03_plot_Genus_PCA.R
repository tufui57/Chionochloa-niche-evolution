###################################################
### Plot PCA of genus
###################################################

library(ggplot2)
library(gridExtra)
source(".//functions//F_plot_PCA.r")
genus_name <- "Chionochloa"

if(genus_name == "Acaena"){
  genus_tag = "acaena"
}else{
  genus_tag = "chion"
    }

source("Y:\\1st chapter_Acaena project\\Acaena manuscript\\scripts\\03_2_function_NicheOverlap_EnvSpace_Map.R")
source("Y:\\1st chapter_Acaena project\\Acaena manuscript\\scripts\\F05_EnvSpaceWithHist.r")

### Data import
if(genus_name == "Acaena"){
  da1 <- read.csv("Y:\\1st chapter_Acaena project\\Acaena manuscript\\meta data\\Acaena_bioclim_landcover_history_worldclim1_1km18sep.csv")
  
}else{
  da1 <- read.csv("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\Chionochloa_bioclim_landcover_history_worldclim1_1km.csv"
  )
}

d <- da1[is.na(da1$landCoverChange) == F, ]

# Species names
sname <- colnames(d)[grepl(paste("^", genus_name, sep=""), colnames(d))]

for(i in sname){
  d[is.na(d[,i]),i] <- 0
}

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], sname,
                           "x", "y", "preLandcover", "currentLandcover", "landCoverChange")], pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)
scores$pre <- factor(ifelse(scores$preLandcover == 1, "NF", "nonF"))
scores$post <- factor(ifelse(scores$currentLandcover == 1, "NF",
                             ifelse(scores$currentLandcover == 3, "nonF", "non potential habitat" # EF(2) is also non potential habitat
                             ))
)
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

# Create genus PCA score
scores.genus <- rowSums(scores[, colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))]], na.rm = TRUE)

scores.genus2 <- scores[(scores.genus >= 1),]


###################################################
# Plot PCA
###################################################

pMain <- PCAplot(scores, scores.genus2, col = "blue", extent_x, extent_y) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
        )

niche <- PCA_withHist(scores.genus2, spname = genus_name,
                      histColour = "blue", pMain = pMain,
             save = F)

##################################################
# Plot maps
###################################################

# Reference raster
ref <- raster(paste("Y:\\GIS map and Climate data\\current_landcover1km.bil", sep = ""))

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)

map_plot <- function(data # data for map
){
  
  ### Create point object from coordinates
  pts <- data[, c("x", "y")]
  # point coordinate system setting
  coordinates(pts) <- data[, c("x", "y")]
  proj4pts <- proj4string(ref)
  proj4string(pts) <- CRS(proj4pts)
  # Land use change column
  pts$changeNo <- rep(1, nrow(data))
  
  ### Map
  pMap <- ggplot() +
    geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill='gray90') +
    
    geom_point(data = data, aes_string(x = "x", y = "y"), color = "blue", alpha = 0.1) +
    
    ggtitle(paste("N =", nrow(data))) +
    
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha=0.7))) +
    
    labs(x="", y="") +
    # Legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          plot.title = element_text(family = "Times New Roman", size = 20),
          legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background =  element_blank(), #element_rect(fill = 'gray96'),
          #axis.text.y = element_text(angle = 90, hjust = 0.5)
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(pMap)
  
}

##################################################
# Plot in multiple panels
###################################################
pMain <- grid.arrange(niche[[2]], niche[[4]], niche[[1]], niche[[3]],
                          ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
pMap <- map_plot(scores.genus2)
title <- textGrob(genus_name, gp=gpar(fontface="bold", cex=1.75))


png(paste("Y://", genus_name, ".png", sep=""), width = 1200, height = 650)
grid.arrange(pMap, pMain, top = title, ncol = 2, widths = c(3,5))
dev.off()


###############################################################
## Niche space 
###############################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)

genus_name = "Chionochloa"
reso = 1

# Import PCA scores
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_", reso, "km.data", sep = ""))

# Data import
d1 <- read.csv(paste("Y://2nd chapter_phylogentic niche conservation//meta data//", genus_name, "_bioclim_landcover_history_worldclim1_", reso, "km.csv", sep="")
               )
# Remove NA rows
d <- d1[!is.na(d1$landCoverChange), ]

# Species name
sname <- colnames(d)[grep(paste("^", genus_name, sep = ""), colnames(d))]


# Reference raster
ref <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411NZTM\\bio1_411.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)

###############################################################
## Plot niche space with histograms
###############################################################

subset.data <- function(i, # character string of species name
                       scores, # data for niche space
                       colname.to.draw # character string. Colname of the data to colour
) {
  ### Subset data
  scores1 <- scores[!is.na(scores[,i]), ] 
  scores.s <- scores1[scores1[,i] == 1, ]
  
  if(sum(scores.s[, colname.to.draw] == "nonPotentialHabitat" | scores.s[, colname.to.draw] == "NF-nonPotentialHabitat"| scores.s[, colname.to.draw] == "nonF-nonPotentialHabitat"| scores.s[, colname.to.draw] == "NF-EF" | scores.s[, colname.to.draw] == "NF-NF" | scores.s[, colname.to.draw] == "nonF-EF" | scores.s[, colname.to.draw] == "nonF-NF", na.rm = TRUE)>0){
    scores.s2 <- scores.s[ - which(scores.s[, colname.to.draw] == "NF-nonPotentialHabitat"| scores.s[, colname.to.draw] == "nonF-nonPotentialHabitat"| scores.s[, colname.to.draw] == "NF-EF" | scores.s[, colname.to.draw] == "NF-NF" | scores.s[, colname.to.draw] == "nonF-EF" | scores.s[, colname.to.draw] == "nonF-NF"),]
  }else{
    scores.s2 <- scores.s
  }
  
  return(scores.s2)
}

niche_plot <- function(i, # character string of species name
                     scores, # data for niche space
                     colname.to.draw # character string. Colname of the data to colour
) {

  scores.s2 <- subset.data(i, scores, colname.to.draw)
  
  ### Plot niche
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = scores.s2, aes_string("PC1", "PC2", colour = colname.to.draw), alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    # change point colour and legend title and texts
    scale_colour_manual(
      # Name of each legend factor. 
      # This must be same factors as factors in "colname" of ggplot(aes(colour = colname)), otherwise no legend will be drawn.
      breaks = c("NF-nonF", "nonF-nonF"),
      label = c("Secondary open area", "Primary open area"),
      # colours
      values = c("red", "blue")
    ) +
    # No legend
    theme(axis.title = element_text(size = 15),
      legend.position = "none",
      panel.background = element_rect(fill = 'gray96')
    )
  
  
  # Primary and secondary open occurrences
  scores.secondary <- scores.s2[scores.s2[, colname.to.draw] == 'NF-nonF', ]
  scores.primary <- scores.s2[scores.s2[, colname.to.draw] == 'nonF-nonF', ]
  
  # Histogram of PC1 (x axis)
  pTop <- ggplot(scores.s2, aes(x = PC1)) +
    geom_histogram(data = scores.secondary, fill = "red", alpha = 0.35) +
    geom_histogram(data = scores.primary, fill = "blue", alpha = 0.35) +
    xlim(extent_x) +
    xlab(expression(hot %<->% cold)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(""),
          axis.ticks.x = element_blank()
    ) +
    theme(axis.title = element_text(size = 15),
      panel.background = element_rect(fill = 'gray96'))
  
  # Histogram of PC2 (y axis)
  pRight <- ggplot(scores.s2, aes(x = PC2)) +
    geom_histogram(data = scores.secondary, fill = "red", alpha = 0.35) +
    geom_histogram(data = scores.primary, fill = "blue", alpha = 0.35) +
    xlim(extent_y) +
    xlab(expression(dry %<->% wet)) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = 270, vjust = 0.25),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(angle = 270, size = 15),
      axis.ticks.y = element_blank()
    ) +
    coord_flip() +
    theme(panel.background = element_rect(fill = 'gray96'))
  
  # Empty panel
  pEmpty <- ggplot(scores, aes(PC1, PC2)) +
    geom_blank() +
    theme(axis.title = element_blank(),
      axis.text = element_blank(),
      line = element_blank(),
      panel.background = element_blank()
      )
  
  # Plot in multiple panels
  combinedP <- grid.arrange(pTop, pEmpty, pMain, pRight,
                      ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
  return(combinedP)
}

#####################
# Plot point map
#####################

map_plot <- function(i, # character string of species name
                     data, # data for map
                     colname.to.draw # character string. Colname of the data to colour
){

  ### subset data for a species including land use change column
  d.s <- subset.data(i, d, colname.to.draw)
  
  ### Convert data type of land use change column from character to numeric
  d.s$changeNo <- NA
  d.s[d.s[,colname.to.draw] == "nonF-nonF", "changeNo"] <- 1
  d.s[d.s[,colname.to.draw] == "NF-nonF", "changeNo"] <- 2
  
  ### Create point object from coordinates
  pts <- d.s[, c("x", "y")]
  # point coordinate system setting
  coordinates(pts) <- d.s[, c("x", "y")]
  proj4pts <- proj4string(ref)
  proj4string(pts) <- CRS(proj4pts)
  # Land use change column
  pts$changeNo <- d.s$changeNo
  
  ### Map
  # ggplot for raster
  # http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
  pMap <- ggplot() +
    geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill='gray90') +
    
    geom_point(data = d.s, aes_string(x = "x", y = "y", color = colname.to.draw), alpha = 0.1) +
    
    scale_colour_manual(
      # title
      name = paste("N =", nrow(d.s)),
      breaks = c("nonF-nonF", "NF-nonF"),
      label = c("Primary \n open area","Secondary \n open area"),
      # colours
      values = c("red", "blue")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha=0.7))) +
    
    labs(x="", y="") +
    # legend position inside plot at topleft
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

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

lapply(seq(1,16,3), function(i){
  
  png(paste("Y://niche_map_3sp", sname[i], ".png", sep=""), width = 800, height = 1300)
  
  pMain1 <- niche_plot(sname[i], scores, "landCoverChange")
  pMap1 <- map_plot(sname[i], d, "landCoverChange")
  title1 <- textGrob(gsub("_", " ", sname[i]), gp=gpar(fontface="bold", cex=1.75))
  spi <- grid.arrange(pMap1, pMain1, top = title1, ncol = 2, widths = c(3,5))
  
  pMain2 <- niche_plot(sname[i+1], scores, "landCoverChange")
  pMap2 <- map_plot(sname[i+1], d, "landCoverChange")
  title2 <- textGrob(gsub("_", " ", sname[i+1]), gp=gpar(fontface="bold", cex=1.75))
  spi2 <- grid.arrange(pMap2, pMain2, top = title2, ncol = 2, widths = c(3,5))
  
  pMain3 <- niche_plot(sname[i+2], scores, "landCoverChange")
  pMap3 <- map_plot(sname[i+2], d, "landCoverChange")
  title3 <- textGrob(gsub("_", " ", sname[i+2]), gp=gpar(fontface="bold", cex=1.75))
  spi3 <- grid.arrange(pMap3, pMain3, top = title3, ncol = 2, widths = c(3,5))
  
  # Plot in multiple panels
  #grid.arrange(pMap1, pMain1, pMap2, pMain2, pMap3, pMain3, ncol = 2, nrow = 3, widths = c(3,5))
  grid.arrange(spi, spi2, spi3, nrow = 3)
  
  dev.off()
  
})


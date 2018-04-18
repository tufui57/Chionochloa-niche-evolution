#################################################################################
### Plot map and PCA of persistent climate of open habitat
#################################################################################

########################################
### Niche data preparation
########################################

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)
#"ccmidbi_30s"

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")
a = 0.001
load(paste(".//currentNicheSimilarToLGM_", a,".data", sep = ""))

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current climate data frame 
scoresLGM <- mutate(scores,  lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1,0))

# Persistent climate
scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

# Primary ope habitat
scores.p <- scoresLGM[scoresLGM$landCoverChange == "nonF-nonF", ]

# Persistent climate of open habitat
scoresLGM.p <- filter(scoresLGM, lgm == 1, landCoverChange == "nonF-nonF")

# Extents
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

###############################################################
## Map data preparation
###############################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)

# Data import
d <- read.csv(paste("Y:\\Acaena project\\", genus_name,"_bioclim_landcover_history_inclNAonland.csv", sep = ""))

# Reference raster
ref <- raster("Y:\\GIS map and Climate data\\current_landcover1km.bil")

# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland2 <- crop(nzland, ref)


########################################
### Plot map and PCA
########################################

niche <- ggplot() +
  # NZ
  geom_point(data = scores, aes(PC1, PC2), color = 'gray90') +
  # Primary open area
  geom_point(data = scores.p, aes(PC1, PC2), color = "blue") +
  # Persistent open habitat
  geom_point(data = scoresLGM.p, aes(PC1, PC2), color = "red") +
  # extent
  xlim(extent_x) +
  ylim(extent_y) +
  # legend position inside plot
  theme(axis.title = element_text(size = 15),
        legend.position = "none",
        panel.background = element_rect(fill = 'gray96')
  )




#####################
# Map
#####################

# Convert land cover change column to numeric
scores.p$changeNo <- NA
scores.p[scores.p$lgm == 1, "changeNo"] <- "Persistent"
scores.p[scores.p$lgm == 0, "changeNo"] <- "Primary"


############################
# Plot map
############################
map <- ggplot() +
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
  geom_point(data = scores.p, aes(x = x, y = y, color = changeNo), size = 0.01) +
  scale_colour_manual(
    # title
    name = "",
    breaks = c("Persistent", "Primary"),
    label = c("Open area with \n persitent climate","Primary open area"),
    # colours
    values = c("red", "blue")
  ) +
  # http://stackoverflow.com/questions/16356052/control-ggplot2-legend-look-without-affecting-the-plot
  guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
  labs(x="", y="") +
  #ggtitle(gsub("_", " ", sname[i])) +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        legend.justification = c(1, 1), legend.position = c(0.5, 0.9),
        panel.background =  element_blank(), #element_rect(fill = 'gray96'),
        #axis.text.y = element_text(angle = 90, hjust = 0.5)
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )
  
png("Y://Persistent_niche_map.png", width = 1200, height = 650)
grid.arrange(map, niche, ncol = 2, widths = c(3,5))
dev.off()


pMap <- ggplot() +
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
  # Primary open area
  geom_point(data = scores.p, aes(x, y), color = "blue", size = 10^(-100)) +
  # Persistent open habitat
  geom_point(data = scoresLGM.p, aes(x, y), color = "red", size = 10^(-100))


# Rasterize habitat with persistent climate and primary open habitat.
# ggplot points can't be as small as you want.


### Ratio of habitat
nz <- nrow(scores)
persistent <- nrow(scoresLGM.p)
primary <- nrow(scores.p)
lgm <- nrow(scoresLGM2)



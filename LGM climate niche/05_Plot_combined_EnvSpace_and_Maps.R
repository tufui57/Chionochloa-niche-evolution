#################################################################################
### Plot map and PCA of persistent climate of open habitat
#################################################################################
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
########################################
### Niche data preparation
########################################

genus_name <- "Acaena"


########################################
### Data preparation
########################################

load( paste(".\\Scores_", genus_tag,"_landcover.data", sep = ""))

a = 0.001
load(paste(".//currentNicheSimilarToLGM_", a,".data", sep = ""))

# Add cell ID
scores$cellID <- 1:nrow(scores)

# Add availability of climate niche in LGM to current climate data frame
# neighbours$dat2cellID has "scores" cell ID. SO, rows of "scores" whose cell ID are in "neighbours$dat2cellID"

scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))

# Persistent climate; climate that is shared between LGM and the present. 
scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

# Persistent climate of open habitat; persistent climate in primary open habitat
persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scoresLGM[scoresLGM[, "landCoverChange"] == "nonF-nonF", ]

# Load LGM climate scores
load(".\\LGM_mainisland_scores.data")

# Extents
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

###############################################################
## Map data preparation
###############################################################

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
  # LGM climate
  geom_point(data = newdf, aes(PC1, PC2), color = "lightpink") +
  # Primary open area
  geom_point(data = primary, aes(PC1, PC2), color = "blue") +
  # Persistent open habitat
  geom_point(data = persistentOpenHabitat, aes(PC1, PC2), color = "red") +
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

# Convert land cover change column
primary$changeNo <- NA
primary[primary$lgm == 1, "changeNo"] <- "Persistent"
primary[primary$lgm == 0, "changeNo"] <- "Primary"
primary[primary$lgm == 1, "size"] <- 0.001
primary[primary$lgm == 0, "size"] <- 0.1

###############################################################
# Prepare a polygon of LGM terrestrial area
###############################################################

mainIsland <- readOGR(".\\LGM_mainisland\\mainIsland.shp")

############################
# Plot map
############################

map <- ggplot() +
  # LGM outline
  geom_polygon(data = mainIsland, aes(x = long, y = lat, group = group), colour = "lightpink3", fill = 'lightpink') +
  # Current NZ outline
  geom_polygon(data = nzland2, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
  # Primary open habitat
  geom_point(data = primary, aes(x = x, y = y, color = changeNo), size = 0.001) +
  scale_colour_manual(
    # title
    name = "",
    breaks = c("Persistent", "Primary"),
    label = c("Open area with \n persitent climate","Primary open area"),
    # colours
    values = c("red", "blue")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
  labs(x="", y="") +
  # legend position inside plot at topleft
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        plot.title = element_text(family = "Times New Roman", size = 20),
        legend.justification = c(1, 1), legend.position = c(0.4, 0.9),
        panel.background =  element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

png("Y://Persistent_niche_map.png", width = 1200, height = 650)
grid.arrange(map, niche, ncol = 2, widths = c(3,5))
dev.off()

# Rasterize habitat with persistent climate and primary open habitat.
# ggplot points can't be as small as you want.

ras.persistent.open <- rasterFromXYZ(primary[,c("x", "y","lgm")]) 
plot(ras.persistent.open, col=c("red", "blue"))

# But the raster plot still looks similar to point ggplot.




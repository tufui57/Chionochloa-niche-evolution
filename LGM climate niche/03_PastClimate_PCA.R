########################################
### PCA drawing
########################################

library(dplyr)
library(extrafont)
library(ggplot2)
library(gridExtra)
library(grid)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)

# Choose past data
time <- #"lig_30s_bio"
  "mrlgmbi_2-5m" # LGM data is 2.5 arc min (4.5km at the equator)
#"ccmidbi_30s"

# Resolution of rasters
reso = 5

# Worldclim version
worldclim=1

### Choose bioclim variables. Use the following bioclim variables to draw PCA.
#"bioclim1", "bioclim6", "bioclim12", "bioclim15"
vars <- c(1,6,12,15)

genus_name <- "Acaena"

# Genus tag
if(genus_name == "Chionochloa"){
  genus_tag <- "chion"
}

if(genus_name == "Acaena"){
  genus_tag <- "acaena"
}

########################################
### Data preparation
########################################

# LGM climate
load(paste(".\\LGM_mainisland_worldclim",
           worldclim, "_", reso, "km_scores.data", sep = "")
)
# Current climate
load(paste(".\\Scores_", genus_tag,"_landcover_worldclim",
           worldclim, "_", reso, "km.data", sep = ""))

# Add cell ID
scores$cellID <- 1:nrow(scores)


########################################################################################################################
### Definition of persistent climate has changed. So, change the following object too. 
########################################################################################################################
# # Persistent climate of open habitat; persistent climate in primary open habitat
# persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 

# Primary open habtat
primary <- scores[scores[, "landCoverChange"] == "nonF-nonF", ]


#################################################################################
### Plot Climate space
#################################################################################

#################################
### Plot current Climate space 
#################################
pMain <- ggplot() +
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  ylim(c(-2, 5)) +
  xlim(c(-4, 6)) +
  # ggtitle("Current climate") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # legend position inside plot at topleft
  theme(panel.background =  element_blank()
  )

ggsave("Y://current.png", pMain, width = 100, height = 80, units = "mm")

#################################
### Plot LGM Climate space 
#################################
pMain2 <- ggplot() +
  geom_point(data = newdf, aes(PC1, PC2), color = 'lightpink', alpha = 0.25) +
  ylim(c(-2, 5)) +
  xlim(c(-4, 6)) +
  # ggtitle("LGM climate") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # legend position inside plot at topleft
  theme(panel.background =  element_blank()
  )

ggsave(paste("Y://",time, ".png", sep = ""), pMain, width = 100, height = 80, units = "mm")

##################################################################
### Plot primary open habitat and current climate space 
##################################################################

p <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  # point of each sp
  geom_point(data = primary, aes(PC1, PC2), color = 'blue', alpha = 0.05) +
  # ggtitle("Current climate and climate of primary open areas") +
  ylim(c(-2, 5)) +
  xlim(c(-4, 6)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # legend position inside plot at topleft
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

ggsave("Y://current_primaryOpenHabitat.png", p, width = 100, height = 80, units = "mm")

##################################################################
### Plot LGM climate and primary open areas
##################################################################

p2 <- ggplot() +
  # plot all NZ data points
  geom_point(data = newdf, aes(PC1, PC2), color = 'lightpink', alpha = 0.25) +
  geom_point(data = primary, aes(PC1, PC2), color = 'blue', alpha = 0.05) +
  # ggtitle("LGM cimate and climate of primary open areas") +
  ylim(c(-2, 5)) +
  xlim(c(-4, 6)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # legend position inside plot at topleft
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

ggsave(paste("Y://", time, "_primaryOpenArea.png", sep = ""), p2, width = 100, height = 80, units = "mm")



##################################################################
### Plot all the above in one panel
##################################################################

png("Y://curernt_LGM_primaryOpen_climateSpace2.png", width = 800, height = 670)

# Plot in multiple panels
grid.arrange(pMain, pMain2, p, p2, nrow = 2)

dev.off()

##################################################################
### Plot climates with primary open ares in one panel
##################################################################

png("Y://curernt_LGM_primaryOpen_climateSpace3.png", width = 400, height = 670)

# Plot in multiple panels
grid.arrange(p, p2, nrow = 2)

dev.off()
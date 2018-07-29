###################################################
### Schoener's D for multiple variables
###################################################
## Basic  Schoener's D is calculated for single variable. 
## But here, I want to calculate Shoener's D for multiple variables.

library(ggplot2)
source("Y:\\R scripts\\1 Acaena project\\Modified\\06_2_function_NicheOverlap_EnvSpace_Map.R")

# Data import
da1 <- read.csv("Y:\\2nd chapter_phylogentic niche conservation\\meta data\\Chionochloa_bioclim_landcover_history_inclNAonland.csv")
d <- da1[is.na(da1$landCoverChange) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

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


#########################################################################################
## Plot environmental space with schoener D value and histgrams of each axis
#########################################################################################

######################################################
### Plot occurrence records of each species
######################################################

## Plot by gglot multipanel
lapply(sname[c(1:16, 18:20, 22:length(sname))], function(i){
  envPlot_sp(scores, i, 9999, #schoener D
             save=T
             )
  })

############################################################################################################
##   Environmental space of occurrence records in priamry and secondary open area
############################################################################################################

allspPlot <- function(al, # Dataframe with occurrence records of target species
                      spname, 
                      D = 9999, # Schoenner's D between primary and secondary open habitat of target species
                      extent_x, extent_y, 
                      save = TRUE) {
  
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = al, aes(PC1, PC2, colour = landCoverChange), alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    # change point colour and legend title and texts
    scale_colour_manual(
      # title
      name = paste("Schoener's D =", D, "\nN =", nrow(al)),
      # Name of each legend factor. 
      # This must be same factors as factors in "colname" of ggplot(aes(colour = colname)), otherwise no legend will be drawn.
      breaks = c("nonF-nonF", "NF-nonF"),
      # Change name of points
      labels = c("Primary open area", "Secondary open area"),
      # colours
      values = c("red", "blue")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape=16, alpha=0.7))) +
    # legend position inside plot
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),
          panel.background = element_rect(fill = 'gray96')
    )
  
  pTop <- ggplot(al, aes(x = PC1)) +
    geom_histogram(data = subset(al, landCoverChange == 'NF-nonF'), fill = "red", alpha = 0.35) +
    geom_histogram(data = subset(al, landCoverChange == 'nonF-nonF'), fill = "blue", alpha = 0.35) +
    xlim(extent_x) +
    xlab(expression(hot %<->% cold)) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(""),
          axis.ticks.x = element_blank()
    ) +
    ggtitle(paste("Environmental space of", spname)) +
    theme(panel.background = element_rect(fill = 'gray96'))
  
  
  pRight <- ggplot(al, aes(x = PC2)) +
    geom_histogram(data = subset(al, landCoverChange == 'NF-nonF'), fill = "red", alpha = 0.35) +
    geom_histogram(data = subset(al, landCoverChange == 'nonF-nonF'), fill = "blue", alpha = 0.35) +
    xlim(extent_y) +
    xlab(expression(dry %<->% wet)) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_text(angle = 270),
          axis.ticks.y = element_blank()
    ) +
    coord_flip() +
    theme(panel.background = element_rect(fill = 'gray96'))
  
  pEmpty <- ggplot(scores, aes(PC1, PC2)) +
    geom_blank() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          line = element_blank(),
          panel.background = element_blank())
  
  if (save == TRUE) {
    png(filename = paste("Y:\\dist_", spname, "_EnvSpace.png"), width = 900, height = 630)
    # change font size
    theme_set(theme_gray(base_size = 18))
    # Plot in multiple panels
    grid.arrange(pTop, pEmpty, pMain, pRight,
                 ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
    dev.off()
  } else {
    res <- list(pMain, pTop, pRight, pEmpty)
    return(res)
  }
  
}

######################################################
### Plot occurrence records of all species
######################################################
dnz <- scores[rowSums(scores[, sname]) != -18,]

# old
ol <- dnz[dnz[, "landCoverChange"] == "nonF-nonF", c("PC1", "PC2", "landCoverChange")]
# new
ne <- dnz[dnz[, "landCoverChange"] == "NF-nonF", c("PC1", "PC2", "landCoverChange")]
# new & old
al <- rbind(ol, ne)

allspPlot(al, "all studied Chionochloa species", 9999, extent_x, extent_y, save = T)

######################################################
### Plot occurrence records of each species
######################################################

SeparatedPrimarySecondary <- function(ne, ol, spname, extent_x, extent_y, save = TRUE) {
  
  pPrimary <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = ol, aes(PC1, PC2), colour = "blue", size=0.5, alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    # legend position inside plot
    theme(legend.position = "none",
          panel.background = element_rect(fill = 'gray96')
    )
  
  pSecondary <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = ne, aes(PC1, PC2), colour = "red", size=0.5, alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    # legend position inside plot
    theme(legend.position = "none",
          panel.background = element_rect(fill = 'gray96')
    )
  
  if (save == TRUE) {
    png(filename = paste("Y:\\", spname, "_separated_EnvSpace.png", sep=""), width = 400, height = 600)
    # change font size
    theme_set(theme_gray(base_size = 18))
    # Plot in multiple panels
    grid.arrange(pPrimary, pSecondary,
                 ncol = 1, nrow = 2)
    dev.off()
  } else {
    res <- list(pPrimary, pSecondary)
    return(res)
  }
}

SeparatedPrimarySecondary(ne, ol, "all studied Chionochloa species", extent_x, extent_y, save = TRUE)

rm(al,ol,ne)

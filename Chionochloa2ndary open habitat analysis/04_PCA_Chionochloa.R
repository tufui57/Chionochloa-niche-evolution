library(gridExtra)
library(ggplot2)

# Data import
alld <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

################################################################################
### Count of occurrence records of Chinochloa
# write.csv(colSums(d[, 23:57], na.rm = T), file = "Y:\\occRecords.csv")

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

# Replace NA with 0
for(i in sname){d[is.na(d[,i]),i] <- 0}

# Species that doesn't have < 5 records can't be used in this analysis.
sname2 <- sname[!(sapply(sname, function(i){sum(d[,i]) < 5}))]

# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)

scores <- data.frame(d[, c(paste("bioclim", c(1, 6, 12, 15), sep = ""), "x", "y", sname2)], pca$x[, 1:2])

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

############################################################################################################
##   Environmental space of Acaena occurrences in priamry and secondary open area
############################################################################################################

spname=sname[1]

PCAplot <- function(spname, extent_x, extent_y, save = TRUE) {
  al <- scores[scores[, spname] == 1,]
  
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of each sp
    geom_point(data = al, aes(PC1, PC2), alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    # legend position inside plot
    theme(legend.justification = c(1, 1), legend.position = c(1, 1),
          panel.background = element_rect(fill = 'gray96')
    )
  
  pTop <- ggplot(al, aes(x = PC1)) +
    geom_histogram(data = al, fill = "red", alpha = 0.35) +
    xlim(extent_x) +
    #xlab(expression(hot %<->% cold)) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(""),
          axis.ticks.x = element_blank()
    ) +
    ggtitle(paste("Environmental space of", spname)) +
    theme(panel.background = element_rect(fill = 'gray96'))
  
  
  pRight <- ggplot(al, aes(x = PC2)) +
    geom_histogram(data = al, fill = "red", alpha = 0.35) +
    xlim(extent_y) +
    #xlab(expression(dry %<->% wet)) +
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
    png(filename = paste("Y:\\", spname, "_EnvSpace.png"), width = 900, height = 630)
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

# Plots
lapply(sname2, PCAplot, extent_x, extent_y)

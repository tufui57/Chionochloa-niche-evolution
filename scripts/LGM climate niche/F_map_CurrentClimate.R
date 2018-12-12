###################################################################################
#############   Plot past climate data   #######################################
###################################################################################
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)

genus_name = "Chionochloa"

# Load current climate data

alld <- read.csv(paste("Y:\\", genus_name, "_bioclim_landcover_history_worldclim1_1km.csv",
                       sep="")
)
d <- alld[is.na(alld$bioclim1) == F, ]

### Plot

map_plot <- function(data, # data for map
                     legend.title, # Legend title (if the column name is not what I want to show on the map)
                     cols, # vector of colour anmes c(low.colour, high.colour) 
                     colname.to.draw # character string. Colname of the data to colour
){
  
  pMap <- ggplot() +
    geom_raster(data = data, aes_string(x = "x", y = "y", fill = colname.to.draw)) +
    
    scale_fill_gradient(legend.title, low = cols[1], high = cols[2],
                        na.value = "white"
                        ) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    
    labs(x = "", y = "") +
    # Legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          plot.title = element_text(family = "Times New Roman", size = 20),
          #legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background =  element_blank(), #element_rect(fill = 'gray96'),
          #axis.text.y = element_text(angle = 90, hjust = 0.5)
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(pMap)
  
}

### Histogram
hist_plot <- function(data, # data for niche space
                      colname.to.draw # character string. Colname of the data to colour
) {
  
  pTop <- ggplot(data, aes_string(x = colname.to.draw)) +
    geom_histogram(col = "black", fill = "lightblue") +
    theme(axis.title = element_text(size = 15),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = 'gray96'))
  
  return(pTop)
}


#############   Plot each climate variable   #######################################

### Extract coordinates and cliamte to plot
d2 <- d[, c("x", "y", "bioclim1", "bioclim6", "bioclim12", "bioclim16")]

### BIOCLIM temperature is actual values x 10 °C
d2$bioclim1 <- d2$bioclim1 * 0.1

### Bioclim 1
bi1 <- map_plot(d2[, c("x", "y", "bioclim1")], "(°C)", c("lightblue", "red"), "bioclim1")
# Hist
bi1.hist <- hist_plot(d2[, c("x", "y", "bioclim1")], "bioclim1")
# Title
title1 <- textGrob("Annual mean temperature", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio1.png", sep=""), width = 800, height = 600)
grid.arrange(bi1, bi1.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()




### Bioclim 6
bi6 <- map_plot(d2[, c("x", "y", "bioclim6")], "(°C)", c("lightblue", "red"), "bioclim6")
# Hist
bi6.hist <- hist_plot(d2[, c("x", "y", "bioclim6")], "bioclim6")
# Title
title1 <- textGrob("Minimum temperature of coldest month", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio6.png", sep=""), width = 800, height = 600)
grid.arrange(bi6, bi6.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()



### Boiclim 12
bi12 <- map_plot(d2[, c("x", "y", "bioclim12")], "(mm)", c("tan2", "blue"), "bioclim12")
# Hist
bi12.hist <- hist_plot(d2[, c("x", "y", "bioclim12")], "bioclim12")
# Title
title1 <- textGrob("Annual precipitation", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio12.png", sep=""), width = 800, height = 600)
grid.arrange(bi12, bi12.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()



### Boiclim 16
bi16 <- map_plot(d2[, c("x", "y", "bioclim16")], "", c("tan2", "brown"), "bioclim16")
# Hist
bi16.hist <- hist_plot(d2[, c("x", "y", "bioclim16")], "bioclim16")
# Title
title1 <- textGrob("Precipitation seasonality", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio16.png", sep=""), width = 800, height = 600)
grid.arrange(bi16, bi16.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()





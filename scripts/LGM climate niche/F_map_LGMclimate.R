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

# Load LGM climate data
load(".//LGMclimate.data")

### Plot

map_plot <- function(data, # data for map
                     legend.title, # Legend title (if the column name is not what I want to show on the map)
                     cols, # vector of colour anmes c(low.colour, high.colour) 
                     colname.to.draw # character string. Colname of the data to colour
){
  
  pMap <- ggplot() +
    geom_raster(data = data, aes_string(x = "x", y = "y", fill = colname.to.draw)) +
    
    scale_fill_gradient(legend.title, low = cols[1], high = cols[2],
                        na.value = "white") +
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

### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[1]]), values(lgm.mainland[[1]])* 0.1))
colnames(rasd)[3] <- "Climate"
# Plot
bi1 <- map_plot(rasd, "(°C)", c("lightblue", "red"), "Climate")
# Hist
bi1.hist <- hist_plot(rasd, "Climate")
# Title
title1 <- textGrob("Annual mean temperature", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio1.png", sep=""), width = 800, height = 600)
grid.arrange(bi1, bi1.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()



### Convert raster to dataframe for plotting with ggplot
rasd6 <- data.frame(cbind(coordinates(lgm.mainland[[2]]), values(lgm.mainland[[2]])* 0.1))
colnames(rasd6)[3] <- "Climate"
# Plot
bi6 <- map_plot(rasd6, "(°C)", c("lightblue", "red"), "Climate")
# Hist
bi6.hist <- hist_plot(rasd6, "Climate")
# Title
title1 <- textGrob("Minimum temperature of coldest month", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio6.png", sep=""), width = 800, height = 600)
grid.arrange(bi6, bi6.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()



### Convert raster to dataframe for plotting with ggplot
rasd12 <- data.frame(cbind(coordinates(lgm.mainland[[3]]), values(lgm.mainland[[3]])))
colnames(rasd12)[3] <- "Climate"
#plot
bi12 <- map_plot(rasd12, "(mm)", c("tan2", "blue"), "Climate")
# Hist
bi12.hist <- hist_plot(rasd12, "Climate")
# Title
title1 <- textGrob("Annual precipitation", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio12.png", sep=""), width = 800, height = 600)
grid.arrange(bi12, bi12.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()

### Convert raster to dataframe for plotting with ggplot
rasd16 <- data.frame(cbind(coordinates(lgm.mainland[[4]]), values(lgm.mainland[[4]])))
colnames(rasd16)[3] <- "Climate"
#plot
bi16 <- map_plot(rasd16, "", c("tan2", "brown"), "Climate")
# Hist
bi16.hist <- hist_plot(rasd16, "Climate")
# Title
title1 <- textGrob("Precipitation seasonality", gp=gpar(fontface="bold", cex=1.75))

png(paste("Y://bio16.png", sep=""), width = 800, height = 600)
grid.arrange(bi16, bi16.hist, top = title1, ncol = 2, widths = c(1,1))
dev.off()





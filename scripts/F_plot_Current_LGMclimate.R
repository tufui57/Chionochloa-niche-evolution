# Load LGM climate data
load(".//LGMclimate.data")


map_plot <- function(data, # data for map
                     colname.to.draw # character string. Colname of the data to colour
){
  
  pMap <- ggplot() +
    geom_raster(data = data, aes_string(x = "x", y = "y", fill = colname.to.draw)) +

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

################ Annual mean temperature
# Load current climate data
alld <- read.csv("Y:\\Chionochloa_bioclim_landcover_history_worldclim1_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]
### Extract coordinates and cliamte to plot
d2 <- d[, c("x", "y", "bioclim1", "bioclim6", "bioclim12", "bioclim16")]
### BIOCLIM temperature is actual values x 10 °C
d2$bioclim1 <- d2$bioclim1 * 0.1
### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[1]]), values(lgm.mainland[[1]])* 0.1))
colnames(rasd)[3] <- "bioclim1"
# vector of colour anmes c(low.colour, high.colour) 
cols = c("blue","white",  "red")

### Plot
bi1_lgm <- map_plot(rasd, "bioclim1")

bi1_lgm <- bi1_lgm + 
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                       na.value = "white",
                       limits = c(-3, 17)
  )



### Plot
bi1_p <- map_plot(d2[, c("x", "y", "bioclim1")], "bioclim1")

bi1_p <- bi1_p + 
  scale_fill_gradient2("(°C)", low = cols[1], mid = cols[2], high = cols[3],
                      na.value = "white",
                      limits = c(-3, 17)
  )


png(paste("Y://bio1.png", sep=""), width = 1200, height = 600)
grid.arrange(bi1_lgm, bi1_p, top = title1, ncol = 2, widths = c(1,1))
dev.off()


################ Annual precipitation

### Convert raster to dataframe for plotting with ggplot
rasd <- data.frame(cbind(coordinates(lgm.mainland[[3]]), values(lgm.mainland[[3]])* 0.1))
colnames(rasd)[3] <- "bioclim12"

min(rasd$bioclim12, na.rm = T)
max(rasd$bioclim12, na.rm = T)


# Plot
bi12_lgm <- map_plot(rasd, "bioclim12")

# vector of colour anmes c(low.colour, high.colour) 
cols = c("blue", "red")
bi12_lgm <- bi12_lgm + 
  scale_fill_gradient("(mm)", low = cols[1], high = cols[2],
                       na.value = "white",
                       limits = c(30, 6000)
  )


min(d2$bioclim12, na.rm = T)
max(d2$bioclim12, na.rm = T)
### Plot
bi12_p <- map_plot(d2[, c("x", "y", "bioclim12")], "bioclim12")
bi12_p <- bi12_p + 
  scale_fill_gradient("(mm)", low = cols[1], high = cols[2],
                       na.value = "white",
                       limits = c(30, 6000)
  )

png(paste("Y://bio12.png", sep=""), width = 1200, height = 600)
grid.arrange(bi12_lgm, bi12_p, top = "Annual precipitation", ncol = 2, widths = c(1,1))
dev.off()

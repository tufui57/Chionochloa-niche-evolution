###############################################################
## Data preparation 
###############################################################

genus_name = "Acaena"

setwd("C://Users//nomur//Documents")
source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")

###############################################################
## Plot map and niche space 
###############################################################

lapply(seq(1,length(spname),3), function(i){
  
  png(paste("Y://monocolour_niche_map_3sp", spname[i], ".png", sep=""), width = 800, height = 1300)
  
  pMain1 <- niche_plot_monoColour(spname[i], scores)
  pMap1 <- map_plot_monoColour(spname[i], chdata)
  title1 <- textGrob(gsub("_", " ", spname[i]), gp = gpar(fontface = "bold", cex = 1.75))
  spi <- grid.arrange(pMap1, pMain1, top = title1, ncol = 2, widths = c(3,5))
  
  if(i+1 > length(spname)){
    spi2 <- ggplot(scores, aes(PC1, PC2)) +
      geom_blank() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            line = element_blank(),
            panel.background = element_blank()
      )
  }else{
    pMain2 <- niche_plot_monoColour(spname[i+1], scores)
    pMap2 <- map_plot_monoColour(spname[i+1], chdata)
    title2 <- textGrob(gsub("_", " ", spname[i+1]), gp=gpar(fontface="bold", cex=1.75))
    spi2 <- grid.arrange(pMap2, pMain2, top = title2, ncol = 2, widths = c(3,5))
  }
  
  if(i+2 > length(spname)){
    spi3 <- ggplot(scores, aes(PC1, PC2)) +
      geom_blank() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            line = element_blank(),
            panel.background = element_blank()
      )
  }else{
    pMain3 <- niche_plot_monoColour(spname[i+2], scores)
    pMap3 <- map_plot_monoColour(spname[i+2], chdata)
    title3 <- textGrob(gsub("_", " ", spname[i+2]), gp=gpar(fontface="bold", cex=1.75))
    spi3 <- grid.arrange(pMap3, pMain3, top = title3, ncol = 2, widths = c(3,5))
    
    }

  # Plot in multiple panels
  grid.arrange(spi, spi2, spi3, nrow = 3)
  
  dev.off()
  
})

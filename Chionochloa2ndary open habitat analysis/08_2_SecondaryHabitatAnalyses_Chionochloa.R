########################################################################################
### Analyses for proportion of secondary open habitat - species age
########################################################################################

# Import data
d <- read.csv("Y://Chionochloa_data_analyses.csv")

library(ggplot2)
plotAnalysis <- function(data, 
                         m, # linear model object
                         xv, yv, # column names of responce and explanatory variable
                         xlabname, ylabname, # axes names for plot
                         showStats = T # TRUE; Show p value and slope of linear model and colour points, FALSE; No stat values and black points
){
  
  if(showStats == T){
    myplot <- ggplot(data, aes_string(x = xv, y = yv)) +
      geom_point() +
      # change xy labels
      labs(x = xlabname, y = ylabname) +
      # change text size
      theme(text = element_text(size = 20),
            axis.text.x = element_text(size = 20)) +
      # drow LM line & confident intervals 
      stat_smooth(method = "lm", col = "red") +
      # show stats result as title
      labs(title = paste("Adj R2 =", signif(summary(m)$adj.r.squared, digits = 2),
                         "Intercept =", signif(m$coef[[1]], 2),
                         " Slope =", signif(m$coef[[2]], 2),
                         " P =", signif(summary(m)$coef[2, 4], 2))) +
      theme(panel.background = element_rect(fill = "gray95"))
  } else {
    myplot <- ggplot(data, aes_string(x = xv, y = yv)) +
      geom_point() +
      # change xy labels
      labs(x = xlabname, y = ylabname) +
      # change text size
      theme(text = element_text(size = 20),
            axis.text.x = element_text(size = 20)) +
      # drow LM line & confident intervals 
      stat_smooth(method = "lm", col = "red") +
      theme(panel.background = element_rect(fill = "gray95"), legend.position="none")
  }
  return(myplot)
}

#########################################################################
### Proportion of Secondary Open Habitat - Preference for Open Habitat
#########################################################################

m <- lm(proportionSecondaryHabitat ~ PreferenceOpen, d)

myplot <- plotAnalysis(data=d, m=m, xv = "PreferenceOpen", yv = "proportionSecondaryHabitat", showStats = T,
                       xlabname = expression("closed" %<-% "Preference for open habitat" %->% "open"), ylabname = "Proportion of secondary open habitat")

myplot2 <- myplot + 
  xlim(0, 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed")

# save
ggsave(paste("Y:\\preferenceOpen_proportionSecondary_nolegend.png", sep = ""), plot = myplot2,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

########################################################
###  Proportion of Secondary Open Habitat - current range size
########################################################

m <- lm(proportionSecondaryHabitat ~ log10.total, d)

myplot <- plotAnalysis(d, m, xv = "log10.total", yv = "proportionSecondaryHabitat",showStats = T,
                       xlabname = "log10(Current range size)", ylabname = "Proportion of secondary open habitat")


# save
ggsave(paste("Y:\\log10CurrentRangeSize_proportionSecondary.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


#########################################################################
### Proportion of Secondary Open Habitat ~ niche volume
#########################################################################

m <- lm(proportionSecondaryHabitat ~ corrected.D, d)

myplot <- plotAnalysis(data=d, m=m, xv = "proportionSecondaryHabitat", yv = "corrected.D", showStats = T,
                       xlabname = "Proportion of secondary open habitat", ylabname = "Species niche volume")

# save
ggsave(paste("Y:\\chionochloa_proportionSecondary_spNicheVolume.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


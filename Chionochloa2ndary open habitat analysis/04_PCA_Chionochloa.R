library(gridExtra)
library(ggplot2)
source(".//functions//F_calculate_PCAscores.r")
source(".//functions//F_plot_PCA.r")

# Data import
load(".//scores_chion.data")

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

############################################################################################################
##   Environmental space of occurrences in priamry and secondary open area
############################################################################################################

spname = colnames(scores)[grep("^Chion", colnames(scores))]

# Plots
lapply(spname, function(i){
  pMain <- PCAplot(scores, scores[scores[,i]==1,], extent_x, extent_y)
  PCA_withHist(scores, pMain, spname = i, save = TRUE)
  
})

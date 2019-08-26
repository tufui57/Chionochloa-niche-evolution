###################################################
### Plot PCA of genus
###################################################

library(ggplot2)
library(gridExtra)
source(".//functions//F_plot_PCA.r")
genus_name <- "Chionochloa"

if(genus_name == "Acaena"){
  genus_tag = "acaena"
}else{
  genus_tag = "chion"
    }

load(paste(".//scores_", genus_tag, ".data", sep = ""))

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

# Create genus PCA score
scores.genus <- rowSums(scores[, colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))]], na.rm = TRUE)

scores.genus2 <- scores[(scores.genus >= 1),]

# Plot PCA
pMain <- PCAplot(scores, scores.genus2, col = "blue", extent_x, extent_y) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


PCA_withHist(scores.genus2, spname = genus_name,  histColour = "blue", pMain = pMain)


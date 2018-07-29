########################################################################################
### Quantification of niche volume
########################################################################################

### Original method in ecospat to calculate Schonner's D
library(ecospat)
library(nichePlot)
source(".//functions//F_SchonnerDdataframeFormat.R")

########################################################################################
### Chionochloa
########################################################################################

# Load PCA scores
load(".//Scores_chion.data")

# sp names
sname <- colnames(scores)[grepl("^Chion", colnames(scores))]
# Species that doesn't have < 5 records can't be used in this analysis.
sname2 <- sname[!(sapply(sname, function(i){sum(scores[,i]) < 5}))]

D <- lapply(sname2, function(i){
            SchoenerD_ecospat(background = scores, "PC1", "PC2",
                              data1 = scores[scores[, i] == 1, ],  data2 = scores)
  }
            )
names(D) <- sname
D2 <- SchonnerDdataframeFormat(scho = D)

write.csv(d6, "Y:\\Chionochloa_nicheVolume.csv")



# Persistent climate
load(paste(".//currentNicheSimilarToLGM_", a,"_", reso, "km.data", sep = ""))

### Add climate similarity to the LGM to current climate data frame
# neighbours$dat2cellID has cell ID of "scores" object. 
# SO, rows of "scores" whose cell ID are in "neighbours$dat2cellID" are the cells with similar climate to the LGM.

scoresLGM <- mutate(scores, lgm = ifelse(scores$cellID %in% neighbours$dat2cellID, 1, 0))

# Persistent climate; current climate similar to the LGM. 
scoresLGM2 <- scoresLGM[scoresLGM$lgm == 1, ]

########################################################################################################################
### Definition of persistent climate has changed. So, change the following object too. 
########################################################################################################################
# # Persistent climate of open habitat; persistent climate in primary open habitat
# persistentOpenHabitat <- scoresLGM2[scoresLGM2[,"landCoverChange"] == "nonF-nonF", ] 



#################################
### Plot persisitent Climate
#################################
pMain <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = scoresLGM2, aes(PC1, PC2), color = 'yellow', alpha = 0.25) +
  ggtitle("Persistent")

ggsave(paste("Y://", time, "_persistentClimate.png", sep = ""), pMain, width = 100, height = 80, units = "mm")

##################################################################
### Plot persistent climate space Where has been available since LGM 
##################################################################

p2 <- ggplot() +
  # plot all NZ data points
  geom_point(data = scores, aes(PC1, PC2), color = 'gray80', alpha = 0.25) +
  geom_point(data = persistentOpenHabitat, aes(PC1, PC2), color = 'red', alpha = 0.25) +
  ggtitle("Persistent climate of open habitat")

ggsave(paste("Y://", time, "_persistentOpenHabitat.png", sep = ""), p2, width = 100, height = 80, units = "mm")

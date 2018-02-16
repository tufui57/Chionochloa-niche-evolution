############################################################################################################
###################            Landcover change data formatting             ################################
############################################################################################################

### Import data
res <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")

##############################################################################################
### Categorize land cover classes and create land cover change history
##############################################################################################


# Land cover classes; NF, non-forest, EF,non potential habitat
# You'd better add numbers instead of characters, because of huge data size
landCoverChange <- function(dat, 
                            prehuman_landcover, # Column name of pre-human land cover
                            current_landcover # Column name of current land cover
){
  
  # Make column of pre-human land cover
  dat$preLandcover <- sapply(dat[,prehuman_landcover], 
                             function(x){
                               ifelse(x %in% 1:20, 1, #"forest" No=1:20
                                      ifelse(x %in% c(0,21:25), 2, #"nonForest" No=0,21:25
                                             NA
                                      )
                               )
                             }
  )
  
  # Make column of current land cover
  dat$currentLandcover <- sapply(dat[,current_landcover], 
                                 function(x){
                                   ifelse(x %in% c(54,69), 1, #"nativeForest" 
                                          ifelse(x %in% c(64,68,71), 2, #"exoticForest"
                                                 ifelse(x %in% c(15,40,41,43,44,10,12,16,47,50,51,52,55,56,58), 3, #"nonForest" incl. shrubland and gravely habita
                                                        ifelse(x %in% c(0,1,2,5,6,14,20,21,22,30,33,45,46), 4,  #"nonPotentialAcaenaHabitat"
                                                               NA
                                                        ))))
                                 }
  )
  
  # Make column of land cover change history
  dat$landCoverChange <- ifelse(dat$preLandcover == 1 & dat$currentLandcover == 1, "NF-NF",
                                ifelse(dat$preLandcover == 1 & dat$currentLandcover == 2, "NF-EF",
                                       ifelse(dat$preLandcover == 1 & dat$currentLandcover == 3, "NF-nonF",
                                              ifelse(dat$preLandcover == 1 & dat$currentLandcover == 4, "NF-nonPotentialHabitat",
                                                     ifelse(dat$preLandcover == 2 & dat$currentLandcover == 3, "nonF-nonF",
                                                            ifelse(dat$preLandcover == 2 & dat$currentLandcover == 2, "nonF-EF",
                                                                   ifelse(dat$preLandcover == 2 & dat$currentLandcover == 1, "nonF-NF", "nonF-nonPotentialHabitat")
                                                            )
                                                     )
                                              )
                                       )
                                )
  )
  return(dat)
}

d <- landCoverChange(res, prehuman_landcover="pre.human_landcover1km", current_landcover="current_landcover1km")

# Delete NA rows of BIOCLIM data
d2 <- d[is.na(d$bioclim1)==F, ]

write.csv(d2, file = "Y://Chionochloa_bioclim_landcover_history_inclNAonland.csv")


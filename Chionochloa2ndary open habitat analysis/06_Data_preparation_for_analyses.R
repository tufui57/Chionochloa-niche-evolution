############################################################################################################
#########                   Analyses of geographical land cover change                        ###############
############################################################################################################
## 
## Plots & simple variance analyses and LMs
##
############################################################################################################

library(dplyr)
source("Y://R scripts//Scripts for Practice and ones written by someone else//cbind_fill.r")

########################################################
##### Make summary of land cover change history
########################################################

### Summary of pre-human land cover for each sp
dat <- read.csv("Y:\\Chionochloa_bioclim_landcover_history_inclNAonland.csv")
dat <- dat[is.na(dat$landCoverChange) == F, ]

# sp names
spname <- grepl("^Chion", colnames(dat)) %>% colnames(dat)[.]
# C. flavicans f. temata, C. nivifera have no occurrence in primary and/or secondary habitats.
spname <- spname[ - c(17, 21)]

### Make summary tables of land cover for each species occurrences

r <- list()

for(i in spname){
  spcol <- colnames(d)[(colnames(dat) %in% spname)==F]
  
  sd <- dat[is.na(dat[,i]) == F, spcol]
  # Table of c(number of occurrences in pre-human landcover, current landcover, unfeasible land cover change)
  prel <- c(nrow(sd), sum(sd[, "preLandcover"] == 1 & sd[, "currentLandcover"] == 4), table(sd[, "preLandcover"]))
  
  if(length(prel) < 4){
    # The species has no records in pre-human forest
    if(names(prel)[3] == "1"){
      prel["2"] <- 0
    }else{
      # The species has no records in pre-human non-forest
      prel["1"] <- 0
    }
  }
  
  names(prel) <- c("number of 1km occurrence cells", "NF-nonPotentialHabitat", "pre-NF", "pre-nonF")
  a <- c(prel, table(sd[, "currentLandcover"]))
  r[[i]] <- a
    }

# Fill NA columns
spsum <- cbind.fill(r)
colnames(spsum) <- spname
rownames(spsum)[5:8] <- c("current-NF", "current-exoticForest","current-nonF", "current-nonPotential")

# Table of Land cover change history
landcover <- lapply(s, function(i){
  spcol <- colnames(dat)[(colnames(dat) %in% spname)==F]
  sd <- dat[is.na(dat[,i]) == F, spcol]
  c(nrow(sd), table(sd$landCoverChange))
})

landcover2 <- cbind.fill(landcover)
colnames(landcover2) <- spname
rownames(landcover2)[1] <- "Occurrence1kmCells"

write.csv(t(landcover2),"Y://Chionochloa_landscapeChangeHistory.csv")

########################################################
### Calclation of indices for analysis
########################################################

# Land cover change data
gd <- read.csv("Y://Chionochloa_landscapeChangeHistory.csv")
# species age
ages <- read.csv("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\Chionochloa\\species_ages_chionochloa.csv")
# Niche volume
vol <- read.csv("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\Chionochloa\\Chionochloa_nicheVolume.csv")

# Modify species name of species age dataframe
a <- gsub("subsp", "subsp.", ages$X)
# Chionochloa flaviscans in phylogeny tree and Chionochloa flaviscans f. flaviscans in occurrence records are treated as the same species?
ages$X <- gsub("Chionochloa_flavicans", "Chionochloa_flavicans_f._flavicans", gsub("var", "var.", a))



# Add land cover type "others"
gd$nonF.others <- (gd$nonF.EF + gd$nonF.nonPotentialHabitat)
gd$NF.others <- (gd$NF.EF + gd$NF.nonPotentialHabitat)

# Replace NA with 0
gd[is.na(gd$NF.NF)] <- 0

geoIncTable <- function(d, filepath
) {
  ### Calculate habitats increase
  
  # Absolute increase = occurrence cells in new habitat
  d$absoluteIncrease <- d$NF.nonF
  
  # Proportion of secondary open habitat = occ in secondary habitat / occ in primary and secondary habitat
  d$proportionSecondaryHabitat <- (d$NF.nonF / (d$NF.nonF + d$nonF.nonF))
  
  # total = no. of all occurrence cells
  d$total <- d$Occurrence1kmCells
  d$log10.total <- log10(d$total)
  
  # Initial range size of all sp = occurrences in old habitat
  d$initialRangeSize <- d$nonF.nonF
  d$log10.initialRange <- log10(d$initialRangeSize)
  
  # Preference for open habitat = sum of occ in open (NF.nonF + nonF.nonF) / (forest + open)
  d$PreferenceOpen <- rowSums(d[, c("NF.nonF", "nonF.nonF")]) / rowSums(d[,c("NF.nonF", "NF.NF", "nonF.nonF", "nonF.NF")])
  
  d2 <- merge(d, merge(vol,ages))
  write.csv(d2[, c("X", "corrected.D", "x",
                  "total", "initialRangeSize", 
                  "proportionSecondaryHabitat", "absoluteIncrease", 
                  "PreferenceOpen",
                  "log10.initialRange", "log10.total"
  )],
  file = filepath)
  
  return(d)
}

geoIncTable(gd,
            "Y://Chionochloa_data_analyses.csv"
)

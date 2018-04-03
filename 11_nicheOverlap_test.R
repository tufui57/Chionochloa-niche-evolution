##############################################################################
### Data preparation
##############################################################################

# genus_name <- "Acaena"
genus_name <- "Chionochloa"
source(".//Chionochloa niche evolution//00_DataPreparation.R")

#library(EcoSimR)
library(ecospat)

data <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_1km.csv", sep = ""))


##############################################################################
### Test by ecospat
##############################################################################

niche.similarity_ecospat <-
  function(background,
           axis1, 
           axis2,
           data1, 
           data2,
           alternative = "greater",
           R = 100, # Resolution of background
           rep # times to repeat similarity test
  ) {
    
    background.clim <- background[, c(axis1, axis2)]
    
    # calculation of occurence density and test of niche equivalency and similarity
    z1 <- ecospat::ecospat.grid.clim.dyn(background.clim, background.clim, data1[ ,c(axis1, axis2)], R = 100)
    z2 <- ecospat::ecospat.grid.clim.dyn(background.clim, background.clim, data2[ ,c(axis1, axis2)], R = 100)
   
    ## test niche similarity
    res <- ecospat::ecospat.niche.similarity.test(z1, z2, rep = rep, alternative = alternative)
    ## Name
    return(res)
  }



sisterpair_niche.similarity_test <- function(sisterpairnode, # vector of node ID pair
         tree,
         background,
         axis1 = "PC1",
         axis2 = "PC2", 
         rep
         ){
  spname1 <- get_spname_from_nodeID(sisterpairnode[1],tree)
  spname2 <- get_spname_from_nodeID(sisterpairnode[2],tree)
  
  d1 <- background[background[, spname1]==1,]
  d2 <- background[background[, spname2]==1,]
  sim <- niche.similarity_ecospat(background, axis1, axis2, d1, d2, R = 500, rep = rep)
  sim.div <- niche.similarity_ecospat(background, axis1, axis2, d1, d2, alternative = "lower", R = 500, rep = rep)
  
  ecospat.plot.overlap.test(sim, "D", "Similarity")
  ecospat.plot.overlap.test(sim.div, "D", "Similarity")
  
  res<-list()
  res[[1]]<-sim
  res[[2]]<-sim.div
  
  return(res)
}

similar <- apply(sispairs, 1, sisterpair_niche.similarity_test, tree, scores, rep = 500)

save(similar, file = paste("Y://similaritytest_", genus_name, ".data", sep = ""))

for(i in 1:length(similar)){
  # Species node ID
    print(paste("species ID", names(similar)[i]))

  # Observed niche similarity I and D
    print(similar[[i]][[2]]$obs)
    
    # P value of the test if the two niches are more similar than random.
    print(paste("Conservatism; p value", similar[[i]][[1]]$p.D))
    # P value of the test if the two niches are less similar than random.
    print(paste("Divergence; p value", similar[[i]][[2]]$p.D))
  }


load(paste("Y://similaritytest_", genus_tag, ".data", sep = ""))

# Display mean p values of similarity test
sapply(similar, function(x){
  return(x[[1]]$p.D)
}) %>% 
  mean %>% 
  paste("Conservatism; Mean p-value over sister sepcies pairs;", .)


sapply(similar, function(x){
  return(x[[2]]$p.D)
}) %>% 
  mean %>% 
  paste("Divergence; Mean p-value over sister sepcies pairs;", .)


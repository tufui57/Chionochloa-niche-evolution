####################################################################
### Functions
####################################################################

### Add occurrence probability to the data frame of PCA scores
get_occurrenceProbability_to_scores <- function(i # spname
){
  
  # Get probability of the species
  prob1 <- pred[names(pred) == gsub("_",".", i)]
  
  prob.d <- data.frame(cbind(coordinates(prob1[[1]]), values(prob1[[1]])))
  colnames(prob.d)[3] <- paste("prob", i, sep = "_") 
  
  scores2 <- merge(prob.d, scores, by = c("x","y"))
  
  return(scores2)
  
}

### Rasterize PCA scores of probability
raster_from_dataframe <- function(prob){
  
  data <- prob[, c("PC1", "PC2", colnames(prob)[grep("^prob", colnames(prob))])]
  colnames(data)[1:2] <- c("x","y")
  
  # Extract target species occurrence records
  points <- data[,c("x", "y")]
  # Set coordinates
  coordinates(points) <- data[,c("x", "y")]
  # Put values in point object
  points$sp <- data[ ,colnames(prob)[grep("^prob", colnames(prob))]]
  
  # Prepare empty raster
  r <- raster(ncol = round((max(prob1$PC1)-min(prob1$PC1))/0.045),
              nrow = round((max(prob1$PC2)-min(prob1$PC2))/0.045)
  )
  extent(r) <- extent(points)
  
  # Rasterize
  sp_raster <- rasterize(points, r, points$sp, fun=mean)
  
  return(sp_raster)
}



######################################################################################
### Find points of a group within neighbourhood of another group of points 
######################################################################################

neighbours_within_a_squire <- function(dat1, # data of points to be searched
                                       dat2, # data of points which are centre of search area
                                       a, # half length (unit of the distance is the unit of coordinate) of the squire which you want to count the number of avairable secondary open cells.
                                       coordinateNames # column names for coordinates in dat1 and dat2
                                       ){
  # Add cell ID to dat2
  dat2$cellID <- 1:nrow(dat2)
  
  # Count the number of secondary open cells within squire of the distance "a"
  dat1_in_dat2area <- lapply(dat2$cellID, function(x){
    
    # Find dat1 points within dat1 point -a <= dat2 point <= dat1 point + a 
    datlat <- dat1[(dat1[, coordinateNames[2]] <= (dat2[x, coordinateNames[2]] + a) & dat1[, coordinateNames[2]] >= (dat2[x, coordinateNames[2]] - a)), ]
    datlatlon <- datlat[(datlat[, coordinateNames[1]] <= (dat2[x, coordinateNames[1]] + a) & datlat[, coordinateNames[1]] >= (dat2[x, coordinateNames[1]] - a)), ]
    
    # Name the group of dat1 points with cell ID of dat2  
    datlatlon$dat2cellID <- rep(x, nrow(datlatlon))
    return(datlatlon)
  }
  )
  
  # Merge all list into one data frame
  dat1_in_dat2area_dataframe <- do.call(rbind, dat1_in_dat2area)
  
  return(dat1_in_dat2area_dataframe)
}

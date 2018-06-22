
convert_occurrencePoints_to_raster <- function(data, # list object with occurrence data
                   refWGS # reference raster for coordinate system
                   ){
  
  # Extract target species occurrence records
  points <- data[,c("lon","lat")]
  # Set coordinates
  coordinates(points) <- data[,c("lon","lat")]
  # Put values in point object
  points$sp <- rep(1,nrow(data))
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  
  # Project points. Original coordinate system of BIOCLIM variables is WGS84
  sp_raster <- rasterize(points, refWGS, field = 1)
  
  # project raster from WGS84 to NZTM
  projected_raster <- projectRaster(sp_raster, crs = proj4stringNZTM)
  
  return(projected_raster)
}

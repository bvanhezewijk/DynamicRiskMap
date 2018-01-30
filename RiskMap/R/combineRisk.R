# combineRisk()
# Function to combine all risk layers into totalRisk 
# Agruments: projectList
# KSchurmann March 1, 2017
#
### species specific weighting and direction parameters??
#
#' Combine risk layers
#'
#' Combines risk rasters generated with \code{calculateRisk} into a single
#' raster named totalRisk. Currently sums all risk rasters together. Scaling 
#' and weighting is not yet included.
#'
#' @param x List created by \code{projectList} function
#' @return projectList with totalRisk raster. Raster values are between 
#'   0 and 1.
#' @details Risk rasters are summed to create a totalRisk raster for the 
#'   ROI and the maximum risk value is scaled to 1.
#' @export
combineRisk <- function(x){
  riskStack <- raster::stack()
  for(i in 1:length(x$layers)){
    if(names(x$layers[i]) == "water"){
    } else {
      dataLayer <- paste(names(x$layers)[i])
      riskStack <- raster::addLayer(riskStack, x$layers[[dataLayer]]$risk)
    } }
  if( nlayers(riskStack) == 1 ) {
    totalRisk <- riskStack
  } else{ totalRisk <- sum(riskStack, na.rm=TRUE) }
  totalRisk[totalRisk<0] <- 0
  totalRisk <- totalRisk/max(raster::values(totalRisk), na.rm=TRUE)
  names(totalRisk) <- c("totalRisk")
  raster::writeRaster(totalRisk, paste0(x$projectDir,"/totalRisk.tif"), format="GTiff", overwrite=TRUE)
  rm(totalRisk)
  
  x$totalRisk <- raster::raster(paste0(x$projectDir,"/totalRisk.tif"))
  return(x)
}

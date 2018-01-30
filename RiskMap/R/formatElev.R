# formatElev()
# KSchurmann Feb 28, 2017
#
#' Format ShuttleDEM elevation data
#'
#' Retrieves elevation data from data library, reprojects to project CRS,
#' resolution and ROI. Saves raster in projectDir and adds to projectList.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @param outputFileName Name of file to be created; defaults to 
#'  "elevation_raw" when function called using \code{loadData}
#' @return projectList with elevation data raster and saves elevation data 
#'   to the projectDirectory
#' @export
formatElev <- function(x, dataLibraryPath, outputFileName){ 
  if(paste0("elevation.tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
    if(paste0(outputFileName,".tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE) {
      template <- raster::raster(ext=x$ROIs$ROI, resolution=x$resolution, crs=x$ROIs$projection)
      temp <-  raster::projectRaster(from= raster::raster(paste0(dataLibraryPath,"/ShuttleDEM/SRTMDEM.SouthwestBC.tif")), to=template, method="bilinear")
      raster::writeRaster(temp, paste0(x$projectDir,"/",outputFileName,".tif"), format="GTiff", overwrite=TRUE) }
    
    elevation <-  raster::raster(paste0(x$projectDir,"/",outputFileName,".tif"))
    names(elevation) <- c('elevation')
    values(elevation) <- round( raster::values(elevation), 1)

    raster::writeRaster(elevation, paste0(x$projectDir, "/elevation.tif"), format="GTiff", overwrite=TRUE) }
    
  x$layers$elevation$raster <-  raster::raster(paste0(x$projectDir,"/elevation.tif"))
  return(x)
  
} 


### Calculating risk for elevation: based only on translate risk curve (constant high risk from 0-300m, risk decreases linearly from 300-600m)
#elev <- round(seq(from=0,to=max(values(testList$layers$elevation$raster),na.rm=T), by=0.1), 1)
#elevRisk <- translateRisk2D(elev, type=testList$layers$elevation$params$type, a=testList$layers$elevation$params$a, 
#                            b=testList$layers$elevation$params$b, riskCutoff=testList$layers$elevation$params$riskCutoff)
#temp <- data.frame(elev, elevRisk)
#testList$layers$elevation$risk <- subs(x=testList$layers$elevation$raster, y=temp)
#testList$layers$elevation$risk <- testList$layers$elevation$risk - 1

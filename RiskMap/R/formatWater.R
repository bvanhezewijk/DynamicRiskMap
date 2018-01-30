# formatWater()
# Function to convert AAFC landcover data to water raster
# Agruments: data library directory (dataLibraryPath), name of raw data output file to save in projectDirectory (outputFileName) and 
#           project parameters list (x)
# KSchurmann March 1, 2017
# 
#' Format water from AAFC landcover data 
#'
#' Retrieves AAFC landcover data from data library, reprojects to project CRS,
#' resolution and ROI. Reclassifies raster to water and NA values. Saves raster
#' in projectDir and adds to projectList.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @param outputFileName Name of file to be created; defaults to 
#'  "water_raw" when function called using \code{loadData}
#' @return projectList with water raster and saves water data
#'   to the projectDirectory
#' @export
formatWater <- function(x, dataLibraryPath, outputFileName){
  if(paste0("water.tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
    if(paste0(outputFileName,".tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE) | "treeCover_raw.tif" %in% list.files(testList$projectDir, pattern="tif$", full.names=FALSE)==FALSE) {
      template <- raster::raster(ext=x$ROIs$ROI, resolution=x$resolution, crs=x$ROIs$projection)
      temp <- raster::projectRaster(from=raster(paste0(dataLibraryPath,"/LandCover/AAFC/2015/ACGEO_2015_CI_BC_30m_v1.tif")), to=template, method="ngb")
      raster::writeRaster(temp, paste0(x$projectDir,"/",outputFileName,".tif")) }
    
    if(paste0(outputFileName,".tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)){
      lcc <- raster::raster(paste0(x$projectDir,"/",outputFileName,".tif"))
    } else { lcc <- raster::raster(paste0(x$projectDir,"/","treeCover_raw.tif")) }
    water <- raster::setValues(lcc, ifelse(values(lcc)!=20, NA, 0))
    names(water) <- c('water') 
    
    raster::writeRaster(water, paste0(x$projectDir, "/water.tif"), format="GTiff", overwrite=TRUE) }
  
  x$layers$water$raster <- raster::raster(paste0(x$projectDir,"/water.tif"))
  return(x)
}


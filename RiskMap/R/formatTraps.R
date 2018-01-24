# formatTraps()
# KSchurmann Feb 28, 2017
#
#' Format trap data
#'
#' Retrieves trap data from data library, reprojects to project CRS,
#' resolution and ROI. Adds unique trap IDs and moth counts. Saves raster 
#' and points in projectDir and adds to projectList.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @param outputFileName Name of file to be created; defaults to 
#'  "traps_raw" when function called using \code{loadData}
#' @return projectList with trap data raster and points and saves trap data 
#'   to the projectDirectory
#' @details Additional format functions for other trap data should follow this 
#'   function as a template. Named slots ("Catch", "Year", "trapID") should remain
#'   the same as they are called in other functions including \code{plotTraps}, \code{zoomTraps}
#'   and \code{createReport}
#' @export
formatTraps <- function(x, dataLibraryPath, outputFileName){
  if("traps.tif" %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
    if(paste0(outputFileName,".shp") %in% list.files(x$projectDir, pattern="shp$", full.names=FALSE)==FALSE) {
      temp <- rgdal::readOGR(dsn=paste0(dataLibraryPath,"/TrapData/2015 prelim.kml"),layer = "Waypoints")
      temp@data$Catch <- suppressWarnings(as.numeric(substr(temp@data$Name, 3, 3)))
      temp@data$Catch[is.na(temp@data$Catch)] <- 0
      temp@coords <- temp@coords[,c(1:2)]
      trapPoints <- raster::crop(sp::spTransform(temp,x$ROIs$projection), x$ROIs$ROI)
      trapPoints@data$num <- 1:length(trapPoints) #unique ID for positive traps
      trapPoints@data$Year <- 2015
      posPoints <- subset(trapPoints, Catch>0)
      posPoints$trapID <- seq(from=1,by=1,length.out=length(posPoints))
      #still a bug here if the ROI doesn't have any positive trap catches
      trapPoints@data$trapID <- merge(data.frame(subset(trapPoints, select=c("num"))), 
                                      data.frame(subset(posPoints, select=c("num", "trapID"))), 
                                      all=T, by="num")$trapID
      trapPoints$num <- NULL
      suppressWarnings(rgdal::writeOGR(obj=trapPoints, dsn=x$projectDir, layer=outputFileName, driver="ESRI Shapefile")) }
    
    trapPoints <- rgdal::readOGR(dsn=x$projectDir, layer=outputFileName)
    template <- raster::raster(ext=x$ROIs$ROI, resolution=x$resolution, crs=x$ROIs$projection)
    traps <- raster::rasterize(trapPoints, template, field="Catch", background=0) 
    names(traps) <- c('traps')
    raster::writeRaster(traps, paste0(x$projectDir, "/traps.tif"), format="GTiff", overwrite=TRUE) }
  
  x$layers$traps$raster <- raster::raster(paste0(x$projectDir,"/traps.tif"))
  x$layers$traps$points <- rgdal::readOGR(dsn=x$projectDir, layer=outputFileName)
  return(x)
}

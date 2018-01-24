# formatPorts()
# Function to locate port  data, crop, save to projectDirectory as layer name
# Agruments: data library directory (dataLibraryPath), name of raw data output file to save in projectDirectory (outputFileName) and 
#           project parameters list (x)
# KSchurmann Feb 28, 2017
#
#' Format port data
#'
#' Retrieves port location data from data library, reprojects to project CRS,
#' resolution and ROI. Saves points and raster in projectDir and adds to 
#' projectList.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @param outputFileName Name of file to be created; defaults to 
#'  "ports_raw" when function called using \code{loadData}
#' @return projectList with ports data raster and points and saves ports data 
#'   to the projectDirectory
#' @export
formatPorts <- function(x, dataLibraryPath, outputFileName){
  if("ports.tif" %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
    if(paste0(outputFileName,".shp") %in% list.files(x$projectDir, pattern="shp$", full.names=FALSE)==FALSE) {
      temp <- rgdal::readOGR(dsn=paste0(dataLibraryPath,"/PortLocations/lowerMainPorts.kml"),layer = "lowerMainPorts")
      temp@data$Locat <- 1 
      temp@coords <- temp@coords[,c(1:2)]
      portsPoints <- raster::crop(sp::spTransform(temp,x$ROIs$projection), x$ROIs$ROI)
      suppressWarnings(rgdal::writeOGR(obj=portsPoints, dsn=x$projectDir, layer=outputFileName, driver="ESRI Shapefile")) }
    
    portsPoints <- rgdal::readOGR(dsn=x$projectDir, layer=outputFileName)
    template <- raster::raster(ext=x$ROIs$ROI, resolution=x$resolution, crs=x$ROIs$projection)
    ports <- raster::rasterize(portsPoints, template, field="Locat", background=0) 
    names(ports) <- c('ports')
    raster::writeRaster(ports, paste0(x$projectDir, "/ports.tif"), format="GTiff", overwrite=TRUE) }
  
  x$layers$ports$raster <- raster::raster(paste0(x$projectDir, "/ports.tif"))
  x$layers$ports$points <- rgdal::readOGR(dsn=x$projectDir, layer=outputFileName)
  return(x)
}

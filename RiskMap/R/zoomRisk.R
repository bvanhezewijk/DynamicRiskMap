# zoomRisk()
# KSchurmann March 2, 2017
#
#' Zoom in on totalRisk map
#'
#' Interactively select extents on totalRisk map to view in subregion, 
#' google map and layers devices. For use with \code{plotRisk}.
#' 
#' @param x List created by \code{projectList} function
#' @param plotLayers plotLayers List of layers to be plotted, or \code{'all'} (default)
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'    or \code{'terrain'} for google map window
#' @return Graphics devices showing selected subregion of totalRisk raster
#' @details Interactively click totalRisk map (Device 2) to select extents to 
#'   view in other graphics devices. Device 3 plots the selected extent of the 
#'   totalRisk raster. Device 4 calls \code{dismo::gmap} to get a google map 
#'   layer of the selected extent and overplots the totalRisk raster. Device 
#'   5 (if open) plots the sub extent of the risk raster for each data layer 
#'   listed in \code{plotLayers} or all layers if \code{'all'} was specified (default). 
#'   
#'   \code{zoomRisk} can be called any time totalRisk is plotted in device 2
#'   but will not add traps or high risk regions to subregion maps as would 
#'   be expected with  \code{plotTraps} or \code{plotHiRisk}.
#' 
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{plotRisk}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
zoomRisk <- function(x, plotLayers="all", basemap="hybrid"){
  writeLines("Click twice on totalRisk map to select extent")
  dev.set(2)
  roi <- clickExtent(x$totalRisk, plot.it=FALSE)
  box <- as(roi, 'SpatialPolygons')
  Plot(box, addTo="x$totalRisk")
  riskCrop <- raster::crop(x$totalRisk, roi)
  if("water" %in% names(x$layers)) waterCrop <- raster::crop(x$layers$water$raster, roi)
  dev.set(3)
  clearPlot()
  Plot(riskCrop,cols=rev(heat.colors(16)), title="Selected subregion", legendRange = 0:1)
  if("water" %in% names(x$layers)) Plot(waterCrop, cols="lightblue", addTo="riskCrop")
  
  if(5 %in% dev.list()) {
    dev.set(5)
    clearPlot()
    if(plotLayers=="all"){
      temp <- list()
      for(j in 1:length(x$layers)){
        dataLayer <- paste(names(x$layers)[j])
        temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
      }
      temp2 <- list()
      for(k in 1:length(temp)){
        dataLayer <- paste(names(x$layers)[k])
        temp2[[dataLayer]] <- raster::crop(temp[[k]], roi)
      }
      Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
      if("water" %in% names(x$layers)) {
        setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          waterCrop <- raster::crop(x$layers$water$raster, roi)
          Plot(waterCrop, addTo=m)
        } }
    } else{
      temp <- list()
      for(j in 1:length(x$layers)){
        if(names(x$layers[j]) %in% plotLayers){
          dataLayer <- paste(names(x$layers)[j])
          temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
        } }
      temp2 <- list()
      for(k in 1:length(temp)){
        dataLayer <- paste(names(temp)[k])
        temp2[[dataLayer]] <- raster::crop(temp[[k]], roi)
      }
      Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
      if("water" %in% names(x$layers)) {
        setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          waterCrop <- raster::crop(x$layers$water$raster, roi)
          Plot(waterCrop, addTo=m) } }
    } }
  
  dev.set(4)
  clearPlot()
  riskGoogle <- dismo::gmap(x = riskCrop, type = basemap, lonlat=TRUE)
  temp <- raster::projectRaster(x$totalRisk, riskGoogle, method="ngb")
  if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, riskGoogle, method="ngb")
  temp[temp<=0] <- NA
  if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
  plot(riskGoogle, main="Google Maps subROI")
  plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F)
  
}




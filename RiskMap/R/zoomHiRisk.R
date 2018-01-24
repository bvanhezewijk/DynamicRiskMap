# zoomHiRisk()
#
#' Zoom in on high risk areas
#'
#' Interactively select extents on totalRisk map to view high risk areas in 
#' subregion, google map and layers devices. For use with \code{plotHiRisk}.
#' 
#' @param x List created by \code{projectList} function
#' @param plotLayers plotLayers List of layers to be plotted, or \code{'all'} (default)
#' @param hiRisk Numeric; value between 0 and 1 that defines minimum risk value
#'   classified as high risk. Default=0.5.
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'    or \code{'terrain'} for google map window
#' @return Graphics devices showing selected subregion of totalRisk raster
#' @details Interactively click totalRisk map (Device 2) to select extents to 
#'   view in other graphics devices. Device 3 plots the selected extent of the 
#'   totalRisk and high risk rasters. Device 4 calls \code{dismo::gmap} to get 
#'   a google map layer of the selected extent and overplots the totalRisk 
#'   and high risk rasters. Device 5 plots the sub extent of the risk raster for 
#'   each data layer listed in \code{plotLayers} or all layers if \code{'all'} was 
#'   specified (default). 
#' 
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{plotHiRisk}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
zoomHiRisk <- function(x, plotLayers='all', 
                       hiRisk=0.5, basemap='hybrid'){
  
  writeLines("Click twice on totalRisk map to select extent")
  
  highRisk <- raster::reclassify(x$totalRisk, matrix(c(0,hiRisk, NA, hiRisk,1,1),ncol=3,byrow=T), include.lowest=TRUE)
  
  dev.set(2)
  
  clicks <- raster::click(x$totalRisk, n=2, xy=TRUE, show=FALSE)
  xmn <- min(clicks$x)
  xmx <- max(clicks$x)
  ymn <- min(clicks$y)
  ymx <- max(clicks$y)
  roi <- raster::extent(xmn, xmx, ymn, ymx)
  box <- as(roi, 'SpatialPolygons')
  plot(box, add=T)
  
  # plot in subROI device
  riskCrop <- raster::crop(x$totalRisk, roi)
  if("water" %in% names(x$layers)) waterCrop <- raster::crop(x$layers$water$raster, roi)
  hiRiskCrop <- raster::crop(highRisk, roi)
  
  dev.set(3)
  clearPlot()
  dev.set(5)
  clearPlot()
  dev.set(4)
  clearPlot()
  
  dev.set(3)
  par(mar=c(0,0.5,0,0.5), bty="n", tcl=0)
  arg <- list(at=c(0, 0.2,0.4,0.6,0.8, 1), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), cex.axis=0.7, line=-0.5,lwd=0)
  plot(riskCrop,
       breaks=seq(0, 1, 1/16), col=rev(heat.colors(16)), axis.args=arg, maxpixels=100000,
       legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
  title(main="ROIsub", line=-3)
  if("water" %in% names(x$layers)) plot(waterCrop, add=TRUE, col="lightblue", legend=F, maxpixels=50000)
  plot(hiRiskCrop, add=T, col="blue", legend=FALSE)
  
  
  # plot layers
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
        Plot(waterCrop, addTo=m)
      } }
  }
  
  dev.set(4)
  clearPlot()
  riskGoogle <- dismo::gmap(x = riskCrop, type = basemap, lonlat=TRUE)
  googHighRisk <- raster::projectRaster(highRisk, riskGoogle, method="ngb")
  plot(riskGoogle, main="Google Maps subROI")
  plot(googHighRisk, col=scales::alpha('red', 0.25), add=T, legend = F)
  
}





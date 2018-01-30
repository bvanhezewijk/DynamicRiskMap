# zoomTraps()
#
#' Zoom in on trap locations
#'
#' Interactively select extents on totalRisk map to view traps in subregion, 
#' google map and layers devices. For use with \code{plotTraps}.
#' 
#' @param x List created by \code{projectList} function
#' @param plotLayers plotLayers List of layers to be plotted, or \code{'all'} (default)
#' @param addCircles Logical; if \code{TRUE}, circles plotted around positive traps
#' @param circleRadius Numeric; radius in raster units of circle around positive
#'    traps. Defaults to 1609 m (1 mile)
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
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{plotTraps}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
zoomTraps <- function(x, plotLayers='all', 
                      addCircles=TRUE, circleRadius=1609, 
                      basemap='hybrid'){
  writeLines("Click twice on totalRisk map to select extent")
  
  posCatch <- subset(x$layers$traps$points, Catch>0)
  posCatchGoog <- sp::spTransform(posCatch, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
  
  if(addCircles==TRUE) {
    circles <- sp::polygons(dismo::circles(sp::coordinates(posCatch), d=circleRadius, lonlat=FALSE, n=360, dissolve=FALSE))
    circlesGoog <- sp::polygons(dismo::circles(sp::coordinates(posCatchGoog), d=circleRadius, lonlat=TRUE, n=360, dissolve=FALSE))
  }
  
  # clicking to get extent
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
  negTraps <- raster::crop(subset(x$layers$traps$points, Catch==0), roi)
  
  dev.set(3)
  clearPlot()
  par(mar=c(0,0.5,0,0.5), bty="n", tcl=0)
  arg <- list(at=c(0, 0.2,0.4,0.6,0.8, 1), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), cex.axis=0.7, line=-0.5,lwd=0)
  plot(riskCrop,
       breaks=seq(0, 1, 1/16), col=rev(heat.colors(16)), axis.args=arg, maxpixels=100000,
       legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
  title(main="ROIsub", line=-3)
  if("water" %in% names(x$layers)) plot(waterCrop, add=TRUE, col="lightblue", legend=F, maxpixels=50000)
  plot(negTraps, add=T, col="grey55", cex=0.6, pch=16)
  plot(posCatch, add=T, pch=16, cex=0.8)
  maptools::pointLabel(x=sp::coordinates(posCatch)[,1], y=sp::coordinates(posCatch)[,2], labels=as.character(posCatch$trapID), cex=0.8)
  if(addCircles==TRUE) {
    plot(circles, add=T) }
  
  
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
    if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
    for(m in names(temp)) {
      if("water" %in% names(x$layers)) {
        waterCrop <- raster::crop(x$layers$water$raster, roi)
        Plot(waterCrop, addTo=m) }
      Plot(negTraps, addTo = m, gp=gpar(col="grey55"))
      Plot(posCatch, addTo=m, size=7)
      if(addCircles==TRUE) {
        Plot(circles, addTo=m)
      }
    } 
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
    if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
    for(m in names(temp)) {
      if("water" %in% names(x$layers)) {
        waterCrop <- raster::crop(x$layers$water$raster, roi)
        Plot(waterCrop, addTo=m) }
      Plot(negTraps, addTo = m, gp=gpar(col="grey55"))
      Plot(posCatch, addTo=m, size=7)
      if(addCircles==TRUE) {
        Plot(circles, addTo=m)
      }
    }
  }
  
  dev.set(4)
  clearPlot()
  riskGoogle <- dismo::gmap(x = riskCrop, type = basemap, lonlat=TRUE)
  temp <- raster::projectRaster(x$totalRisk, riskGoogle, method="ngb")
  if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, riskGoogle, method="ngb")
  temp[temp<=0] <- NA
  if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
  plot(riskGoogle, main="Google Maps subROI")
  plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F)
  plot(posCatchGoog, add=T, pch=16, col="red", cex=0.8)
  maptools::pointLabel(x=sp::coordinates(posCatchGoog)[,1], y=sp::coordinates(posCatchGoog)[,2], 
                       labels=as.character(posCatchGoog$trapID), cex=0.8, font=2)
  if(addCircles==TRUE) {
    plot(circlesGoog, add=T, lwd=1, border="black", lwd=2)
  }
  
}





# plotTraps()
# function to plot totalRisk with positive traps, subregions surrounding each positive trap, and google map of subregion
# KSchurmann, March 10, 2017
#
#' Plotting positive trap catches
#'
#' Plots region showing location of positive and negative traps. Shows inset map
#' of totalRisk, google map and individual layers around each positive trap.
#' 
#' @param x List created by \code{projectList} function
#' @param plotLayers List of layers to be  plotted, or \code{'all'} (default)
#' @param addCircles Logical; if \code{TRUE}, circles plotted around positive traps (default)
#' @param circleRadius Numeric; radius in raster units of circle around positive
#'    traps. Defaults to 1609 m (1 mile)
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'    or \code{'terrain'} for google map window
#' @param cycle Logical; if \code{TRUE}, subregion devices cycle through all positive trap 
#'   locations, waiting for prompts from console (see details). If \code{FALSE} (default),
#'   subregion devices display ROIsub.
#' @param window Numeric; distance in raster units in each direction to show around trap
#'   if \code{cycle = TRUE}. Default is 5000m.
#' @return Graphics devices showing risk around positive trap locations
#' @details Device 2 plots the totalRisk raster for the entire \code{ROI} extent with  
#'   the location and ID of positive traps. Device 3 plots subregion of the
#'   totalRisk raster around each positive trap. Device 4 calls \code{dismo::gmap} to 
#'   get a google map layer of the subregion and overplots the totalRisk raster. Device 
#'   5 plots the risk raster subregion for each data layer listed in \code{plotLayers} 
#'   or all layers if \code{'all'} was specified (default). 
#'   
#'   If \code{cycle = TRUE}, Devices 3, 4 & 5 display the subregion around the first
#'   positive trap and wait for <ENTER> to be pressed in the console before displaying 
#'   subsequent traps. Extent of the subregion is defined by the \code{window} argument. 
#'   Plotting can be stopped by typing "END" into the console. Negative trap locations 
#'   are shown in grey.
#' 
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{zoomTraps}, \code{plotRisk}, \code{plotHiRisk}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
plotTraps <- function(x, plotLayers='all', 
                      addCircles=TRUE, circleRadius=1609, 
                      basemap='hybrid', cycle=FALSE, window=5000) {
  graphics.off()
  windows(w=3.5, h=4, xpos=1685, ypos=0)     #totalRisk
  windows(w=3.5, h=4, xpos=2115, ypos=0)     #subROI
  windows(w=3.5, h=4, xpos=2545, ypos=0)     #google
  windows(w=10.5, h=3, xpos=1690, ypos=560)  #risk layers
  
  posCatch <- subset(x$layers$traps$points, Catch>0)
  posCatchGoog <- sp::spTransform(posCatch, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
  
  if(addCircles==TRUE) {
    circles <- sp::polygons(dismo::circles(sp::coordinates(posCatch), d=circleRadius, lonlat=FALSE, n=360, dissolve=FALSE))
    circlesGoog <- sp::polygons(dismo::circles(sp::coordinates(posCatchGoog), d=circleRadius, lonlat=TRUE, n=360, dissolve=FALSE))
  }
  
  # total risk plus positive trap points
  dev.set(2)
  clearPlot()
  par(mar=c(0,0.5,0,0.5), bty="n", tcl=0)
  arg <- list(at=c(0, 0.2,0.4,0.6,0.8, 1), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), cex.axis=0.7, line=-0.5,lwd=0)
  plot(x$totalRisk,
       breaks=seq(0, 1, 1/16), col=rev(heat.colors(16)), axis.args=arg, maxpixels=100000,
       legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
  title(main=paste0(names(x$totalRisk)," & Positive Traps"), line=-3)
  if("water" %in% names(x$layers)) plot(x$layers$water$raster, add=TRUE, col="lightblue", legend=F, maxpixels=50000)
  plot(posCatch, add=T, pch=16, cex=0.8)
  maptools::pointLabel(x=sp::coordinates(posCatch)[,1], y=sp::coordinates(posCatch)[,2], labels=as.character(posCatch$trapID), cex=0.8)
  
  # individual traps
  if(cycle==TRUE){
    for(i in 1:length(posCatch)) {
      dev.set(3)
      clearPlot()
      dev.set(4)
      clearPlot()
      dev.set(5)
      clearPlot()
      
      xmin <- sp::coordinates(posCatch)[i,][1] - window
      xmax <- sp::coordinates(posCatch)[i,][1] + window
      ymin <- sp::coordinates(posCatch)[i,][2] - window
      ymax <- sp::coordinates(posCatch)[i,][2] + window
      trapROI <- raster::extent(xmin, xmax, ymin, ymax)
      
      #subROI centred on positive trap - window specified in arguments
      dev.set(3)
      clearPlot()
      trapRisk <- raster::crop(x$totalRisk, trapROI)
      if("water" %in% names(x$layers)) tempWater <- raster::crop(x$layers$water$raster, trapROI)
      Plot(trapRisk, cols=rev(heat.colors(16)), gpText=gpar(fontsize=10), legendRange = 0:1,
           title=paste0("Positive Trap ", posCatch$trapID[i], ": ",round(sp::coordinates(posCatchGoog)[i,][2],2),", ",round(sp::coordinates(posCatchGoog)[i,][1],2)))
      if("water" %in% names(x$layers)) Plot(tempWater, addTo="trapRisk")
      Plot(x$layers$traps$points, addTo = "trapRisk", gp=gpar(col="grey60"))
      Plot(posCatch, addTo="trapRisk", size=7)
      if(addCircles==TRUE) {
        Plot(circles, addTo="trapRisk")
      }
      
      # plotting individual layers
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
          dataLayer <- paste(names(temp)[k])
          temp2[[dataLayer]] <- raster::crop(temp[[k]], trapROI)
        }
        Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
        if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          if("water" %in% names(x$layers)) {
          waterCrop <- raster::crop(x$layers$water$raster, trapROI)
          Plot(waterCrop, addTo=m)  }
          Plot(x$layers$traps$points, addTo = m, gp=gpar(col="grey55"))
          Plot(posCatch, addTo=m, size=7)
          if(addCircles==TRUE) {
            Plot(circles, addTo=m)
          }
        } 
      } else {
        temp <- list()
        for(j in 1:length(x$layers)){
          if(names(x$layers[j]) %in% plotLayers){
            dataLayer <- paste(names(x$layers)[j])
            temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
          } }
        temp2 <- list()
        for(k in 1:length(temp)){
          dataLayer <- paste(names(temp)[k])
          temp2[[dataLayer]] <- raster::crop(temp[[k]], trapROI)
        }
        Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
        if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          if("water" %in% names(x$layers)) {
          waterCrop <- raster::crop(x$layers$water$raster, trapROI)
          Plot(waterCrop, addTo=m) }
          Plot(x$layers$traps$points, addTo = m, gp=gpar(col="grey55"))
          Plot(posCatch, addTo=m, size=7)
          if(addCircles==TRUE) {
            Plot(circles, addTo=m)
          }
        }
      }
      
      #Google map of subROI
      dev.set(4)
      catchGoogle <- dismo::gmap(x = trapRisk, type = basemap, lonlat=TRUE)
      temp <- raster::projectRaster(x$totalRisk, catchGoogle, method="ngb")
      if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, catchGoogle, method="ngb")
      temp[temp<=0] <- NA
      if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
      
      plot(catchGoogle, main="Google Maps subROI")
      plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F) 
      points(sp::coordinates(posCatchGoog)[i,][1],sp::coordinates(posCatchGoog)[i,][2], add=T, pch=16)
      if(addCircles==TRUE) {
        plot(circlesGoog[i], add=T, lwd=2)
      }
      
      line <- readline(prompt='Press <Enter> to see next trap or type "END" to stop: ')
      if(line == "END" | line == "end" | line == "End") stop("Ended trap plotting")
    } 
  } else{
    # plotting subROI
    dev.set(3)
    clearPlot()
    trapRisk <- raster::crop(x$totalRisk, x$ROIs$ROIsub)
    if("water" %in% names(x$layers)) tempWater <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
    negTraps <- raster::crop(subset(x$layers$traps$points, Catch==0), x$ROIs$ROIsub)
    par(mar=c(0,0.5,0,0.5), bty="n", tcl=0)
    arg <- list(at=c(0, 0.2,0.4,0.6,0.8, 1), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), cex.axis=0.7, line=-0.5,lwd=0)
    plot(trapRisk,
         breaks=seq(0, 1, 1/16), col=rev(heat.colors(16)), axis.args=arg, maxpixels=100000,
         legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
    title(main="ROIsub", line=-3)
    if("water" %in% names(x$layers)) plot(tempWater, add=TRUE, col="lightblue", legend=F, maxpixels=50000)
    plot(negTraps, add=T, col="grey55", cex=0.6, pch=16)
    plot(posCatch, add=T, pch=16, cex=0.8)
    maptools::pointLabel(x=sp::coordinates(posCatch)[,1], y=sp::coordinates(posCatch)[,2], labels=as.character(posCatch$trapID), cex=0.8)
    if(addCircles==TRUE) {
      plot(circles, add=T) }
    
    # plotting individual layers
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
        dataLayer <- paste(names(temp)[k])
        temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
      }
      Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
      if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
      for(m in names(temp)) {
        if("water" %in% names(x$layers)) {
        waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
        Plot(waterCrop, addTo=m) }
        Plot(x$layers$traps$points, addTo = m, gp=gpar(col="grey55"))
        Plot(posCatch, addTo=m, size=7)
        if(addCircles==TRUE) {
          Plot(circles, addTo=m)
        }
      } 
    } else {
      temp <- list()
      for(j in 1:length(x$layers)){
        if(names(x$layers[j]) %in% plotLayers){
          dataLayer <- paste(names(x$layers)[j])
          temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
        } }
      temp2 <- list()
      for(k in 1:length(temp)){
        dataLayer <- paste(names(temp)[k])
        temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
      }
      Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
      if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
      for(m in names(temp)) {
        if("water" %in% names(x$layers)) {
        waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
        Plot(waterCrop, addTo=m) }
        Plot(x$layers$traps$points, addTo = m, gp=gpar(col="grey55"))
        Plot(posCatch, addTo=m, size=7)
        if(addCircles==TRUE) {
          Plot(circles, addTo=m)
        }
      }
    }
    
    #Google map of subROI
    dev.set(4)
    catchGoogle <- dismo::gmap(x = trapRisk, type = basemap, lonlat=TRUE)
    temp <- raster::projectRaster(x$totalRisk, catchGoogle, method="ngb")
    if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, catchGoogle, method="ngb")
    temp[temp<=0] <- NA
    if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
    
    plot(catchGoogle, main="Google Maps subROI")
    plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F)
    plot(posCatchGoog, add=T, pch=16, col="red", cex=0.8)
    maptools::pointLabel(x=sp::coordinates(posCatchGoog)[,1], y=sp::coordinates(posCatchGoog)[,2], 
                         labels=as.character(posCatchGoog$trapID), cex=0.8, font=2)
    if(addCircles==TRUE) {
      plot(circlesGoog, add=T, lwd=1, border="black", lwd=2)
    }
    
  }  
  
}


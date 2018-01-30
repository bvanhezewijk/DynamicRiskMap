# plotHiRisk()
# function to plot totalRisk with highRisk areas highlighted in blue
# ROI divided into equal subregions to show in more detail highRisk areas - plotted in subROI and google maps devices 
# Arguments: projectList, hiRisk (cutoff for hiRIsk), rows and cols (how many sections to divide ROI into),
#            plotLayers (specify if only want certain layers to be shown, default=="all)
# KSchurmann, March 10, 2017
#
#' Plotting area of high risk
#'
#' Plots region with highlighted areas of high risk. Divides map region into
#' subregions based on number of \code{rows} and \code{cols} and displays each subregion
#' as total & high risk, google map, and individual layers.
#' 
#' @param x List created by \code{projectList} function
#' @param plotLayers List of layers to be plotted, or \code{'all'} (default)
#' @param hiRisk Numeric; value between 0 and 1 that defines minimum risk value
#'   classified as high risk. Default=0.5.
#' @param rows Integer; number of rows to divide map region into for plotting
#' @param cols Integer; number of columns to divide map region into for plotting
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'    or \code{'terrain'} for google map window
#' @return Graphics devices showing high risk regions
#' @details Device 2 plots the totalRisk raster for the entire \code{ROI} extent with high 
#'   risk regions, as defined by the argument \code{hiRisk} mapped in blue. Device 3 
#'   plots an inset subregion of the totalRisk raster. Device 4 calls \code{dismo::gmap} to 
#'   get a google map layer and overplots the high risk areas in red. Device 5 plots the 
#'   risk rasters each data layer listed in \code{plotLayers} or all layers if \code{'all'}
#'   was specified (default). 
#'   
#'   If \code{rows=1} and \code{cols=1}, \code{ROIsub} extent is shown in devices 3, 4 & 5 (default). 
#'   If \code{rows} or \code{cols} is greater than 1, devices 3, 4 & 5 display 
#'   the first subregion and wait for <ENTER> to be pressed in the console before 
#'   displaying subsequent subregions. Number and extent of subregions is defined by 
#'   the number of \code{rows} and \code{cols} Plotting can be stopped by typing "END" 
#'   into the console.
#' 
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{plotRisk}, \code{plotTraps}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
plotHiRisk <- function(x, plotLayers='all', 
                       hiRisk=0.5, rows=1, cols=1, 
                       basemap='hybrid') {
  graphics.off()
  windows(w=3.5, h=4, xpos=1685, ypos=0)     #totalRisk
  windows(w=3.5, h=4, xpos=2115, ypos=0)     #subROI
  windows(w=3.5, h=4, xpos=2545, ypos=0)     #google
  windows(w=10.5, h=3, xpos=1690, ypos=560)  #risk layers
  
  # plotting totalRisk & highRisk
  
  dev.set(2)
  clearPlot()
  par(mar=c(0,0.5,0,1), bty="n", tcl=0)
  arg <- list(at=c(0, 0.2,0.4,0.6,0.8, 1), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1.0"), cex.axis=0.7, line=-0.5,lwd=0)
  plot(x$totalRisk,
       breaks=seq(0, 1, 1/16), col=rev(heat.colors(16)), axis.args=arg, maxpixels=100000,
       legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
  title(main="totalRisk & high risk regions", line=-3)
  if("water" %in% names(x$layers)){
    plot(x$layers$water$raster, add=TRUE, col="lightblue", legend=F, maxpixels=30000)
  } 
    
  highRisk <- raster::reclassify(x$totalRisk, matrix(c(0,hiRisk, NA, hiRisk,1,1),ncol=3,byrow=T), include.lowest=TRUE)
  par(mar=c(0,0.5,0,1), bty="n", tcl=0, new=TRUE)
  plot(highRisk, col="blue", legend=FALSE, maxpixels=100000,
       legend.mar=2, axes=FALSE, box=FALSE, legend.width=0.5, legend.shrink=0.3)
  legend(min(sp::coordinates(x$totalRisk)[,1]), min(sp::coordinates(x$totalRisk)[,2]), 
         legend=paste0("hiRisk > ", hiRisk), pch=15, col="blue", cex=0.7, bty="n")
  
  if(rows==1 & cols==1){
    box <- as(x$ROIs$ROIsub, 'SpatialPolygons')
    plot(box, add=T)
    
    dev.set(3)
    SpaDES::clearPlot()
    subTotalRisk <- raster::crop(x$totalRisk, x$ROIs$ROIsub)
    subHiRisk <- raster::crop(highRisk, x$ROIs$ROIsub)
    if("water" %in% names(x$layers)){
      tempWater <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
      SpaDES::setColors(tempWater) <- "lightblue" }
    SpaDES::setColors(subHiRisk) <- "blue"
    
    Plot(subTotalRisk, title="High Risk Subregion", cols=rev(heat.colors(16)) , legendRange = 0:1)
    Plot(subHiRisk, addTo="subTotalRisk")
    if("water" %in% names(x$layers)) Plot(tempWater, addTo="subTotalRisk")
    
    # individual layers
    dev.set(5)
    SpaDES::clearPlot()
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
      if("water" %in% names(x$layers)){
        SpaDES::setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
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
        temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
      }
      Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
      if("water" %in% names(x$layers)){
        SpaDES::setColors(x$layers$water$raster) <- "lightblue"
        for(m in names(temp)) {
          waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
          Plot(waterCrop, addTo=m)
        } }
    }
    
    #Google map of subROI
    dev.set(4)
    subGoogle <- dismo::gmap(x = subHiRisk, type = basemap, lonlat=TRUE)
    googHighRisk <- raster::projectRaster(highRisk, subGoogle, method="ngb")
    plot(subGoogle)
    plot(googHighRisk, col=scales::alpha('red', 0.25), add=T, legend = F)
    
  } else{
    # dividing ROI into smaller regions based on specified number of rows and columns
    xmin <- raster::xmin(x$totalRisk)
    ymax <- raster::ymax(x$totalRisk)
    
    xdist <- raster::xmax(x$totalRisk) - raster::xmin(x$totalRisk) # x axis distance
    ydist <- raster::ymax(x$totalRisk) - raster::ymin(x$totalRisk) # y axis distance
    
    xCoords <- list()
    for(i in 1:cols){
      temp <-  c(xmin + (xdist/cols)*(i-1), xmin + (xdist/cols)*(i))
      xCoords[[i]] <- temp                
    } 
    
    yCoords <- list()
    for(i in 1:rows){
      temp <-  c(ymax - (ydist/rows)*(i), ymax - (ydist/rows)*(i-1))
      yCoords[[i]] <- temp                
    } 
    quadCoords <- expand.grid(xCoords, yCoords)
    
    temp <- list()
    for(p in 1:nrow(quadCoords)){
      temp$x[[p]] <- unlist(quadCoords[p,])[[1]]+2000
      temp$y[[p]] <- unlist(quadCoords[p,])[[4]]-2000
      temp$Label[[p]] <- p
    }
    Labels <- sp::SpatialPointsDataFrame(coords=data.frame(x=temp$x, y=temp$y), proj4string=x$ROIs$projection, data=data.frame(temp$Label))
    
    for(i in 1:nrow(quadCoords)){
      
      dev.set(2)
      box <- as(raster::extent(unlist(quadCoords[i,])), 'SpatialPolygons')
      plot(box, add=T)
      lab <- subset(Labels, temp.Label==i)
      text(sp::coordinates(lab)[,1], sp::coordinates(lab)[,2], label=lab$temp.Label)
      
      dev.set(3)
      SpaDES::clearPlot()
      dev.set(4)
      SpaDES::clearPlot()
      dev.set(5)
      SpaDES::clearPlot()
      
      #subregion of highRisk
      dev.set(3)
      SpaDES::clearPlot()
      subTotalRisk <- raster::crop(x$totalRisk, raster::extent(unlist(quadCoords[i,])))
      subHiRisk <- raster::crop(highRisk, raster::extent(unlist(quadCoords[i,])))
      if("water" %in% names(x$layers)){
        tempWater <- raster::crop(x$layers$water$raster, subHiRisk)
        SpaDES::setColors(tempWater) <- "lightblue" }
        SpaDES::setColors(subHiRisk) <- "blue"
      
      Plot(subTotalRisk, title=paste0("High Risk Subregion ", lab$temp.Label), cols=rev(heat.colors(16)) , legendRange = 0:1)
      Plot(subHiRisk, addTo="subTotalRisk")
      if("water" %in% names(x$layers)) Plot(tempWater, addTo="subTotalRisk")
      
      # individual layers
      dev.set(5)
      SpaDES::clearPlot()
      if(plotLayers=="all"){
        temp <- list()
        for(j in 1:length(x$layers)){
          dataLayer <- paste(names(x$layers)[j])
          temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
        }
        temp2 <- list()
        for(k in 1:length(temp)){
          dataLayer <- paste(names(temp)[k])
          temp2[[dataLayer]] <- raster::crop(temp[[k]], raster::extent(unlist(quadCoords[i,])))
        }
        Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
        if("water" %in% names(x$layers)){
          SpaDES::setColors(x$layers$water$raster) <- "lightblue"
          for(m in names(temp)) {
            waterCrop <- raster::crop(x$layers$water$raster, raster::extent(unlist(quadCoords[i,])))
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
          temp2[[dataLayer]] <- raster::crop(temp[[k]], raster::extent(unlist(quadCoords[i,])))
        }
        Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
        if("water" %in% names(x$layers)){ 
          SpaDES::setColors(x$layers$water$raster) <- "lightblue"
          for(m in names(temp)) {
            waterCrop <- raster::crop(x$layers$water$raster, raster::extent(unlist(quadCoords[i,])))
            Plot(waterCrop, addTo=m)
          } }
      }
      
      #Google map of subROI
      dev.set(4)
      subGoogle <- dismo::gmap(x = subHiRisk, type = basemap, lonlat=TRUE)
      googHighRisk <- raster::projectRaster(highRisk, subGoogle, method="ngb")
      plot(subGoogle)
      plot(googHighRisk, col=scales::alpha('red', 0.25), add=T, legend = F)
      
      line <- readline(prompt='Press <Enter> to see next trap or type "END" to stop: ')
      if(line == "END") stop("Ended High Risk plotting")
    }
  }
}

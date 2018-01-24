# plotRisk()
# Function to create risk raster for each data layer
# Agruments: projectList
# KSchurmann March 10, 2017
#
## for layers plot - color scale not same for each layer
#
#' Plotting total risk
#'
#' Plots region showing total risk. Shows subregion map of total risk, 
#' google map and risk of individual layers.
#' 
#' @param x List created by \code{projectList} function
#' @param plot Character string; specifies which devices to plot: \code{'all'} (default),
#'   \code{'totalRisk'}, or \code{'layers'}
#' @param plotLayers List of layers to be plotted, or \code{'all'} (default)
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'    or \code{'terrain'} for google map window
#' @return Graphics devices showing total risk.
#' @details Device 2 plots the totalRisk raster for the entire \code{ROI} extent. Device 3 
#'   plots the \code{ROIsub} extent of the totalRisk raster. Device 4 calls \code{dismo::gmap} to 
#'   get a google map layer of the \code{ROIsub} and overplots the totalRisk raster. Device 
#'   5 plots the risk raster \code{ROIsub} for each data layer listed in \code{plotLayers} 
#'   or all layers if \code{'all'} was specified (default). 
#' 
#'   Devices 3 & 5 uses the \code{Plot} function from the SpaDES package.
#' @seealso \code{zoomRisk}, \code{plotHiRisk}, \code{plotTraps}, \code{SpaDES::Plot}, \code{dismo::gmap}
#' @export
plotRisk <- function(x, plot='all', plotLayers='all', 
                     basemap='hybrid'){
  if("water" %in% names(x$layers)) setColors(x$layers$water$raster) <- "lightblue"
  
  switch(plot,
         all = {
           graphics.off()
           windows(w=3.5, h=4, xpos=1685, ypos=0)     #totalRisk
           windows(w=3.5, h=4, xpos=2115, ypos=0)     #subROI
           windows(w=3.5, h=4, xpos=2545, ypos=0)     #google
           windows(w=10.5, h=3, xpos=1690, ypos=560)  #risk layers
           
           # plotting totalRisk
           dev.set(2)
           clearPlot()
           Plot(x$totalRisk, title=names(x$totalRisk) ,cols=rev(heat.colors(16)), legendRange = 0:1)
           if("water" %in% names(x$layers)) Plot(x$layers$water$raster, addTo="x$totalRisk")
           box <- as(x$ROIs$ROIsub, 'SpatialPolygons')
           Plot(box, addTo="x$totalRisk")
           
           # plotting subROI & totalRisk
           dev.set(3)
           clearPlot()
           subROI <- raster::crop(x$totalRisk, x$ROIs$ROIsub)
           if("water" %in% names(x$layers)) subWater <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
           Plot(subROI, cols=rev(heat.colors(16)), legendRange = 0:1)
           if("water" %in% names(x$layers)) Plot(subWater, addTo="subROI")
           
           # plotting individual layers
           dev.set(5)
           clearPlot()
           if(plotLayers=="all"){
             temp <- list()
             for(i in 1:length(x$layers)){
               dataLayer <- paste(names(x$layers)[i])
               temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
             } 
             temp2 <- list()
             for(k in 1:length(temp)){
               dataLayer <- paste(names(temp)[k])
               temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
             }
             Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
             #Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)), legendRange = 0:1)
             if("water" %in% names(x$layers)){
               setColors(x$layers$water$raster) <- "lightblue"
               for(m in names(temp)) {
                 waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
                 Plot(waterCrop, addTo=m) } }
           } else { 
             temp <- list()
             for(i in 1:length(x$layers)){
               if(names(x$layers[i]) %in% plotLayers){
                 dataLayer <- paste(names(x$layers)[i])
                 temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
               }
             }
             temp2 <- list()
             for(k in 1:length(temp)){
               dataLayer <- paste(names(temp)[k])
               temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
             }
             Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
             #Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)), legendRange = 0:1)
             if("water" %in% names(x$layers)){
               setColors(x$layers$water$raster) <- "lightblue"
               for(m in names(temp)) {
                 waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
                 Plot(waterCrop, addTo=m) } }
           }
           
           
           #plotting Google Maps image
           dev.set(4)
           googlemap <- dismo::gmap(x = raster::crop(x$totalRisk, x$ROIs$ROIsub), type = basemap, lonlat=TRUE)
           temp <- raster::projectRaster(x$totalRisk, googlemap, method="ngb")
           if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, googlemap, method="ngb")
           temp[temp<=0] <- NA
           if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
           plot(googlemap, main="Google Maps subROI")
           plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F)},
         
         totalRisk = {
           graphics.off()
           windows(w=3.5, h=4, xpos=1685, ypos=0)     #totalRisk
           windows(w=3.5, h=4, xpos=2115, ypos=0)     #subROI
           windows(w=3.5, h=4, xpos=2545, ypos=0)     #google
           
           # plotting totalRisk
           dev.set(2)
           clearPlot()
           Plot(x$totalRisk, title=names(x$totalRisk) ,cols=rev(heat.colors(16)), legendRange = 0:1)
           if("water" %in% names(x$layers)) Plot(x$layers$water$raster, addTo="x$totalRisk") 
           box <- as(x$ROIs$ROIsub, 'SpatialPolygons')
           Plot(box, addTo="x$totalRisk")
           
           # plotting subROI & totalRisk
           dev.set(3)
           clearPlot()
           subROI <- raster::crop(x$totalRisk, x$ROIs$ROIsub)
           Plot(subROI, cols=rev(heat.colors(16)), legendRange = 0:1)
           if("water" %in% names(x$layers)) subWater <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
           if("water" %in% names(x$layers)) Plot(subWater, addTo="subROI")
           
           #plotting Google Maps image
           dev.set(4)
           googlemap <- dismo::gmap(x = raster::crop(x$totalRisk, x$ROIs$ROIsub), type = basemap, lonlat=TRUE)
           temp <- raster::projectRaster(x$totalRisk, googlemap, method="ngb")
           if("water" %in% names(x$layers)) waterMask <- raster::projectRaster(x$layers$water$raster, googlemap, method="ngb")
           temp[temp<=0] <- NA
           if("water" %in% names(x$layers)) temp <- raster::mask(temp, waterMask, inverse=TRUE)
           plot(googlemap, main="Google Maps subROI")
           plot(temp, breaks=seq(0, 1, 1/16), col=rev(heat.colors(16, alpha = 0.35)), add=T, legend = F) },
         
         layers = { 
           if(plotLayers=="all"){
             windows(w=10.5, h=3, xpos=1690, ypos=560)  
             clearPlot()
             temp <- list()
             for(i in 1:length(x$layers)){
               dataLayer <- paste(names(x$layers)[i])
               temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
             } 
             temp2 <- list()
             for(k in 1:length(temp)){
               dataLayer <- paste(names(temp)[k])
               temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
             }
             Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
             #Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)), legendRange = 0:1)
             if("water" %in% names(x$layers)) {
               setColors(x$layers$water$raster) <- "lightblue"
               for(m in names(temp)) {
                 waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
                 Plot(waterCrop, addTo=m) } }
           } else { 
             temp <- list()
             for(i in 1:length(x$layers)){
               if(names(x$layers[i]) %in% plotLayers){
                 dataLayer <- paste(names(x$layers)[i])
                 temp[[dataLayer]] <- x$layers[[dataLayer]]$risk
               }
             }
             temp2 <- list()
             for(k in 1:length(temp)){
               dataLayer <- paste(names(temp)[k])
               temp2[[dataLayer]] <- raster::crop(temp[[k]], x$ROIs$ROIsub)
             }
             Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)))
             #Plot(temp2, title=TRUE ,cols=rev(heat.colors(16)), legendRange = 0:1)
             if("water" %in% names(x$layers)) {
               setColors(x$layers$water$raster) <- "lightblue"
               for(m in names(temp)) {
                 waterCrop <- raster::crop(x$layers$water$raster, x$ROIs$ROIsub)
                 Plot(waterCrop, addTo=m) } }
           }
           
         }
  )
}


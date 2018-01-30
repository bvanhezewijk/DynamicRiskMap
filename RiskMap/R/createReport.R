# createReport()
# function to create PDF file/maps for positive trap catches - zoom level 13
# Arguments: projectList, fileName for PDF output, 
#            basemap type ("roadmap", "satellite", "terrain", "hybrid"), circleRadius (default= 1609 (ie. 1 mile))
# KSchurmann, March 17, 2017
#
#' Generate PDF report for positive trap catches
#'
#' Creates a PDF with an overall region map showing all positive traps, and 
#' individual local maps for each positive trap.
#'
#' @param x List created by \code{projectList} function
#' @param fileName Character string; name of PDF report file output
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'} (default), \code{'hybrid'},
#'    or \code{'terrain'}
#' @param addCircles Logical; if \code{TRUE}, circles plotted around positive traps (default)
#' @param circleRadius Numeric; radius in raster units of circle around 
#'   positive traps. Default = 1609 m (1 mile)
#' @return A PDF report saved to the projectDirectory
#' @details \code{createReport} calls the \code{pdf} function which opens the
#'   file \code{fileName} and the PDF commands needed to plot the maps are sent 
#'   to that file. \code{pdf} argument \code{onefile} is set to code{TRUE} so all 
#'   maps are added to the same PDF report. 
#'   
#'   The output report includes a map of the entire ROI showing positive 
#'   trap locations and individual zoomed in maps for each positive trap.
#'   If positive traps are less than 3 km apart, they are included on the 
#'   same map. 
#' @seealso \code{pdf}
#' @export
createReport <- function(x, fileName, basemap='roadmap', 
                         addCircles=TRUE, circleRadius=1609){
  posCatch <- subset(x$layers$traps$points, Catch>0)
  posCatchGoog <- sp::spTransform(subset(x$layers$traps$points, Catch>0), CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))

  plot_gmap<-function(ras,...){
    cols<-ras@legend@colortable
    z<-raster::unique(ras)
    par(mar=c(0,0,0,0))
    plot(ras,col=cols[z+1],legend=F,box=F,axes=F,legend.mar=0,...)
  }
  
  pdf(paste0(x$projectDir,"/",fileName), width=7,height=9, onefile=TRUE, paper = "letter")
  
  # entire extent map
  extentGoog <- sp::spTransform(x$layers$traps$points, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  googleMapROI <- dismo::gmap(x = extentGoog, type = basemap, lonlat=TRUE, zoom=10)
  plot_gmap(googleMapROI)
  box <- as(raster::extent(googleMapROI), 'SpatialPolygons')
  plot(box, add=T)
  points(posCatchGoog, add=T, pch=16)
  legend(min(sp::coordinates(googleMapROI)[,1])+0.0006, min(sp::coordinates(googleMapROI)[,2])+0.027, legend=paste0(posCatchGoog$Year[1]," capture sites                                    "), 
         pch=16, cex=0.8, pt.cex=1.2, bg="white")
  raster::scalebar(d=10, xy=c(min(sp::coordinates(googleMapROI)[,1])+0.20, min(sp::coordinates(googleMapROI)[,2])+0.010),
           type="bar", divs=4, lonlat=TRUE, below="km", cex=0.7)
  maptools::pointLabel(x=sp::coordinates(posCatchGoog)[,1], y=sp::coordinates(posCatchGoog)[,2], labels=as.character(posCatchGoog$trapID), cex=1, font=2)
  text((max(sp::coordinates(googleMapROI)[,1])+min(sp::coordinates(googleMapROI)[,1]))/2, max(sp::coordinates(googleMapROI)[,2])+0.02, 
       label=paste0(x$projectName,": Positive Trap Locations"), font=2, cex=1.5)
  
  # plotting traps
  trapLabels <- c()
  for(i in 1:length(posCatch)) {
    if(posCatch$trapID[i] %in% trapLabels){
    } else{
      xmin <- sp::coordinates(posCatch)[i,][1] - 3000
      xmax <- sp::coordinates(posCatch)[i,][1] + 3000
      ymin <- sp::coordinates(posCatch)[i,][2] - 3000
      ymax <- sp::coordinates(posCatch)[i,][2] + 3000
      trapROI <- raster::extent(xmin, xmax, ymin, ymax)
      trapCrop <- raster::crop(posCatch, trapROI)
      
      if(length(trapCrop)>1){
        centreXCoord <- mean(sp::coordinates(trapCrop)[,1])
        centreYCoord <- mean(sp::coordinates(trapCrop)[,2])
        trapROI <- raster::extent(centreXCoord-3000, centreXCoord+3000, centreYCoord-3000, centreYCoord+3000)
        trapLabels <- c(trapLabels, as.character(trapCrop$trapID))
      } else {trapLabels <- c(trapLabels, trapCrop$trapID)}
      
      
      googExt <- raster::extent(raster::projectExtent(raster::raster(trapROI, crs=raster::crs(posCatch)), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
      googleMap <- dismo::gmap(x = googExt, type = basemap, lonlat=TRUE, zoom=13)
      
      # selecting only spatial points in map extent
      if(length(trapCrop) > 1){ 
        temp <- list()
        for(i in 1:length(trapCrop)){
          temp[i] <- subset(posCatchGoog, trapID==trapCrop$trapID[i]) }
        trapSub <- do.call(maptools::spRbind, temp) 
      } else{ trapSub <- subset(posCatchGoog, trapID==trapCrop$trapID) }
      
      # plotting 
      plot_gmap(googleMap)
      box2 <- as(raster::extent(googleMap), 'SpatialPolygons')
      plot(box2, add=T)
      points(trapSub, pch=16, cex=1)
      if(addCircles==TRUE){
        plot(sp::polygons(dismo::circles(sp::coordinates(trapSub), d=1609, lonlat=TRUE, n=500, dissolve=FALSE)), add=T)
      }
      maptools::pointLabel(x=sp::coordinates(trapSub)[,1], y=sp::coordinates(trapSub)[,2], labels=as.character(trapSub$trapID), cex=1, font=2)
      legend(min(sp::coordinates(googleMap)[,1])+0.00008, min(sp::coordinates(googleMap)[,2])+.0032, legend=paste0(posCatchGoog$Year[1]," capture sites                                    "), 
             pch=16, cex=0.8, pt.cex=1.2, bg="white")
      raster::scalebar(d=1, xy=c(min(sp::coordinates(googleMap)[,1])+0.026,min(sp::coordinates(googleMap)[,2])+0.0012),
               type="bar", divs=2, lonlat=TRUE, below="km", cex=0.7)
      
      # labels and headings 
      text(googExt[1]+0.005, googExt[3]-0.003, label=paste0("Trap:"), cex=1)
      text(googExt[1]+0.025, googExt[3]-0.003, label=paste0("Captures:"), cex=1)
      text((googExt[2]+googExt[1])/2, googExt[3]-0.003, label=paste0("Location:"), cex=1, adj=1)
      # trap info
      if(length(trapSub)>1){
        for(j in 1:length(trapSub)){
          text(googExt[1]+0.005, googExt[3] - 0.003 -(0.0025*j), label=trapSub$trapID[j], cex=1)
          text(googExt[1]+0.025, googExt[3] - 0.003 -(0.0025*j), label=trapSub$Catch[j], cex=1)
          text(googExt[1]+0.05, googExt[3] - 0.003 -(0.0025*j), 
               label=paste0(round(sp::coordinates(trapSub)[j,][2],2),",  ",round(sp::coordinates(trapSub)[j,][1],2)), 
               cex=1, adj=0.4)
        }
      } else {
        text(googExt[1]+0.005, googExt[3] - 0.003 -(0.0025), label=trapSub$trapID, cex=1)
        text(googExt[1]+0.025, googExt[3] - 0.003 -(0.0025), label=trapSub$Catch, cex=1)
        text(googExt[1]+0.05, googExt[3] - 0.003 -(0.0025), 
             label=paste0(round(sp::coordinates(trapSub)[2],2),",  ",round(sp::coordinates(trapSub)[1],2)), 
             cex=1, adj=0.4) }
    }
  }
  dev.off()
}

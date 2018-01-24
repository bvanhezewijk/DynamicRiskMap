# Function to interactively define a Region of Interest (ROI) from a supplied map
# The function also defines a subROI which is a spatial subset of the ROI.
# If an initial ROI is supplied as an extent, it must have the same projection as the supplied map.
# B. Van Hezewijk Feb 23, 2017
#
#' Select ROI and subROI from map
#'
#' Displays map and prompts user to select region of interest (ROI)
#' and ROIsub interactively. This function is called from within 
#' \code{projectList} when arguments \code{ROI} or\code{ROIsub} is \code{NULL}.
#' 
#' @param mapOutline Map outline to be displayed; must be projected
#' @param roi Region of interest; extent object, a matrix or vector 
#'   of four numbers
#' @param roisub Subregion of interest; extent object, a matrix or 
#'   vector of four numbers
#' @return List with ROI, ROIsub and projection
#' @details A projected map or spatial object is needed for interactively 
#' selecting extents. Selecting ROI allows for zooming in multiple times 
#' until the desired extent is chosen. 
#' 
#' Values for ROI, ROIsub and projection are added to the projectList if
#' \code{getROI} is called from within \code{projectList}
#' @export
getROI<-function(mapOutline=NULL,roi=NULL,roisub=NULL){
  if (is.null(mapOutline)) {
    writeLines("A map or other spatial object must be supplied")
  }
  questionYN<-function(x){
    writeLines(x)
    out<-scan(what="character",nmax=1,quiet=T)
    out
  }
  if (!is.null(roi)){
    mapOutline <- raster::crop(mapOutline,roi)
  }
  raster::plot(mapOutline)
  maplim <- raster::extent(mapOutline)
  graphics::text((maplim[1]+maplim[2])/2,maplim[4],"Select ROI ",col="red",pos=3,offset=0.1)

  if (is.null(roi)) {
    repeat {
      roi <- drawExtent(show=TRUE)
      mapOutline<- raster::crop(mapOutline,roi)
      raster::plot(mapOutline,new=FALSE)
      graphics::text((roi[1]+roi[2])/2,roi[4],"Select ROI or press y in the console to zoom in.",col="red",pos=3,offset=0.1)
      ans<-questionYN("Zoom in again?  y/n :")
      if (ans != "y") break
    }
  }
  if (is.null(roisub)) {
    raster::plot(mapOutline,new=FALSE)
    graphics::text((roi[1]+roi[2])/2,roi[4],"Select subROI",col="blue",pos=3,offset=0.1)
    roisub <- raster::drawExtent(show=TRUE,col="blue")
  }
  out<-list(ROI=roi,ROIsub=roisub,projection=raster::crs(mapOutline))
  out
}


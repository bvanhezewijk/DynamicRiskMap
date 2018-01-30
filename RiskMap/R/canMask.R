#' Canada map outline
#'
#' Projected map of Canada for use in \code{getROI} and \code{projectList} functions
#'
#' @docType data
#'
#' @usage data(canMask)
#'
#' @format An object of class \code{SpatialPolygonsDataFrame}; see sp package. Projected
#'   crs is "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'   +datum=NAD83 +units=m +no_defs +towgs84=0,0,0". 
#'
#' @keywords datasets
#'
#' @examples
#' data(canMask)
#' plot(canMask)
#' getROI(mapOutline=canMask)
"canMask"


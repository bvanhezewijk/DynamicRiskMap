# plotInteract()
# Function to create leaflet map for interactive zooming and clicking on trap locations
# Agruments: projectList, basemap type ("satellite", "roadmap", "hybrid", "terrain"), 
#            mapRisk=T/F, mapTraps=T/F, mapHiRisk=T/F, hiRisk for plotting hiRisk raster           
# KSchurmann March 10, 2017
#
# can save directly to projectList (ex. projectList <- plotInteract(projectList))
#
#' Interactive leaflet risk map
#'
#' Builds a leaflet map with ability to interactively zoom in on areas
#' and click on traps. Map can optionally show totalRisk, trap locations
#' and high risk areas.
#' 
#' @param x List created by \code{projectList} function
#' @param basemap Basemap type: \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'} (default), 
#'   or \code{'terrain'}
#' @param mapRisk Logical; if \code{TRUE}, totalRisk raster is added to basemap
#' @param mapTraps Logical; if \code{TRUE}, trap locations are added to basemap
#' @param mapHiRisk Logical; if \code{TRUE}, hiRisk raster is added to basemap
#' @param hiRisk Numeric; if \code{mapHiRisk=TRUE}, value between 0 and 1 that 
#'   defines minimum risk value classified as high risk. Default=0.5.
#' @return Interactive leaflet map with selected risk and data layers
#' @details \code{plotInteract} opens a webpage or, if using RStudio, a Viewer 
#'   window and displays an interative leaflet map of the ROI.
#'   
#'   If \code{mapRisk = TRUE} (default), the totalRisk raster of plotted
#'   over the basemap. If \code{mapHiRisk = TRUE}, areas of high risk, defined by
#'   the \code{hiRisk} argument are highlighted in red. If \code{mapTraps = TRUE} 
#'   (default), red pins and blue circles show locations of positive and negative 
#'   traps, respectively. Clicking on red pins displays the trapID, location and 
#'   catch number. 
#'   
#'   Basemap options include \code{'satellite'}, \code{'roadmap'}, \code{'hybrid'}, and \code{'terrain'}
#'   and can be viewed at \url{http://leaflet-extras.github.io/leaflet-providers/preview/}.
#'   ProviderTiles for the basemap options are
#'   \itemize{
#'   \item \code{satellite}: Esri.WorldImagery
#'   \item \code{roadmap}:   Esri.WorldStreetMap
#'   \item \code{hybrid}:    Esri.WorldImagery & Stamen.TopOSMFeatures
#'   \item \code{terrain}:   Stamen.Terrain }
#' @seealso Functions in the \code{leaflet} package
#' @export
plotInteract <- function(x, basemap='hybrid', mapRisk=TRUE, 
                         mapTraps=TRUE, mapHiRisk=FALSE, hiRisk=0.5){
  map <- leaflet::leaflet()
  ext <- raster::extent(raster::projectExtent(x$layers[[1]]$raster, crs="+proj=longlat +datum=WGS84"))
  map <- leaflet::fitBounds(map, lng1=ext[1], lat1=ext[3], lng2=ext[2], lat2=ext[4])
  
  switch(basemap,
         satellite = { map <- leaflet::addProviderTiles(map, "Esri.WorldImagery") },
         roadmap = {  map <- leaflet::addProviderTiles(map, "Esri.WorldStreetMap") },
         hybrid = { map <- leaflet::addProviderTiles(map, "Esri.WorldImagery")
                    map <- leaflet::addProviderTiles(map, "Stamen.TopOSMFeatures", options = providerTileOptions(opacity = 0.75)) },
         terrain = { map <- leaflet::addProviderTiles(map, "Stamen.Terrain") })
  
  if(mapRisk==TRUE){
    totalRiskLeaflet <- leaflet::projectRasterForLeaflet(x$totalRisk)
    if("water" %in% names(x$layers)) waterLeaflet <- leaflet::projectRasterForLeaflet(x$layers$water$raster)
    totalRiskLeaflet[totalRiskLeaflet<=0] <- NA
    if("water" %in% names(x$layers)) totalRiskLeaflet <- raster::mask(totalRiskLeaflet, waterLeaflet, inverse=TRUE)
    
    map <- leaflet::addRasterImage(map, totalRiskLeaflet, colors=rev(heat.colors(16)), opacity=0.35, project=FALSE)
  }
  
  if(mapTraps==TRUE){
    trapPoints <- sp::spTransform(x$layers$traps$point, CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))
    negTraps <- subset(trapPoints, Catch==0)
    posTraps <- subset(trapPoints, Catch>0)
    posTraps$popup <- paste0("Trap ", posTraps$trapID,"<BR>",
                             "Catch: ",posTraps$Catch,"<BR>Location: ",
                             round(sp::coordinates(posTraps)[,2],2),", ",round(sp::coordinates(posTraps)[,1],2))
    
    map <- leaflet::addMarkers(map, lng = sp::coordinates(posTraps)[,1], lat = sp::coordinates(posTraps)[,2], layerId = NULL, group = NULL,
                      icon = leaflet::icons(iconUrl="http://maps.google.com/mapfiles/kml/paddle/red-circle.png", iconWidth = 40, iconHeight = 40), 
                      popup = posTraps$popup)
    map <- leaflet::addCircles(map, lng = sp::coordinates(negTraps)[,1], lat = sp::coordinates(negTraps)[,2], opacity = 1)
  }
  
  if(mapHiRisk==TRUE){
    highRisk <- raster::reclassify(x$totalRisk, matrix(c(0,hiRisk, NA, hiRisk,1,1),ncol=3,byrow=T), include.lowest=TRUE)
    highRiskLeaflet <- leaflet::projectRasterForLeaflet(highRisk)
    if("water" %in% names(x$layers)) waterLeaflet <- leaflet::projectRasterForLeaflet(x$layers$water$raster)
    highRiskLeaflet[highRiskLeaflet<=0] <- NA
    if("water" %in% names(x$layers)) highRiskLeaflet <- raster::mask(highRiskLeaflet, waterLeaflet, inverse=TRUE)
    
    map <- leaflet::addRasterImage(map, highRiskLeaflet, 
                          colors=leaflet::colorNumeric(colorRampPalette(c("red"))(1), domain=1, na.color = "#00000000"), 
                          opacity=0.35, project=FALSE)
  }
  
  x$mapInteract <- map
  map
  return(x)
}



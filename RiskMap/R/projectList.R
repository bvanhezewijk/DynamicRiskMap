# projectList()
# Function to define project parameters and create parameter list
# Arguments: projectName, species, layers, ROI, ROIsub, resolution
# KSchurmann Feb 28, 2017
#
#' Create a projectList for a new risk mapping project
#'
#' Create and populate a project list that will be used for all other 
#' functions in the package. This function provides the base information
#' required for \code{loadData}, \code{calculateRisk}, \code{combineRisk}
#' functions and many others. 
#'
#' @param projectName Character string naming the risk mapping project.
#'   Defaults to 'RiskMapProject'
#' @param species Character string specifying species for project; 
#'   must be listed in SpeciesRiskParams.csv
#' @param layers List of data layers to include
#' @param ROI Region of interest; extent or raster object, a matrix or 
#'   vector of four numbers, or character string specifying a predetermined \code{ROI} 
#' @param ROIsub Subregion of interest; extent or raster object, a matrix or 
#'   vector of four numbers, or character string specifying a predetermined \code{ROIsub} 
#' @param resolution Resolution; single number of vector of two numbers. Defaults
#'   to 30m
#' @param map Map outline to be displayed if \code{ROI} or \code{ROIsub} is \code{NULL}; must 
#'   be projected
#' @return A list containing project parameters
#' @seealso \code{getROI}
#' @details This function is the building block for all other functions in this
#'   package. Data and raster layers are added to this list as they are loaded and 
#'   calculated by other functions. 
#'   
#'   Water is recommended to be included as a layer. Risk is not calculated for the
#'   water layer but is used to mask over areas where risk=0.
#'   
#'   The region of interest (ROI) can be selected from the list of predefined 
#'   extents (below) or left blank. If blank, \code{getROI} will automatically 
#'   prompt the selection of an \code{ROI} and \code{ROIsub} from the map shown. \code{getROI} 
#'   requires a projected map to be specified in order to select the extent. 
#'   
#'   Predefined ROI extents:
#'   \tabular{lrrrr}{
#'   ROI	\tab	xmn	\tab	xmx	\tab	ymn	\tab	ymx	\cr
#'   Canada	\tab	-2317000	\tab	3092154	\tab	298781.1	\tab	4811413	\cr
#'   BC	\tab	-2350000	\tab	-1276830	\tab	1135380	\tab	3002980	\cr
#'   SouthwestBC	\tab	-2118298	\tab	-1770148	\tab	1255505	\tab	1546415	\cr
#'   VancouverIsland	 \tab	-2040398	\tab	-1948121	\tab	1312569	\tab	1546415	\cr
#'   LowerMainland	\tab	-1942000	\tab	-1873500	\tab	1359500	\tab	1546415	\cr }
#'   
#'   Predefined ROIsub extents:
#' \tabular{lrrrr}{
#'   ROIsub	\tab	xmn	\tab	xmx	\tab	ymn	\tab	ymx	\cr
#'   Canada.sub	\tab	-1917894	\tab	-1904337	\tab	1399578	\tab	1411426	\cr
#'   BC.sub	\tab	-1917894	\tab	-1904337	\tab	1399578	\tab	1411426	\cr
#'   SouthwestBC.sub	\tab	-1917894	\tab	-1904337	\tab	1399578	\tab	1411426	\cr
#'   VancouverIsland.sub	 \tab	-1986703	\tab	-1971396	\tab	1333124	\tab	1348818	\cr
#'   LowerMainland.sub	\tab	-1917894	\tab	-1904337	\tab	1399578	\tab	1411426	\cr }
#'   Predefined extent projection: "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 
#'   +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#' @export
projectList <- function(projectName='RiskMapProject', species, 
                        layers, ROI=NULL, ROIsub=NULL, resolution=c(30), 
                        map=canMask){
  parameterList <- list()
  
  parameterList$projectName <- projectName
  
  parameterList$species <- species
  speciesParams <- subset(read.csv("./Species Risk Parameters/SpeciesRiskParams.csv",stringsAsFactors = FALSE), species==Species)
  
  temp <- list()
  for(i in layers) {
    dataLayer <- paste(i)
    temp[[dataLayer]]$params <- subset(speciesParams,Layer==i, select=c(3:ncol(speciesParams)))
  }
  parameterList$layers <- temp
  
  parameterList$resolution <- resolution
  
  ROIDefined <- data.frame(ROI    =        c( "xmn",     "xmx",    "ymn",     "ymx"),
                           Canada =        c(-2317000,  3092154,  298781.1,  4811413),
                           BC     =        c(-2350000, -1276830,  1135380,   3002980),
                           SouthwestBC =   c(-2118298, -1770148,  1255505,   1546415),
                           VancouverIsland=c(-2040398, -1948121,  1312569,   1434588),
                           LowerMainland = c(-1942000, -1873500,  1359500,   1420300))
  if(!is.null(ROI)){
    if (as.vector(ROI) %in% names(ROIDefined)) { 
      parameterList$ROIs$ROI <- raster::extent(ROIDefined[,match(as.vector(ROI), names(ROIDefined))])
    } else {parameterList$ROIs$ROI <- raster::extent(ROI) } }
  
  
  ROIsubDefined <- data.frame(ROIsub     =        c( "xmn",      "xmx",    "ymn",    "ymx"),
                              Canada.sub =        c(-1917894,  -1904337,  1399578,  1411426),
                              BC.sub     =        c(-1917894,  -1904337,  1399578,  1411426),
                              SouthwestBC.sub =   c(-1917894,  -1904337,  1399578,  1411426),
                              VancouverIsland.sub=c(-1986703,  -1971396,  1333124,  1348818),
                              LowerMainland.sub = c(-1917894,  -1904337,  1399578,  1411426))
  if(!is.null(ROIsub)){
    if (as.vector(ROIsub) %in% names(ROIsubDefined)) { 
      parameterList$ROIs$ROIsub <- raster::extent(ROIsubDefined[,match(as.vector(ROIsub), names(ROIsubDefined))])
    } else {parameterList$ROIs$ROIsub <- raster::extent(ROIsub) } }
  
  parameterList$ROIs <- getROI(map,parameterList$ROIs$ROI, parameterList$ROIs$ROIsub)
  
  #Naming project directory
  if(is.character(ROI)){
    tempPath <- file.path(paste0(getwd(),"/projectDirectories/",species,"_",ROI))
  } else { tempPath <- file.path(paste0(getwd(),"/projectDirectories/",species, "_", 
                                        "SW",round(parameterList$ROIs$ROI[1], digits = 1),
                                        ",",round(parameterList$ROIs$ROI[3], digits = 1), "_",
                                        "NE",round(parameterList$ROIs$ROI[2], digits = 1),
                                        ",",round(parameterList$ROIs$ROI[4], digits = 1)) ) }
  
  #Checking if project directory exists - if yes, option to continue using existiting directory or to create new one
  questionYN<-function(x){
    writeLines(x)
    out<-scan(what="character",nmax=1,quiet=T)
    out
  }
  
  nameDir <- function() {
    ANSWER <- readline("Type name of new directory: ")
    newPath <- file.path(paste0(getwd(),"/projectDirectories/", ANSWER))
    parameterList$projectDir <- newPath
    dir.create(file.path(parameterList$projectDir), recursive=TRUE, showWarnings = FALSE)
    return(parameterList)
  }
  
  if(dir.exists(file.path(tempPath))==FALSE) {
    parameterList$projectDir <- tempPath
    dir.create(file.path(parameterList$projectDir), recursive=TRUE) 
  } else { 
    ans <- questionYN("Directory already exists. Use existing directory? y/n : ") 
    if (ans == "y" | ans == "Y") { 
      parameterList$projectDir <- tempPath
      dir.create(file.path(parameterList$projectDir), recursive=TRUE, showWarnings = FALSE) 
    } else { parameterList <- nameDir() } }
  
  return(parameterList)
}


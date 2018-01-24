# loadData()
# Function to load data layers specified in x list
#     Each layer needs an associated formatFunction in species parameters csv
# Agruments: data library directory (dataLibraryPath) and project parameters list (x)
# KSchurmann Feb 27, 2017
#
# Output file names are hardcoded but could add an argument to specify list of names same length as layers?
#
#' Load data layers
#'
#' Gets data from data library using \code{format} functions. Each
#' layer must have appropriate \code{format} function specified in
#' formatFunction column of SpeciesRiskParams.csv file.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @return projectList with raster (and SpatialPointsDataFrame for point
#' data) for each data layer
#' @details \code{loadData} calls the individual \code{format} function 
#'   for each data layer. \code{format} functions are specified in the 
#'   SpeciesRiskParams.csv and retrieve data from the dataLibrary,
#'   save the raw data, format, rasterize and add data to the 
#'   projectList. Raw data files are cropped and saved in the 
#'   projectDirectory as "_raw" files. 
#' @seealso \code{format} functions for each data layer
#' @export
loadData <- function(x, dataLibraryPath){
  for(i in 1:length(x$layers)){
    func <- x$layers[[i]]$params$formatFunction
    outputFileName <- paste0(names(x$layers[i]), "_raw")
    x <- eval(call(func, x, dataLibraryPath, outputFileName))
  }
  return(x)
}




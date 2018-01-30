# unarchiveProject() function
# getwd() needs to be same working dirctory where "./projectDirectories" is located
#
#' Unarchive an existing project 
#'
#' Restore a project from an archived .tar.gz file. ProjectLists saved as 
#' .rds files can be loaded using the \code{readRDS} function.
#' 
#' @param fileName Character string; name of the archived file
#' @param extDir The directory to extract files to. Defaults to working directory
#' @param listFiles Logical; if \code{TRUE}, lists files within tar archive. Otherwise 
#'   extracts files (default)
#' @return Unarchived project files
#' @seealso \code{untar},   \code{readRDS}
#' @export
unarchiveProject <- function(fileName, extDir=getwd(), listFiles=FALSE){
  untar(tarfile = fileName, exdir=extDir, list=listFiles)
}





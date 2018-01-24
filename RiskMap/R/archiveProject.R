# archiveProject() function
# getwd() needs to be same working dirctory where "./projectDirectories" is located
#
#' Archive risk mapping project
#'
#' Save projectList and projectDirectory as a .tar.gz compressed 
#' archive file. The working directory must be set to path where "./projectDirectories"
#' is located.
#' 
#' @param x List created by \code{projectList} function
#' @param fileName Character string; name of the file where the data will be saved
#' @param archiveDir The directory path to save the archive file. Defaults to 
#'   \code{paste0(getwd(),"/archivedProjects")}
#' @param saveProjectDir Logical; if \code{TRUE}, saved projectList and projectDirectory (default).
#'   Otherwise, saves only projectList \code{x}
#' @details When archiving the projectList and projectDirectory, files are archived 
#'   using the \code{tar} function. Archiving only a projectList saves the list as 
#'   an .rds file using \code{saveRDS} that can be loaded into R using later the \code{readRDS} 
#'   function. If the archiveDir does not exist, it will be created.
#'   
#'   The working directory must be set to folder where "./projectDirectories"
#'   is located. 
#' @seealso \code{tar} \code{saveRDS}
#' @return A .tar.gz or .rds file
#' @export
archiveProject <- function(x, fileName, 
                           archiveDir=paste0(getwd(),"/archivedProjects"), 
                           saveProjectDir=TRUE) {
  dir.create(file.path(archiveDir), recursive=TRUE, showWarnings = FALSE)
  
  if(saveProjectDir==TRUE){
    saveRDS(x, file=paste0(x$projectDir,"/",fileName,".rds"))
    
    splitDir <- unlist(strsplit(x$projectDir, split="/"))
    tempFile <- splitDir[length(splitDir)]
    tar(tarfile = paste0(archiveDir,"/", fileName,".tar.gz"), files= paste0("./projectDirectories/",tempFile))
  } else {
    saveRDS(x, file=paste0(archiveDir,"/",fileName,".rds"))
  }
}




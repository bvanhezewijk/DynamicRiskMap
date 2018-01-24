# formatLCC()
# Function to locate AAFC landcover data, crop, save to projectDirectory as layer name
# Agruments: data library directory (dataLibraryPath), name of raw data output file to save in projectDirectory (outputFileName) and 
#           project parameters list (x)
# KSchurmann March 2, 2017
# 
#' Format AAFC landcover data
#'
#' Retrieves AAFC landcover data from data library, reprojects to project CRS,
#' resolution and ROI. Reclassifies raster based on species parameters from
#' .csv file. Saves raster in projectDir and adds to projectList.
#' 
#' @param x List created by \code{projectList} function
#' @param dataLibraryPath File path to data library directory
#' @param outputFileName Name of file to be created; defaults to 
#'  "treeCover_raw" when function called using \code{loadData}
#' @return projectList with treeCover data raster and saves treeCover data 
#'   to the projectDirectory
#' @export
formatLCC <- function(x, dataLibraryPath, outputFileName){
  if(paste0("treeCover.tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
    if(paste0(outputFileName,".tif") %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE) | "water_raw.tif" %in% list.files(x$projectDir, pattern="tif$", full.names=FALSE)==FALSE){
      template <- raster::raster(ext=x$ROIs$ROI, resolution=x$resolution, crs=x$ROIs$projection)
      temp <- raster::projectRaster(from=raster::raster(paste0(dataLibraryPath,"/LandCover/AAFC/2015/ACGEO_2015_CI_BC_30m_v1.tif")), to=template, method="ngb")
      raster::writeRaster(temp, paste0(x$projectDir,"/",outputFileName,".tif"), format="GTiff", overwrite=TRUE) }
    
    lcc <- raster::raster(paste0(x$projectDir,"/",outputFileName,".tif"))
    
    lccClassLegend <- read.csv(paste0(dataLibraryPath,"/LandCover/AAFC/2015/aci_crop_classifications_iac_classifications_des_cultures.csv"))
    names(lccClassLegend)[2] <- "Label"
    temp <- list()
    for(i in strsplit(x$layers$treeCover$params$lccClass,split = ",")){
      temp[i] <- lccClassLegend$Code[charmatch(i, lccClassLegend$Label)]
    }
    temp.df <- data.frame(lccClassLegend$Code)
    temp.df$Reclass <- ifelse(temp.df[,1] %in% temp, 1, 0)
    treeCover <- raster::subs(x=lcc, y=temp.df)
    names(treeCover) <- c('treeCover')
    
    raster::writeRaster(treeCover, paste0(x$projectDir, "/treeCover.tif"), format="GTiff", overwrite=TRUE) }
  
  x$layers$treeCover$raster <- raster::raster(paste0(x$projectDir,"/treeCover.tif"))
  return(x)
}

               
               
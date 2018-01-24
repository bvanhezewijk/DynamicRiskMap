# calculateRisk()
# Function to create risk raster for each data layer
# Agruments: projectList
# KSchurmann Feb 28, 2017
#
### species specific weighting and direction parameters??
#
#' Calculate risk for data layers
#'
#' Creates risk raster for each data layer using \code{focalWeightRisk} and
#' \code{focal} functions, and SpeciesRiskParams.csv file.
#'
#' @param x List created by \code{projectList} function
#' @return projectList with risk rasters for each data layer. Raster values 
#'   are between 0 and 1.
#' @details Data rasters are translated into risk rasters using the parameters
#'   listed in the SpeciesRiskParams.csv and calls to \code{translateRisk2D}, 
#'   \code{focalWeightRisk} and \code{raster::focal} functions. Maximum risk is 
#'   scaled to 1.
#' @seealso \code{focalWeightRisk}, \code{translateRisk2D}, \code{focalWeight}, \code{raster::focal}
#' @export
calculateRisk <- function(x){
  for(i in 1:length(x$layers)){
    if(names(x$layers[i]) != "water"){
      dataLayer <- paste(names(x$layers)[i])
      temp <- x$layers[[i]]
      fw <- focalWeightRisk(temp$raster, type=temp$params$type, a=temp$params$a, b=temp$params$b, riskCutoff=temp$params$riskCutoff)
      risk <- raster::focal(temp$raster, w=fw, pad=T, padValue=0)
      risk <- risk/max(raster::values(risk), na.rm=T) #  scale to 1 - can remove and scale elsewhere
      #risk <- risk * ifelse(temp$params$direction=="increase", 1, -1) * as.numeric(temp$params$weight)
      names(risk) <- c(paste0(dataLayer, "Risk"))
      raster::writeRaster(risk, paste0(x$projectDir,"/",dataLayer,"Risk.tif"), format="GTiff", overwrite=TRUE)
      rm(risk)
      
      x$layers[[dataLayer]]$risk <- raster::raster(paste0(x$projectDir,"/",dataLayer,"Risk.tif"))
    } }
  return(x)
}

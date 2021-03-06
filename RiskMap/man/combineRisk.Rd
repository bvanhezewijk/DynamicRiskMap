% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineRisk.R
\name{combineRisk}
\alias{combineRisk}
\title{Combine risk layers}
\usage{
combineRisk(x)
}
\arguments{
\item{x}{List created by \code{projectList} function}
}
\value{
projectList with totalRisk raster. Raster values are between 
  0 and 1.
}
\description{
Combines risk rasters generated with \code{calculateRisk} into a single
raster named totalRisk. Currently sums all risk rasters together. Scaling 
and weighting is not yet included.
}
\details{
Risk rasters are summed to create a totalRisk raster for the 
  ROI and the maximum risk value is scaled to 1.
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygonizeCoast.R
\name{polygonizeCoast}
\alias{polygonizeCoast}
\title{Helper function to getLandVal: Turns land mask matrix into polygons}
\usage{
polygonizeCoast(mask, landVal = 0)
}
\arguments{
\item{mask}{Matrix[lon,lat] with land mask. Land=0 (default) and water=1.}

\item{landVal}{integeer representing the land value in the mask}
}
\value{
Matrix[lon,lat] whith coast line ids
##' @details ...
}
\description{
Helper function to getLandVal: Turns land mask matrix into polygons
}

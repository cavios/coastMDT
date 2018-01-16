\name{eigen6c4r}
\alias{eigen6c4r}
\docType{data}
\title{
eigen6c4r
}
\description{
eigen6c4r is a geoid model based on GOCO5 and EIGEN-6C4. Eigen6c4r is a 2880 x
1440 matrix. The first cell is longitude 0 to 1/8 degree, latitude -90
to -90+1/8 degree cells are all 1/8 by 1/8 degree order is east to west, then south to north.

The unit is in meters. The geoid is referenced to the TOPEX ellipsoid
and is given in the permanent mean tide system. 
}
\usage{load("eigen6c4r.rda")}
\format{
A matrix[lon,lat] of dimension 2880 x 1440 
}
\details{
...
}
\source{
...
}
\references{
F\"{o}rste, Christoph; Bruinsma, Sean.L.; Abrikosov, Oleg; Lemoine, Jean-Michel; Marty, Jean Charles; Flechtner, Frank; Balmino, G.; Barthelmes, F.; Biancale, R. (2014): EIGEN-6C4 The latest combined global gravity field model including GOCE data up to degree and order 2190 of GFZ Potsdam and GRGS Toulouse. GFZ Data Services. http://doi.org/10.5880/icgem.2015.1
}
\examples{
load('eigen6c4r.rda')
}
\keyword{datasets}

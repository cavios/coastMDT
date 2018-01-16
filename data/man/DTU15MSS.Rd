\name{DTU15MSS}
\alias{DTU15MSS}
\docType{data}
\title{
DTU15MSS
}
\description{
DTU15MSS is the DTU15 mean sea surface (MSS). DTU15MSS is a 2880 x 1440
matrix. The first cell is longitude 0 to 1/8 degree, latitude -90 to
-90+1/8 degree cells are all 1/8 by 1/8 degree order is east to west, then south to north

The MSS is in meters and are referenced to the TOPEX ellipsoid. The grid
is in the permanent mean tide system. 
}
\usage{load("DTU15MSS.rda")}
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
Created by Ole B. Andersen \email{oa@space.dtu.dk}
}
\examples{
load("DTU15MSS.rda")
}
\keyword{datasets}

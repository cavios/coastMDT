\name{dDTU15MSS_ref2003_2007}
\alias{dDTU15MSS_ref2003_2007}
\docType{data}
\title{
dDTU15MSS_ref2003_2007
}
\description{
Grid to transform the DTU15 MSS (DTU15MSS) to the MSS of the period
2003-2007. The grid should be added. dDTU15MSS_ref2003_2007 is a 2880 x
1440 matrix. The unit is meter.

The first cell is longitude 0 to 1/8 degree, latitude -90 to -90+1/8 degree
cells are all 1/8 by 1/8 degree
order is east to west, then south to north
}
\usage{load("dDTU15MSS_ref2003_2007.rda")}
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
load("dDTU15MSS_ref2003_2007.rda")
}
\keyword{datasets}

\name{dacCor5Y_2003_2007}
\alias{dacCor5Y_2003_2007}
\docType{data}
\title{
dacCor5Y_2003_2007
}
\description{
Dynamic atmosphere corrections for the reference period 2003-2007. The
corrections are given on the following grid: 
  
2880 = longitudes (0.5+(0,1,2....2879))/8. degrees
1440 = latitudes  (0.5+(0,1,2....1439))/8. -90 degrees

These numbers should be added to ssh in metres to give ib-corrected ssh

They are derived from the DAC, as provided by aviso at 6-hour and 1/4 degree resolution.

dacCor5Y_2003_2007 represents the 5-year average of the period 2003 to
2007.

}
\usage{load("dacCor5Y_2003_2007.rda")}
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
Created by Christopher W. Hughes \email{cwh@noc.ac.uk}
}
\examples{
load("dacCor5Y_2003_2007.rda")
}
\keyword{datasets}


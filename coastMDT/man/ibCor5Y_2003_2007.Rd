\name{ibCor5Y_2003_2007}
\alias{ibCor5Y_2003_2007}
\docType{data}
\title{
ibCor5Y_2003_2007
}
\description{
Inverse barometer correction for the 5 year reference period given on
the following grid:

2880 = longitudes (0.5+(0,1,2....2879))/8. degrees
1440 = latitudes  (0.5+(0,1,2....1439))/8. -90 degrees

These numbers should be added to ssh in metres to give ib-corrected ssh

They are derived from monthly-mean sea-level pressure from the era-interim analysis, as provided by ECMWF at 1/4 degree resolution.
The conversion from pressure in Pa to sea level correction in m is given by
correction = -1.e-4*(0.99*(p-101100.)-0.974*(pglob-101100.))
where p is pressure and pglob is global-ocean-average pressure at the same time.

(the reason for the different coefficients, 0.99 and 0.974, is that global ocean
average pressure does produce a small sea level change due to compressibility of
seawater - without this effect, the formula would reduce to -1.e-4*0.99*(p-pglob) )

ibCor5Y_2003_2007 represents the 5-year average of the period 2003 to
2007.

NOTE: Gibbs fringes can be seen in both these products, slightly larger in the ib than the dac,
but are typically at the 1 mm level or less (the largest ocean values occur by the Pacific
South American coast).
}

\usage{data("ibCor5Y_2003_2007")}
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
data(ibCor5Y_2003_2007)
}
\keyword{datasets}

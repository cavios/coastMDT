\name{landmask8}
\alias{landmask8}
\docType{data}
\title{landmask8}
\description{
landmasks from GEBCO2014, on a 1/8 degree grid.
  
landmask8 is a 2880 x 1440 matrix

first cell is longitude 0 to 1/8 degree, latitude -90 to -90+1/8 degree
cells are all 1/8 by 1/8 degree
order is east to west, then south to north

value is 1 for any cell which is 50% or more ocean according to the gebco mask
         0 for any cell which is less than 50% ocean

isolated ocean points have been removed (excluding Black Sea and Sea of Marmara)


}
\usage{data(landmask8)}
\format{
   A matrix of dimension 2880 x 1440 
 }
\details{
points below sea level, but enclosed by land (e.g. Caspian Sea, Dead Sea) are here classed as land.

GEBCO2014 original data have been modified by hand to allow the Sea of Marmara and the
Black Sea to be connected. This involved converting 1 point in the Dardanelles (strait connecting
Sea of Marmara to the Mediterranean), and 3 points in the Bosphorus (strait connecting
Sea of Marmara to the Black Sea), from land to ocean. For reference, these points are:

i values (counting in range 1 to 43200) =  3167, 3484, 3486, 3486
j values (counting in range 1 to 21600) = 15618,15725,15726,15729


}
\source{
...
}
\references{
Create from GEBCO2014 by Chris Hughes 
  
}
\examples{
data(landmask8)
}
\keyword{datasets}

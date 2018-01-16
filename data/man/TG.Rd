\name{TG}
\alias{TG}
\docType{data}
\title{TG}
\description{
  Tide gauge information and data: MSL estimates relative to WGS84 must
  be created by adding the RLR heights to the MSL estimates for the
  respective reference periods.
  
}
\usage{load("TG.rda")}
\format{
 A data frame with 302 observations of the following variables.
  \describe{
    \item{\code{PSMSL_ID}}{a numeric vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{Station_Name}}{a factor vector}
    \item{\code{Coastline}}{a numeric vector}
    \item{\code{Station}}{a numeric vector}
    \item{\code{QC_Flag}}{a factor vector}
    \item{\code{GPS_type}}{a factor vector}
    \item{\code{GPS_code}}{a factor vector}
    \item{\code{Geoid_pt_LAT}}{a numeric vector}
    \item{\code{Geoid_pt_LONG}}{a numeric vector}
    \item{\code{RLR_ell_2005.5}}{a numeric vector}
    \item{\code{Uncertainty_2005.5}}{a numeric vector}
    \item{\code{MSL_2003_2007}}{a numeric vector}
    \item{\code{RLR_ell_2010.5}}{a numeric vector}
    \item{\code{Uncertainty_2010.5}}{a numeric vector}
    \item{\code{MSL_2008_2012}}{a numeric vector}
    \item{\code{IB_2003_2007}}{a numeric vector}
    \item{\code{IB_2008_2012}}{a numeric vector}
    \item{\code{DAC_2003_2007}}{a numeric vector}
    \item{\code{DAC_2008_2012}}{a numeric vector}
    \item{\code{egm08C}}{a numeric vector, EGM2008 geoid height above
      TOPEX ellipsoid. The values have been extracted the high
      resolution EGM2008}
    \item{\code{egm08H}}{a numeric vector, EGM2008 geoid height above
      TOPEX ellipsoid. The values have been extracted a 1/8 degree
      version of EGM2008}
    \item{\code{eigen6c4rC}}{a numeric vector, GOCO+Eigen6c4r geoid height above
      TOPEX ellipsoid. The values have been extracted a 1/8 degree
      version of Eigen6c4r }
  }   
}
\details{
  To construct MSL values above WGS84 for the reference period
  2003-2007; MSLWGS84={\code{RLR_ell_2005.5}}+
  {\code{MSL_2003_2007}}. MSLWGS84 will be in the tide free system.
}
\source{
...
}
\references{
Create by Mederic Gravelle and Karina Nielsen 
  
}
\examples{
load("TG.rda")
}
\keyword{datasets}

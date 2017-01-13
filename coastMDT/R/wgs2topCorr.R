##' Function that estiamtes the height difference between WGS84 and Topex ellipsoids 
##' @param phi The latitude in degrees. 
##' @return An array with the height differences.
##' @details ...
##' @export
wgs2topCorr<-function(phi){
    aWGS<-6378137.000000
    aTop<-6378136.300000
    bWGS<-6356752.314245
    bTop<-6356751.600563
    delta_h<--((aTop - aWGS) * cos(phi/180*pi)^2 + (bTop - bWGS) * sin(phi/180*pi)^2)
    return(delta_h)
}

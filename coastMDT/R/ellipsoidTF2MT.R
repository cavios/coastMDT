##' Function that estimates the correction to go from a tide free ellipsoid to a mean tide ellipsoid. 
##' @param phi The latitude in degrees.
##' @param h2 Love number, the default value is h2=0.62
##' @return An array with the height differences in meter.
##' @details ...
##' @export
ellipsoidTF2MT<-function(phi,h2=0.62){
    dhTF2MT<-h2*(0.099-0.296*sin(phi/180*pi)^2)
    return(dhTF2MT)
}

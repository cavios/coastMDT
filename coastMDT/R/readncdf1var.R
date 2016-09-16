##' Read net cdf file with one variable 
##'
##' This function \code{readncdf1var} ........
##' @param filename String containing the filename 
##' @param var String containing the variable name
##' @importFrom ncdf open.ncdf get.var.ncdf
##' @return Matrix var[longitude,latitude]
##' ##' @details ...
##' @export
##' @examples
##'\dontrun{
##' mydat<-readncdf1var('landmask8_anyland.nc','z')
##' }
readncdf1var<-function(filename,var){
    ncin <- open.ncdf(filename)
    return(get.var.ncdf(ncin,var))  
}

##' Read net cdf file with one variable 
##'
##' This function \code{readncdf1var} ........
##' @param filename String containing the filename 
##' @importFrom ncdf4 nc_open ncvar_get nc_close
##' @return Matrix var[longitude,latitude]
##' ##' @details ...
##' @export
##' @examples
##'\dontrun{
##' mydat<-readncdf1var('landmask8.nc')
##' }
readncdf1var<-function(filename){
    nc <- nc_open(filename)
    data <- ncvar_get(nc)
    nc_close(nc)
    return(data)  
}

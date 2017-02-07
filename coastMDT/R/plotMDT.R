##' Plot MDT grid 
##' @param dat An object as returned by the function 'getSubGrid' or 'iterativeAveSmoother', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). 
##' @param zlim Range of the MDT values given as a  Vector of length 2.
##' @param addContour Bolean; To add a contour plot. Default is TRUE
##' @param conlev The spacing between contour lines given in meters. The default is 0.05. 
##' @param ... Additional arguments to image.plot from fields
##' @importFrom fields image.plot
##' @details ...
##' @export
plotMDT<-function(dat,zlim,addContour=TRUE,conlev=0.05,...){
    image.plot(dat$lon,dat$lat,dat$g,legend.width = 1.5,zlim=zlim,...)
    if (addContour)
        {
            mylev=seq(zlim[1],zlim[2],by=conlev)
            contour(dat$lon,dat$lat,dat$g, add=TRUE,levels=mylev)
        }
}

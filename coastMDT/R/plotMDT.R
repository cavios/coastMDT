##' Plot MDT grid 
##' @param dat Matrix[lon,lat] with MDT values.
##' @param zlim Range of the MDT values given as a  Vector of length 2.
##' @param lonlim  Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param addContour Bolean; To add a contour plot. Default is TRUE
##' @param res Grid spacing
##' @param conlev The spacing between contour lines given in meters. The default is 0.05. 
##' @param ... Additional arguments to image.plot from fields
##' @importFrom fields image.plot
##' @details ...
##' @export
plotMDT<-function(dat,zlim,lonlim,latlim,addContour=TRUE,res=0.125,conlev=0.05,...){
    lonlim[1]<-lonlim[1]+(res/2)
    lonlim[2]<-lonlim[2]-(res/2)
    latlim[1]<-latlim[1]+(res/2)
    latlim[2]<-latlim[2]-(res/2)
    lon<-seq(lonlim[1],lonlim[2],by=res)
    lat<-seq(latlim[1],latlim[2],by=res)
    image.plot(lon,lat,mymdt,legend.width = 1.5,zlim=zlim,...)
    if (addContour)
        {
            mylev=seq(zlim[1],zlim[2],by=conlev)
            contour(lon,lat,mymdt, add=TRUE,levels=mylev)
        }
}

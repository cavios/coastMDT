##' Plot MDT grid 
##' @param dat An object as returned by the function 'getSubGrid' or 'iterativeAveSmoother', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). 
##' @param zlim Range of the MDT values given as a  Vector of length 2.
##' @param addContour Bolean; To add a contour plot. Default is TRUE
##' @param conlev The spacing between contour lines given in meters. The default is 0.05.
##' @param TGdat MDT tide gauge data file. The file TGdat must contain at least the columns; Longitude, Latitude, TGMDT. The default is NULL. 
##' @param ... Additional arguments to image.plot from fields
##' @importFrom fields image.plot tim.colors
##' @details ...
##' @export
##'
##'
plotMDT<-function(dat,zlim,addContour=TRUE,conlev=0.05,TGdat=NULL,legendUnit='m',...){
    nlevels=20
    Longitude<-dat$lon
    Latitude<-dat$lat
    image.plot(Longitude,Latitude,dat$g,legend.width = 1.5,zlim=zlim,xlab='Longitude',ylab='Latitude',legend.lab=legendUnit,legend.cex=1.5,legend.line=3,...)
    if (addContour)
        {
            mylev=seq(zlim[1],zlim[2],by=conlev)
            contour(dat$lon,dat$lat,dat$g, add=TRUE,levels=mylev)
        }

    if (!is.null(TGdat)){
        lat<-c(TGdat$Latitude,NA,NA)
        lon<-c(TGdat$Longitude,NA,NA)
        z<-c(TGdat$TGMDT,zlim[1],zlim[2])
        id<-which(z>= zlim[1] &z<= zlim[2])
        TGlim<-z[id]
        col <- tim.colors(nlevels)  
        colz <- col[cut(TGlim,nlevels)]  
        id<-which(z>= zlim[1] &z<= zlim[2])
        TGlim<-z[id]
        points(lon[id], lat[id], col = 'white' , pch = 3,lwd=5,cex=1.2)
        points(lon[id], lat[id], col = colz , pch = 3,lwd=2)
    }
}

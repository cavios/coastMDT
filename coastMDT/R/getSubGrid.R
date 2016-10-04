##' Extract sub grid 
##' @param grid Input grid[longitude,latitude] of type matrix. 
##' @param lonlim Vector of length two containing the longitude limits of the sub grid.
##' @param latlim Vector of length two containing the latitude limits of the sub grid.
##' @param glonlim Vector of length two containing the longitude limits of the input grid. The default is c(0,360).
##' @param glatlim Vector of length two containing the latitude limits of the input grid. The default is c(-90,90).
##' @param res The resolution of the input grit in decimal degrees.
##' @return List with the elements;  matrix g (sub grid), vector lon (longitudes), vector lat (latitudes) 
##' ##' @details ...
##' @export
##' @examples
##' data(landmask8_anyland)
##' out<-getSubGrid(landmask8_anyland,c(280,300),c(30,60))
##' image(out$lon,out$lat,out$g)
getSubGrid<-function(grid,lonlim,latlim,res=0.125,glonlim=c(0+(res/2),360-(res/2)),glatlim=c(-90+(res/2),90-(res/2))){
    lonlim[1]<-lonlim[1]+(res/2)
    lonlim[2]<-lonlim[2]-(res/2)
    latlim[1]<-latlim[1]+(res/2)
    latlim[2]<-latlim[2]-(res/2)
    lat<-seq(glatlim[1],glatlim[2],by=res)
    lon<-seq(glonlim[1],glonlim[2],by=res)
    ilat1<-which(lat==latlim[1])
    ilat2<-which(lat==latlim[2])
    ilon1<-which(lon==lonlim[1])
    ilon2<-which(lon==lonlim[2])
    nlat<-seq(latlim[1],latlim[2],by=res)
    nlon<-seq(lonlim[1],lonlim[2],by=res)
    g<-grid[ilon1:ilon2,ilat1:ilat2]
    return(list(g=g,lat=nlat,lon=nlon))
}


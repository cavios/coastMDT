##' Extract sub grid 
##' @param grid Input grid[longitude,latitude] of type matrix. 
##' @param lonlim Vector of length two containing the longitude limits of the sub grid. The limits must be given with the smallest longitude first for example c(270,300), except when the 0 longitude id crossed. In this case for example c(355,10). Acceptable values are between 0 and 360.   
##' @param latlim Vector of length two containing the latitude limits of the sub grid.
##' @param glonlim Vector of length two containing the longitude limits of the input grid. The default is c(0,360).
##' @param glatlim Vector of length two containing the latitude limits of the input grid. The default is c(-90,90).
##' @param res The resolution of the input grid in decimal degrees.
##' @return List with the elements;  matrix g (sub grid), vector lon (longitudes), vector lat (latitudes) 
##' ##' @details ...
##' @export
##' @examples
##' \dontrun{data(landmask8)
##' out<-getSubGrid(landmask8,c(280,300),c(30,60))
##' image(out$lon,out$lat,out$g)}

getSubGrid<-function(grid,lonlim,latlim,res=0.125,glonlim=c(0+(res/2),360-(res/2)),glatlim=c(-90+(res/2),90-(res/2))){
    
    lonlim[1]<-lonlim[1]+(res/2)
    lonlim[2]<-lonlim[2]-(res/2)
    latlim[1]<-latlim[1]+(res/2)
    latlim[2]<-latlim[2]-(res/2)
    lat<-seq(glatlim[1],glatlim[2],by=res)
    lon<-seq(glonlim[1],glonlim[2],by=res)
    ilat1<-which(lat==latlim[1])
    ilat2<-which(lat==latlim[2])
    nlat<-seq(latlim[1],latlim[2],by=res)
    #if the grid cross the 0 line
    if (lonlim[1]>lonlim[2]){
        lonlimb<-c(NA,NA)
        lonlima<-c(NA,NA)
        lonlimb[1]<-lonlim[1]
        lonlimb[2]<-360-res/2
        lonlima[1]<-0+res/2
        lonlima[2]<-lonlim[2]
        
        ilon1b<-which(lon==lonlimb[1])
        ilon2b<-which(lon==lonlimb[2])
        ilon1a<-which(lon==lonlima[1])
        ilon2a<-which(lon==lonlima[2])
        nlonb<-seq(lonlimb[1],lonlimb[2],by=res)-360
        nlona<-seq(lonlima[1],lonlima[2],by=res)
        nlon<-c(nlonb,nlona)

        gb<-grid[ilon1b:ilon2b,ilat1:ilat2]
        ga<-grid[ilon1a:ilon2a,ilat1:ilat2]
        g<-rbind(gb,ga)
        
    }
    else{
       
        ilon1<-which(lon==lonlim[1])
        ilon2<-which(lon==lonlim[2])
        nlon<-seq(lonlim[1],lonlim[2],by=res)
        g<-grid[ilon1:ilon2,ilat1:ilat2]
    }

    return(list(g=g,lat=nlat,lon=nlon))
}

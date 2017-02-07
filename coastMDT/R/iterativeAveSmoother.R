##' Iterative box filter
##'
##' The function \code{iterativeAveSmoother} is a simple average filter applied nit number of times. The size of the filter in the E-W direction is scaled according to the latitude.   
##' @param dat An object as returned by the function 'getSubGrid', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). The matrix dat$g[lon,lat] containes the values to be filtered.
##' @param mask An object as returned by the function 'getSubGrid', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). mask$g is a Matrix[lon,lat] representing the land mask, where land=0 and water=1.
##' @param land Matrix[lon,lat] containing land values
##' @param radius Filter radius. Default is radius=0.15/0.83
##' @param nit Number of iterations of the box filter. Default is nit=10
##' @param dlat Grid spacing of the matrix dat. Default is dlat=0.125
##' @importFrom smoothie kernel2dmeitsjer
##' @return  List with the elements;  matrix[lon,lat] g (grid), vector lon (longitudes), vector lat (latitudes).    
##' @details ...
##' @export 
iterativeAveSmoother<-function(dat,mask,land, radius=0.15/0.83,nit=10,res=0.125){
    grid<-t(dat$g)
    mask<-t(mask$g)
    land<-t(land$g)
    idland<-which(mask==0,arr.ind=TRUE)
    grid[idland]<-land[idland]
    sigmau<-radius/res/sqrt(2)
    nr<-nrow(grid)
    nc<-ncol(grid)
    nfiltNS<-floor(sigmau)
    for (i in 1:nit){
       for(i in (nfiltNS+1):(nr-(nfiltNS+1))){
         coslat<-cos(dat$lat[i]*pi/180)
         nfiltEW<-floor(nfiltNS/coslat)+1
         #kmat <- kernel2dmeitsjer( "average", nx=2*nfiltEW+1, ny=2*nfiltNS+1)
         for(j in (nfiltEW+1):(nc-(nfiltEW+1))){
             sub<-grid[(i-nfiltNS):(i+nfiltNS),(j-nfiltEW):(j+nfiltEW)]
             #isna<-any(is.na(sub))
             #if(isna){
             #    idna<-which(is.na(sub))
             #    sub[idna]<-mean(sub, na.rm=TRUE)
             #}
                 
           #grid[i,j]<-sum(t(kmat)*sub,na.rm=TRUE)
             grid[i,j]<-mean(sub,na.rm=TRUE)
         }
       }
       grid[idland]<-land[idland]
   }
   grid[idland]<-NA
   return(list(g=t(grid),lon=dat$lon,lat=dat$lat)) 
}

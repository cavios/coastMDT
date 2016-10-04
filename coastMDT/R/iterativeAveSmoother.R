##' Iterative box filter
##'
##' This function \code{iterativeAveSmoother} ........
##' @param dat Matrix[lon,lat] to be filtered
##' @param mask Matrix[lon,lat] containing land/ocean values. Land=0, water=1.
##' @param land Matrix[lat,lon] containing land values
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param radius Filter radius. Default is radius=0.15/0.83
##' @param nit Number of iterations of the box filter. Default is nit=10
##' @param dlat Grid spacing of the matrix dat. Default is dlat=0.125
##' @importFrom smoothie kernel2dmeitsjer
##' @return Matrix[lat,lon] with filtered MDT values.
##' @details ...
##' @export 
iterativeAveSmoother<-function(dat,mask,land,latlim, radius=0.15/0.83,nit=10,res=0.125){
    dat<-t(dat)
    mask<-t(mask)
    latlim[1]<-latlim[1]+(res/2)
    latlim[2]<-latlim[2]-(res/2)
    lat<-seq(latlim[1],latlim[2],by=res)
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-land[idland]
    sigmau<-radius/res/sqrt(2)
    nr<-nrow(dat)
    nc<-ncol(dat)
    nfiltNS<-floor(sigmau)
    for (i in 1:nit){
       for(i in (nfiltNS+1):(nr-(nfiltNS+1))){
         coslat<-cos(lat[i]*pi/180)
         nfiltEW<-floor(nfiltNS/coslat)+1
         kmat <- kernel2dmeitsjer( "average", nx=2*nfiltEW+1, ny=2*nfiltNS+1)
         for(j in (nfiltEW+1):(nc-(nfiltEW+1))){
             sub<-dat[(i-nfiltNS):(i+nfiltNS),(j-nfiltEW):(j+nfiltEW)]
             isna<-any(is.na(sub))
             if(isna){
                 idna<-which(is.na(sub))
                 sub[idna]<-mean(sub, na.rm=TRUE)
             }
                 
           dat[i,j]<-sum(t(kmat)*sub,na.rm=TRUE)
         }
       }
       dat[idland]<-land[idland]
   }
   dat[idland]<-NA
   out<-dat
   return(out) 
}

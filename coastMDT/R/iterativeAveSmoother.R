##' Iterative box filter
##'
##' This function \code{iterativeAveSmoother} ........
##' @param dat Matrix[lon,lat] to be filtered
##' @param mask Matrix[lon,lat] containing land/ocean values. Land has value 0
##' @param TGmat Matrix[lat,lon] containing tide gauge values
##' @param latlim Vector of length 2, c(lat1,lat2), containing latitude limits of the matrix dat
##' @param radius Filter radius
##' @param nit Number of iterations of the box filter
##' @param dlat Grid spacing of the matrix dat
##' @importFrom smoothie kernel2dmeitsjer
##' @return out[longitude,latitude]
##' ##' @details ...
##' @export 
iterativeAveSmoother<-function(dat,mask,TGmat,latlim, radius=0.15/0.83,nit=10,dlat=0.125){
    dat<-t(dat)
    mask<-t(mask)
    lat<-seq(latlim[1],latlim[2],by=dlat)
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-TGmat[idland]
    sigmau<-radius/dlat/sqrt(2)
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
       dat[idland]<-TGmat[idland]
   }
   dat[idland]<-NA
   out<-dat
   return(out) 
}

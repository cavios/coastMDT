##' Helper function to getLandVal:
##'
##' Extract MDT values at TG positions and estimates a potential bias between the tide gauges and the model based MDT.  
##'
##' @param TG  Data frame or matrix with tide gauge information. TG should contain at least the columns with the names 'Longitude', 'Latitude', and 'TGMDT'. 'TGMDT' should contain MDT values at the tide gauge positions.   
##' @param dat Matrix[lon,lat] with MDT values
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param boxlon Integer. The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @param boxlat Integer. The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast.  
##' @return list(TGland=out,bias=bias). TGland is data frame with 4 columns; row id, col id, corrected tide gauge value, sd of boxmean value of modeled MDT
##' @details ...
##' @export
getTGVal<-function(TG,dat,mask,lonlim,latlim,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    myland<-matrix(NA,nrow(dat),ncol(dat))


    TGid<-getTGid(TG,lonlim,latlim)
    boxmean<-getBoxMean(dat,TGid)
   
    mydif<-TG$TGMDT-boxmean$mean
    bias<-median(mydif,na.rm=TRUE) #this could be improved
    # in TGcompareWith we have
    if(bias > 0) new<-TG$TGMDT-bias
    else new<-TG$TGMDT+abs(bias)
    #if(bias > 0) new<-TG$TGMDT+bias
    #else new<-TG$TGMDT-bias

    out<-cbind(TGid,TG=new,sd=boxmean$sd)
    return(list(TGland=out,bias=bias))
}

##' Helper function to getLandVal: Identify Coast lines with tide gauges 
##'
##' @param polyCoast Matrix[lon,lat], the out put of the function polygonizeCoast. The matrix contains the coastlines of the region defined by lonlim and latlim, where the integer values represents the coast id. 
##' @param TG Data frame or matrix with tide gauge information. TG should contain at least the columns with the names 'Longitude', 'Latitude', and 'TGMDT'. 'TGMDT' should contain MDT values at the tide gauge positions.  
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees
##' @return Vector with coast id for the tide gauges. 
##' @details ...
##' @export
getTGCoast<-function(polyCoast,TG,lonlim,latlim){
    NTG<-nrow(TG)
    landVal<-matrix(NA, nrow(polyCoast),ncol(polyCoast))
    TGcoast<-rep(NA,NTG)
    idPC<-which(!is.na(polyCoast),arr.ind=TRUE)
    NidPC<-nrow(idPC)
    TGid<-getTGid(TG,lonlim,latlim)
    idlon<-TGid[,1]
    idlat<-TGid[,2]
    
    for (i in 1:NTG){

        mydist<-sqrt((idlon[i]-idPC[,1])^2+(idlat[i]-idPC[,2])^2)
        idmin<-which.min(mydist)
        rowmin<-idPC[idmin,]
        TGcoast[i]<-polyCoast[rowmin[1],rowmin[2]]       
    }
    
    return(TGcoast)
}

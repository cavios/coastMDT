##' Helper funtion to getLandVal: Finds land values based on tide gauges and altimetry 
##'
##' @param polyCoast Matrix[lon,lat], the out put of the function polygonizeCoast. The matrix contains the coastlines of the region defined by lonlim and latlim, where the integer values represents the coast id. 
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauge.
##' @param TGcorr Vector with bias corrected tide gauge values. Obtained from the helper function getTGVal. 
##' @param dat Matrix[lon,lat] with MDT values
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param boxlon Integer. The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast.
##' @param boxlat Integer. The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @return Matrix[lon,lat] with MDT land values at the coast line, defined by polyCoast 
##' @details ...
##' @export
getLandComb<-function(polyCoast,TG,TGcorr,dat,lonlim,latlim,boxlon=4,boxlat=4){
    landVal<-matrix(NA, nrow(polyCoast),ncol(polyCoast))
    myTGCoast<-getTGCoast(polyCoast,TG,lonlim,latlim)
    myTGCoast<-unique(myTGCoast)
    NoPoly<-length(myTGCoast)
    TGid<-getTGid(TG,lonlim,latlim)
    idlon<-TGid[,1]
    idlat<-TGid[,2]
    landVal[TGid]<-TGcorr 
    idPC<-which(!is.na(polyCoast),arr.ind=TRUE)
    valPC<-polyCoast[idPC]
    for (i in 1:NoPoly){
        id<-which(valPC!=myTGCoast[i])
        idPC<-idPC[id,]
        valPC<-valPC[id]
    }
    boxmean<-getBoxMean(dat,idPC)
    landVal[idPC]<-boxmean$mean
    return(landVal)
}

##' Helper function: Identify Coast lines with tide gauges 
##'
##' @param polyCoast ...
##' @param TG ...
##' @param lonlim ...
##' @param latlim ...
##' @return list with mean and sd values
##' ##' @details ...
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

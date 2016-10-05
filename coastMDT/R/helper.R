##' Helper function: Find mean value in a box 
##'
##' @param dat Matrix[lon,lat] with MDT values
##' @param id Matrix[N,2] with row and column id, reprecenting the center of the box
##' @param boxlon Integer. The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @param boxlat Integer. The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @return list with mean and sd values
##' @details ...
##' @export
getBoxMean<-function(dat,id,boxlon=4,boxlat=4){
    NR<-nrow(id)
    mymean<-rep(NA,NR)
    mysd<-rep(NA,NR)
    idlon<-id[,1]
    idlat<-id[,2]
    for (i in 1:NR){
        if(idlon[i]>boxlon & idlon[i]<dim(dat)[1]-boxlon & idlat[i]>=boxlat&idlat[i]<dim(dat)[2]-boxlat){
            sub<-dat[(idlon[i]-boxlon):(idlon[i]+boxlon),(idlat[i]-boxlat):(idlat[i]+boxlat)]
                                       
            mymean[i]<-mean(sub,na.rm=TRUE)
            mysd[i]<-sd(sub,na.rm=TRUE)
        }
    }
    return(list(mean=mymean,sd=mysd))
}

##' Helper function: Find index of coast line with Tide gauges 
##'
##' @param polyCoast Matrix[lon,lat], the out put of the function polygonizeCoast. The matrix contains the coastlines of the region defined by lonlim and latlim, where the integer values represents the coast id. 
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauge.
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @return coastid for each TG
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

##' Find row and col id of Tide gauges 
##'
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauge.
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @return Matrix[N,2] with row and coulmn id for the tide gauges
##' @export
getTGid<-function(TG,lonlim,latlim){
    if(lonlim[1]>lonlim[2]) lonlim[1]<-lonlim[1]-360   
    idlon<-floor((TG[,3]-lonlim[1])/0.125)+1
    idlat<-floor((TG[,2]-latlim[1])/0.125)+1
    TGid<-as.matrix(cbind(idlon,idlat))
    return(TGid)
}

##' Find subset of Tide gauges 
##'
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauge.
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @return subset of TG
##' @export
getSubTG<-function(TG,lonlim,latlim){
     if (lonlim[1]>lonlim[2]){
         lonlim[1]<-lonlim[1]-360
         id<-which(TG[,3]>180)
         TG[id,3]<-TG[id,3]-360
         TGsub<-TG[TG[,3]> lonlim[1]&TG[,3]< lonlim[2]&TG[,2]> latlim[1]& TG[,2]< latlim[2],]
     }else{
     
         TGsub<-TG[TG[,3]> lonlim[1]&TG[,3]< lonlim[2]&TG[,2]> latlim[1]& TG[,2]< latlim[2],]
     }
    return(TGsub)
}

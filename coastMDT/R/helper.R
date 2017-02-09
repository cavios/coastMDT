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

##' Helper function: Finds row and col id of Tide gauges 
##'
##' @param TG Data frame or matrix with tide gauge information. TG should contain at least the columns with the names 'Longitude', 'Latitude', and 'TGMDT'. 'TGMDT' should contain MDT values at the tide gauge positions.   
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @return Matrix[N,2] with row and coulmn id for the tide gauges
##' @export
getTGid<-function(TG,lonlim,latlim){
    if(lonlim[1]>lonlim[2]) lonlim[1]<-lonlim[1]-360   
    idlon<-floor((TG$Longitude-lonlim[1])/0.125)+1
    idlat<-floor((TG$Latitude-latlim[1])/0.125)+1
    TGid<-as.matrix(cbind(idlon,idlat))
    return(TGid)
}

##' Find subset of tide gauges 
##'
##' @param TG Data frame or matrix with tide gauge information. TG should contain at least the columns with the names 'Longitude' and 'Latitude'.
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @return subset of TG
##' @export
getSubTG<-function(TG,lonlim,latlim){
     if (lonlim[1]>lonlim[2]){
         lonlim[1]<-lonlim[1]-360
         id<-which(TG$Longitude>180)
         TG$Longitude[id]<-TG$Longitude[id]-360
         TGsub<-TG[TG$Longitude> lonlim[1]&TG$Longitude< lonlim[2]&TG$Latitude> latlim[1]& TG$Latitude< latlim[2],]
     }else{
     
         TGsub<-TG[TG$Longitude> lonlim[1]&TG$Longitude< lonlim[2]&TG$Latitude> latlim[1]& TG$Latitude< latlim[2],]
     }
    return(TGsub)
}

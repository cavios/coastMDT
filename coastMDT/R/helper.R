##' Find mean value in a box 
##'
##' @param dat ...
##' @param id ...
##' @param boxlon ...
##' @param boxlat ...
##' @return list with mean and sd values
##' ##' @details ...
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

##' Find index of coast line with Tide gauges 
##'
##' @param polyCoast ...
##' @param TG ...
##' @param lonlim ...
##' @param latlim ...
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
##' @param TG ...
##' @param lonlim ...
##' @param latlim ...
##' @return TGid
##' @export
getTGid<-function(TG,lonlim,latlim){
    idlon<-floor((TG[,3]-lonlim[1])/0.125)+1
    idlat<-floor((TG[,2]-latlim[1])/0.125)+1
    TGid<-as.matrix(cbind(idlon,idlat))
    return(TGid)
}

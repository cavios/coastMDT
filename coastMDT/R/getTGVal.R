##' Extract MDT values at TG positions 
##'
##' @param TG ...
##' @param dat ...
##' @param lonlim ...
##' @param latlim ...
##' @param boxlon
##' @param boxlat 
##' @return list with mean and sd values
##' ##' @details ...
##' @export
getTGVal<-function(TG,dat,mask,lonlim,latlim,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    myland<-matrix(NA,nrow(dat),ncol(dat))


    TGid<-getTGid(TG,lonlim,latlim)
    boxmean<-getBoxMean(dat,TGid)
   
    mydif<-TG[,4]-boxmean$mean
    bias<-median(mydif,na.rm=TRUE) #this could be improved
    if(bias > 0) new<-TG[,4]+bias
    else new<-TG[,4]-bias

    out<-cbind(TGid,TG=new,sd=boxmean$sd)
    return(list(TGland=out,bias=bias))
    
    #landVal<-new
    #data<-cbind(x=idlon,y=idlat)
    #query <- cbind(x=idland[,1], y=idland[,2])
    #nns <- get.knnx(data, query, k=1)
    #myland<-matrix(NA,nrow(mask),ncol(mask))
    #myland[idland]<-landVal[nns$nn.index]
    #return(list(mean=boxmean$mean,sd=boxmean$sd,id=TGid,TG=new,grid=myland,bias=bias))      
}

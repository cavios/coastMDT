##' Estimate land values
##'
##' @param mycoast ...
##' @param mask ...
##' @param dat ...
##' @param delta ...
##' @param boxlon
##' @param boxlat
##' @param scale
##' @importFrom FNN get.knnx 
##' @return Matrix myland[longitude,latitude]
##' ##' @details ...
##' @export
getLandInfo<-function(mycoast,mask,dat,delta=0,boxlon=4,boxlat=4,scale=FALSE){
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-NA
    NR<-nrow(mycoast$id)
    idlon<-mycoast$id[,1]
    idlat<-mycoast$id[,2]
    mymean<-rep(NA,NR)
    landsd<-rep(NA,NR)
    
    for (i in 1:NR){
        if(idlon[i]>boxlon & idlon[i]<dim(dat)[1]-boxlon & idlat[i]>=boxlat&idlat[i]<dim(dat)[2]-boxlat)
        {
            sub<-dat[(idlon[i]-boxlon):(idlon[i]+boxlon),(idlat[i]-boxlat):(idlat[i]+boxlat)]
            #remove land values
            mymean[i]<-mean(sub,na.rm=TRUE)
            landsd[i]<-sd(sub,na.rm=TRUE)
            
            #landVal[i]<-mymean+delta
        }
    }
    #scaleFactor<-ifelse(scale,landsd/max(landsd,na.rm=TRUE),rep(1,NR))
    scaleFactor<-landsd/max(landsd,na.rm=TRUE)
    out<-as.matrix(cbind(idlon,idlat,mymean,landsd,scaleFactor))
    out<-na.omit(out)
    
    if(scale)landVal<-out[,3]+delta*out[,5]
    else landVal<-out[,3]+delta
    data<-cbind(x=out[,1],y=out[,2])
    query <- cbind(x=idland[,1], y=idland[,2])
    nns <- get.knnx(data, query, k=1)
    myland<-matrix(NA,nrow(mask),ncol(mask))
    sdland<-matrix(NA,nrow(mask),ncol(mask))
    sdland[mycoast$id]<-scaleFactor
    myland[idland]<-landVal[nns$nn.index]
    return(list(landmean=myland,landsd=sdland,myscale=scaleFactor))
}

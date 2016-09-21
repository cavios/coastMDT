##' Estimate land values
##'
##' @param mycoast ...
##' @param mask ...
##' @param dat ...
##' @param delta ...
##' @param boxlon
##' @param boxlat
##' @importFrom FNN get.knnx 
##' @return Matrix myland[longitude,latitude]
##' ##' @details ...
##' @export
getLandInfo<-function(mycoast,mask,dat,delta=0,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-NA
    NR<-nrow(mycoast$id)
    idlon<-mycoast$id[,1]
    idlat<-mycoast$id[,2]
    landVal<-rep(NA,NR)
    
    for (i in 1:NR){
        if(idlon[i]>boxlon & idlon[i]<dim(dat)[1]-boxlon & idlat[i]>=boxlat&idlat[i]<dim(dat)[2]-boxlat)
        {
            sub<-dat[(idlon[i]-boxlon):(idlon[i]+boxlon),(idlat[i]-boxlat):(idlat[i]+boxlat)]
            #remove land values
            mymean<-mean(sub,na.rm=TRUE)
            landVal[i]<-mymean+delta
        }
    }
    out<-as.matrix(cbind(idlon,idlat,landVal))
    out<-na.omit(out)
    data<-cbind(x=out[,1],y=out[,2])
    query <- cbind(x=idland[,1], y=idland[,2])
    nns <- get.knnx(data, query, k=1)
    myland<-matrix(NA,nrow(mask),ncol(mask))
    myland[idland]<-out[nns$nn.index,3]
    return(myland)
}

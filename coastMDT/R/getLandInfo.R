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
getLandInfo<-function(mycoast,mask,dat,delta=0,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-NA
    boxmean<-getBoxMean(dat,mycoast$id)
    out<-as.matrix(cbind(mycoast$id,mean=boxmean$mean+delta,sd=boxmean$sd))
    out<-na.omit(out)
    return(out)
    
    #landVal<-out[,3]+delta
    #data<-cbind(x=out[,1],y=out[,2])
    #query <- cbind(x=idland[,1], y=idland[,2])
    #nns <- get.knnx(data, query, k=1)
    #myland<-matrix(NA,nrow(mask),ncol(mask))
    #sdland<-matrix(NA,nrow(mask),ncol(mask))
    #sdland[mycoast$id]<-boxmean$sd
    #myland[idland]<-landVal[nns$nn.index]
    #return(list(mean=myland,sd=sdland))
}

##' Helper function to getLandVal: Estimate land MDT values at the coastline based on altimetry
##'
##' @param mycoast Matrix[N,2] with row and column values of the coast line. mycoast is the out put of helper function getCoastLine 
##' @param mask Matrix[lon,lat] representing the land mask, where land=0 and water=1
##' @param dat Matrix[lon,lat] with MDT values 
##' @param boxlon Integer. The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @param boxlat Integer. The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast. 
##' @importFrom FNN get.knnx 
##' @return Matrix[N,4]; row id, col id, mean MDT value, sd of MDT 
##' @details ...
##' @export
getLandInfo<-function(mycoast,mask,dat,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    dat[idland]<-NA
    boxmean<-getBoxMean(dat,mycoast$id)
    out<-as.matrix(cbind(mycoast$id,mean=boxmean$mean+delta,sd=boxmean$sd))
    out<-na.omit(out)
    return(out)
}

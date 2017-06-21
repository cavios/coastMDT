##' Estimate a MDT error field via bootstrap 
##'
##' With the function getError it is possible to estimate an MDT error field via the bootstrap approach. 
##' @param dat An object as returned by the function 'getSubGrid' or 'iterativeAveSmoother', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). 
##' @param dat An object as returned by the function 'getSubGrid', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). The matrix dat$g[lon,lat] containes the values to be filtered.
##' @param land Matrix[lon,lat] containing land values
##' @param mask An object as returned by the function 'getSubGrid', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). mask$g is a Matrix[lon,lat] representing the land mask, where land=0 and water=1.
##' @param bootNr Number of bootstap data sets
##' @param nnx number of grid cells in the east-west direction 
##' @param nny number of grid cells in the north-south direction 
##' @param ncores Number of available cores 
##' @importFrom parallel parLapply detectCores makeCluster clusterEvalQ clusterExport stopCluster
##' @details The data is sampled in blocks, which size is specified by the user in the arguments nnx and nny. nnx and nny should be chosen carefully. If nnx and nny are too small the data will not be independent and the error will be underestimated. If too large, there will be data gabs and the errors will not be representative.  
##' @export
##'
getError<-function(dat,land,mask,bootNr=100,nnx=3,nny=3,ncores=detectCores()){
    cat("Please, be patient. This function take some time to run\n")
    nr<-nrow(dat$g)
    nc<-ncol(dat$g)
    IMat<-getIdMat(mask,nnx,nny,nr,nc)
    myCount<-lapply(1:bootNr,function(i) getCount(IMat,nr,nc))
    cl <- makeCluster(ncores) #set up nodes
    clusterEvalQ(cl, {library(coastMDT)}) #load the package to each node
    clusterExport(cl=cl, varlist=c("myCount","raw","mask","land"),envir=environment())
    bootDat <- parLapply(cl, 1:bootNr, function(i) iterativeAveSmootherBoot(dat,mask,land,countMat=myCount[[i]]))
    stopCluster(cl) #shut it down
    ##calculate sd
    X<-lapply(bootDat, function (x) x$g)
    Y <- do.call(cbind,X)
    Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
    out<-apply(Y, c(1, 2), sd, na.rm = TRUE)
    bootSD<-list(lat=dat$lat,lon=dat$lon,g=out)
    return(bootSD)
}

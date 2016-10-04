##' Extract MDT values at TG positions 
##'
##' @param dat ...
##' @param TG ...
##' @param mask
##' @param lonlim ...
##' @param latlim ...
##' @param type ...
##' @param intMethod ...
##' @param boxlon ...
##' @param boxlat ...
##' @param delta ...
##' @importFrom akima interp
##' @importFrom FNN get.knnx
##' @return matrix with land values
##' ##' @details ...
##' @export
getLandVal<-function(dat,TG,mask,lonlim,latlim,type="alt",intMethod="lin",boxlon=4,boxlat=4,delta=0){
    if (!type %in% c("alt", "tg","both")) stop(paste("unknown type:", type))
    if (!intMethod %in% c("lin", "nn")) stop(paste("unknown intMethod:", intMethod))
    landMat<-matrix(NA,nrow(mask),ncol(mask))
    idland<-which(mask==0,arr.ind=TRUE)
    
    if(type=="alt"){
        mycoast<-getCoastLine(mask)
        myland<-getLandInfo(mycoast,mask,dat)
    }
    else if(type=="tg"){
        out<-getTGVal(TG,dat,mask,lonlim,latlim)
        myland<-out$TGland
    }
    else{
        #this needs to be improoved at some point
        myPolyCoast<-polygonizeCoast(mask)
        TGcoast<-getTGCoast(myPolyCoast,TG,lonlim,latlim)
        TGval<-getTGVal(TG,dat,mask,lonlim,latlim)
        MDTland<-getLandComb(myPolyCoast,TG,TGval$TGland[,3],dat,lonlim,latlim)
        idland2<-which(!is.na(MDTland),arr.ind=TRUE)
        landval<-MDTland[idland2]
        myland<-cbind(idland2[,1],idland2[,2],landval)
}

    #interpolation
    idlon<-myland[,1]
    idlat<-myland[,2]
    landVal<-myland[,3]

    if(intMethod=="lin"){
        xo<-1:nrow(dat)
        yo<-1:ncol(dat)
        out<-interp(idlon,y=idlat,z=landVal,xo=xo,yo=yo,duplicate="mean")
        landMat[idland]<-out$z[idland]
        
    }
    else{
        data<-cbind(x=idlon,y=idlat)
        query <- cbind(x=idland[,1], y=idland[,2])
        nns <- get.knnx(data, query, k=1)
        landMat[idland]<-landVal[nns$nn.index]
    }
    
    return(landMat)
}

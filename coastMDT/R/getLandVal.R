##' Estimates MDT land values based based on altimetry, tide gauges of both
##'
##' @param dat Matrix[lon,lat] with MDT values
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauge 
##' @param mask Matrix[lon,lat] representing the land mask, where land=0 and water=1
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees
##' @param type Character string representing the data to be used when the land values are estimated. type="tg": tide gauge data is used to estimate the MDT land values, type="alt": altimetry data is used, and type="both": both altimetry and tide gauge data is used.
##' @param intMethod Character string describing the interpolation method used. "lin": linear interpolation and "nn": nearest neighbor interpolation 
##' @param boxlon Integer. The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast.     
##' @param boxlat Integer. The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast.      
##' @importFrom akima interp
##' @importFrom FNN get.knnx
##' @return Matrix[lon,lat] with land values
##' @details ...
##' @export
##' @examples
##' #load data
##' data(landmask8) #land mask
##' data(difmss15eig6c4r)
##' data(MDT_EGM08_2003_2007)
##' TG<-MDT_EGM08_2003_2007
##' #region of interest
##' lonlim<-c(275,300)
##' latlim<-c(20,55)
##' #sub grids
##' mask<-getSubGrid(landmask8,lonlim,latlim)
##' rawUS<-getSubGrid(difmss15eig6c4r,lonlim,latlim)
##' idraw<-which(mask$g==0,arr.ind=TRUE)
##' rawUS$g[idraw]<-NA
##' TGsub<-getSubTG(TG,lonlim,latlim)
##' #Estimation of land data
##' mylandTG<-getLandVal(rawUS$g,TGsub,mask$g,lonlim,latlim,type="tg",intMethod="lin")
##' 
getLandVal<-function(dat,TG,mask,lonlim,latlim,type="alt",intMethod="lin",boxlon=4,boxlat=4){
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

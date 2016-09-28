##' Polygonize land mask
##' 
##' @param mask Matrix[lon,lat] containing land/ocean values. Land has value 0
##' @param landVal ...
##' @importFrom raster raster rasterToPolygons
##' @importFrom sp disaggregate
##' @return out[longitude,latitude]
##' ##' @details ...
##' @export 
polygonizeCoast<-function(mask,landVal=0){
    polyCoast<-matrix(NA,nrow=nrow(mask),ncol=ncol(mask))    
    myraster<-raster(mask, ymn=1, ymx=nrow(mask), xmn=1, xmx=ncol(mask))
    test<-rasterToPolygons(myraster, dissolve = T,fun=function(x){x == landVal})
    shape2 <- disaggregate(test)
    shape2$id <- factor(seq_len(length(shape2)))
    NoPoly<-length(shape2$id)
    image(1:nrow(mask),1:ncol(mask),mask)

    for (i in 1:NoPoly){
        x<-floor(nrow(mask)+1-shape2@polygons[[i]]@Polygons[[1]]@coords[,2])
        y<-floor(shape2@polygons[[i]]@Polygons[[1]]@coords[,1])
        idpol<-as.matrix(cbind(x,y))
        polyCoast[idpol]<-i
        lines(nrow(mask)+1-shape2@polygons[[i]]@Polygons[[1]]@coords[,2],shape2@polygons[[i]]@Polygons[[1]]@coords[,1],t='l')
    }
    return(polyCoast)
}

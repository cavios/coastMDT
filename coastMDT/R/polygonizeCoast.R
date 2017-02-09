##' Helper function to getLandVal: Turns land mask matrix into polygons
##'
##' @param mask Matrix[lon,lat] with land mask. Land=0 (default) and water=1.
##' @param landVal integeer representing the land value in the mask
##' @importFrom raster raster rasterToPolygons
##' @return Matrix[lon,lat] whith coast line ids
##' ##' @details ...
##' @export 
polygonizeCoast<-function(mask,landVal=0){
    require(sp)
    polyCoast<-matrix(NA,nrow=nrow(mask),ncol=ncol(mask))    
    myraster<-raster(mask, ymn=1, ymx=nrow(mask), xmn=1, xmx=ncol(mask))
    test<-rasterToPolygons(myraster, dissolve = T,fun=function(x){x == landVal})
    shape2 <- disaggregate(test)
    shape2$id <- factor(seq_len(length(shape2)))
    NoPoly<-length(shape2$id)
    #image(1:nrow(mask),1:ncol(mask),mask)

    for (i in 1:NoPoly){
        x<-floor(nrow(mask)+1-shape2@polygons[[i]]@Polygons[[1]]@coords[,2])
        y<-floor(shape2@polygons[[i]]@Polygons[[1]]@coords[,1])
        idpol<-as.matrix(cbind(x,y))
        polyCoast[idpol]<-i
        #lines(nrow(mask)+1-shape2@polygons[[i]]@Polygons[[1]]@coords[,2],shape2@polygons[[i]]@Polygons[[1]]@coords[,1],t='l')
    }
    return(polyCoast)
}

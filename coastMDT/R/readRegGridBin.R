##' Function that reads a regular global binary grid. 
##' @param filename A character string with the file name.
##' @param nx Number of rows in the grid (longitude). Deafault is nx=2880.
##' @param ny Number of columns in the grid (latitude). Deafault is ny=1440.
##' @param res The grid spacing in degrees. Default is res=0.125.  
##' @param ... Additional arguments to readBin.
##' @return An object of the type coastMDT;  a list containing a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes).
##' @details The default is grid is a 1/8 degree grid which is 2880 longitudes by 1440 latitudes: longitudes (0.5,1.5,2.5 .... 2879.5)/8 degrees, latitudes  -90 + (0.5,1.5,2.5 .... 1439.5)/8 degrees. The type is real*4, hence the length of the file i 4*nx*ny. The unit is meter.
##' @export
readRegGridBin<-function(filename,nx=2880,ny=1440,res=0.125,...){
    oceandata <- file(filename, "rb")
    ocean<-readBin(oceandata, double(), size = 4, n =nx*ny , endian = "little",...)
    ocean<-matrix(ocean,nrow=nx,ncol=ny)
    lat<-seq(-90+res/2,90-res/2,by=res)
    lon<-seq(0+res/2,360-res/2,by=res)
    ocean<-list(g=ocean,lat=lat,lon=lon)
    close(oceandata)

    return(ocean)
}




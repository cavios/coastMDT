##' Extract sub grid 
##' @param mask Matrix[lon,lat] with land mask values. 
##' @param land Integer value for land. Default is 0.
##' @param water Integer value for land. Default is 0.
##' @return List with the elements;  matrix g[lon,lat], which contains the location of the land value which has a water neighbor. Matrix id which contains two columns; row no and col no of the matrix g where a land value is identified as coast.  
##' @details ...
##' @export
getCoastLine<-function(mask, land=0, water=1){
    mycoast<-matrix(NA,nrow(mask), ncol(mask))
    for (i in 2:(nrow(mask)-1)){
        for (j in 2:(ncol(mask)-1)){
            if ((mask[i,j]==land) & ((mask[i,j-1]==water)|(mask[i,j+1]==water)|(mask[(i-1),j]==water)|(mask[i+1,j]==water))){
                mycoast[i,j]<-2                
            }
        }
    }
    idcoast<-which(mycoast==2,arr.ind=TRUE)
    return(list(g=mycoast,id=idcoast))
}

##' Helper function for getError
##'
##' The function getIdMat divides a matrix into sub matrices specified by the arguments  nnx and nny .    
##' @param mask An object as returned by the function 'getSubGrid', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). mask$g is a Matrix[lon,lat] representing the land mask, where land=0 and water=1.
##' @param nnx number of grid cells in the east-west direction 
##' @param nny number of grid cells in the north-south direction 
##' @param nr number of rows in the data matrix
##' @param nc number of columns in the data matrix
##' @return  List with the elements;  matrix[lon,lat] Mat (ids of the submatrices), vector mysamples (the values of the ids), and  nrSam (the length of mysamples).    
##' @details  
getIdMat<-function(mask,nnx,nny,nr,nc){
        #dim of index matrix
        inr<-ceiling(nr/nny)
        inc<-ceiling(nc/nnx)

        idm<-lapply(1:inc, function(i)(i-1)*inr+matrix((rep(sort(rep(1:inr,nnx)),nny)),ncol=inr*nny,byrow=TRUE))
        out<-do.call(rbind,idm)
        out<-t(out)
        idmat<-out[1:nr,1:nc]
        #remove data over land
        idland<-which(mask$g==0,arr.ind=TRUE)
        idmat[idland]<-NA
        #find unique number
        mysamples<-unique(as.vector(idmat))
        mysamples <- mysamples[!is.na(mysamples)]
        nrSam<-length(mysamples)
        return(list(Mat=idmat,mysamples=mysamples,nrSam=nrSam))
}

##' Helper function for the bootstrap function getError
##'
##' This function returns a matrix with the number of times each element must be counted.   
##' @param IMat a matrix that divides the area into sub areas. It is the output of the function getIdMat.
##' @param nr the number of rows in the data matrix (the raw MDT).
##' @param nc the number of columns in the data matrix (the raw MDT)
##' @return  matrix[lon,lat] where each element represents the number of times each elements should be counted in the iterative filter, when the sd is estimated for the filtered MDT, by the use of bootstrap.    
##' @details ...
getCount<-function(IMat,nr,nc){
        bootOne<-sample(IMat$mysamples,IMat$nrSam,replace=TRUE)
        countMat<-matrix(0,nr,nc)
        idBoot<-unique(sort(bootOne))
        tabBoot<-table(bootOne)
        for (i in 1:length(idBoot)){
            myid<-which(IMat$Mat==idBoot[i],arr.ind=TRUE)
            countMat[myid]<-tabBoot[i]
        }
        countMat[idraw]<-NA
        return(countMat)
}

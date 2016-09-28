##' Extract MDT values at TG positions 
##'
##' @param TG ...
##' @param dat ...
##' @param lonlim ...
##' @param latlim ...
##' @param boxlon
##' @param boxlat 
##' @return list with mean and sd values
##' ##' @details ...
##' @export
getTGVal<-function(TG,dat,mask,lonlim,latlim,boxlon=4,boxlat=4){
    idland<-which(mask==0,arr.ind=TRUE)
    myland<-matrix(NA,nrow(dat),ncol(dat))
    
    idlon<-floor((TG[,3]-lonlim[1])/0.125)+1
    idlat<-floor((TG[,2]-latlim[1])/0.125)+1
    TGid<-as.matrix(cbind(idlon,idlat))
    NR<-nrow(TGid)
     mymean<-rep(NA,NR)
    mysd<-rep(NA,NR)
    for (i in 1:NR){
        if(idlon[i]>boxlon & idlon[i]<dim(dat)[1]-boxlon & idlat[i]>=boxlat&idlat[i]<dim(dat)[2]-boxlat)
            {
                sub<-dat[(idlon[i]-boxlon):(idlon[i]+boxlon),(idlat[i]-boxlat):(idlat[i]+boxlat)]
                                       
                mymean[i]<-mean(sub,na.rm=TRUE)
                mysd[i]<-sd(sub,na.rm=TRUE)           
        }
    }
    mydif<-TG[,4]-mymean
    bias<-median(mydif,na.rm=TRUE) #this could be improved
    if(bias > 0) new<-TG[,4]+bias
    else new<-TG[,4]-bias
   
    landVal<-new
    data<-cbind(x=idlon,y=idlat)
    query <- cbind(x=idland[,1], y=idland[,2])
    nns <- get.knnx(data, query, k=1)
    myland<-matrix(NA,nrow(mask),ncol(mask))
    myland[idland]<-landVal[nns$nn.index]

    
    return(list(mean=mymean,sd=mysd,id=TGid,TG=new,grid=myland))      
}

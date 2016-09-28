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
compareWithTG<-function(TG,dat,lonlim,latlim,boxlon=3,boxlat=3){
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
                
                NoGod<-(boxlon*2+1)*(boxlat*2+1)-length(which(is.na(sub)))                       
                mymean[i]<-mean(sub,na.rm=TRUE)
                mysd[i]<-sd(sub,na.rm=TRUE)/sqrt(NoGod)           
        }
    }
    mydif<-TG[,4]-mymean
    bias<-median(mydif,na.rm=TRUE) #this could be improved
    if(bias > 0) new<-TG[,4]+bias
    else new<-TG[,4]-bias
    newdif<-mymean-new
    idna<-which(is.na(newdif))
    RMS<-sqrt(sum(newdif[-idna]^2)/length(newdif[-idna]))

    comb<-cbind(TG[,1:3],new,mymean,mysd)
    low<-newdif-2*comb[,6]
    high<-newdif+2*comb[,6]
    myrange<-range(high,low,na.rm=TRUE)
    myrange[1]<-myrange[1]-0.1*(myrange[2]-myrange[1])
    par(mfrow=c(1,2))
    plot(comb[,2],newdif,xlab='Latitude',ylab='MDT_Alt-MDT_TG',ylim=myrange)
    arrows(comb[,2], y0=low, y1=high, length=0.05, angle=90, code=3,col='black',lwd=2)
    abline(h=0,lty=2,lwd=2,col=gray(0.5))
    legend('bottomleft',legend=paste('RMS = ',signif(RMS,3),' m',sep=''),bty='n')
    #plot(comb[,c(2,4)],pch=4,lwd=2,xlab="Latitude",ylab='MDT')
#points(comb[,2],comb[,5],col='red',lwd=2)> arrows(comb[,2], y0=comb[,5]-2*comb[,6], y1=comb[,5]+2*comb[,6], length=0.05, angle=90, code=3,col='red',lwd=2)
    hist(newdif,n=20, col='lightblue',xlab='Difference [m]')
    par(mfrow=c(1,1))
    return(list(mean=mymean,sd=mysd,bias=bias,diff=newdif,RMS=RMS))      
}

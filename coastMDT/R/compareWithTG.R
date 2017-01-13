##' Extract MDT values at TG positions 
##'
##' The function extracts MDT values at the position of the tide gauges, and compares the MDT values of the field with MDT values based on the tide gauges.  
##' @param TG Data frame or matrix with tide gauge information. The dimension of TG is N x 4, where N is the number of tide gauges and the 4 columns are; PSMSL station id, latitude, longitude, MDT value at the tide gauges. 
##' @param dat Matrix[lon,lat] with MDT values
##' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
##' @param boxlon The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast.
##' @param boxlat The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast.
##' @param export If true a file named "TGcompare.dat" containing the following columns; TG station No., TG_MDT,Alt_mean MDT,MDT_Alt_sd,bias corrected difference (alt-TG_bias_corr) is produced. 
##' @return A list that includes:
##' mean: The mean values of the field in the box, defined by boxlon and boxlat at each tide gauge position.
##' sd: The standard deviation of the field in the box, defined by boxlon and boxlat at each tide gauge position.
##' bias: The bias between the mean field values and the tide gauge MDT values.
##' diff: The difference between the mean field values and the bias corrected tide gauge MDT values
##' RMS: The RMS of the mean field values and the tide gauge MDT values
##' @details Besides the list, that is returned. A plot of the difference between the altimetry and the tide gauges MDT values is automatically generated  
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
##' #Compare with tide gauges
##' res<-compareWithTG(TGsub,rawUS$g,lonlim,latlim)
compareWithTG<-function(TG,dat,lonlim,latlim,boxlon=3,boxlat=3,export=FALSE){
    TGid<-getTGid(TG,lonlim,latlim)
    idlon<-TGid[,1]
    idlat<-TGid[,2]
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
    if(bias > 0) new<-TG[,4]-bias
    else new<-TG[,4]+abs(bias)
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
    if(export){
        data=data.frame(stationNo=TG[,1],TG_MDT=TG[,4],Alt_mean=mymean,Alt_sd=mysd,biasCorrDif=newdif)
        write.table(data,file="TGcompare.dat",row.names=FALSE, quote=FALSE)
    }
    return(list(mean=mymean,sd=mysd,bias=bias,diff=newdif,RMS=RMS))      
}

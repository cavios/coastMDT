#' Extract MDT values at TG positions 
#'
#' The function extracts MDT values at the position of the tide gauges, and compares the MDT values of the field with MDT values based on the tide gauges.  
#' @param TG Data frame or matrix with tide gauge information. TG should contain at least the columns with the names 'Longitude', 'Latitude', and 'TGMDT'. 'TGMDT' should contain MDT values at the tide gauge positions. 
#' @param dat An object as returned by the function 'getSubGrid' or 'iterativeAveSmoother', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes). 
#' @param lonlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
#' @param latlim Vector of length 2 with the longitude data grid limits, c(lonlim[1],lonlim[2]). The limits must be given in whole degrees.
#' @param boxlon The number ((2 x boxlon) +1) of grid cells in the longitude direction, that is used to estimate the altimetry based MDT value at the coast.
#' @param boxlat The number ((2 x boxlat) +1) of grid cells in the latitude direction, that is used to estimate the altimetry based MDT value at the coast.
#' @param export If true the information estimated in the function is saved in a csv file. The default name is "TGcompare.csv". The file contains; All columns in the data frame TG,Alt_mean MDT,MDT_Alt_sd,bias corrected difference (alt-TG_bias_corr).
#' @param tgfile a character string giving the name of the file.
#' @return A list that includes:
#' mean: The mean values of the field in the box, defined by boxlon and boxlat at each tide gauge position.
#' sd: The standard deviation of the field in the box, defined by boxlon and boxlat at each tide gauge position.
#' bias: The bias between the mean field values and the tide gauge MDT values.
#' diff: The difference between the mean field values and the bias corrected tide gauge MDT values
#' RMS: The RMS of the mean field values and the tide gauge MDT values
#' @details Besides the list, that is returned. A plot of the difference between the altimetry and the tide gauges MDT values is automatically generated  
#' @export
#' 
#' 
compareWithTG<-function(TG,dat,lonlim,latlim,boxlon=3,boxlat=3,export=FALSE,tgfile="TGcompare.csv"){
    if(!"TGMDT" %in% colnames(TG)){
        stop(paste("The column 'TGMDT' is not in the 'Tide gauge file'"))
    }
    else{
        TGid<-getTGid(TG,lonlim,latlim)
        idlon<-TGid[,1]
        idlat<-TGid[,2]
        NR<-nrow(TGid)
        mymean<-rep(NA,NR)
        mysub<-list()
        mysd<-rep(NA,NR)
        for (i in 1:NR){
            if(idlon[i]>boxlon & idlon[i]<dim(dat$g)[1]-boxlon & idlat[i]>=boxlat&idlat[i]<dim(dat$g)[2]-boxlat)
                {
                    sub<-dat$g[(idlon[i]-boxlon):(idlon[i]+boxlon),(idlat[i]-boxlat):(idlat[i]+boxlat)]
                    mysub[[i]]<-sub
                    
                    NoGod<-(boxlon*2+1)*(boxlat*2+1)-length(which(is.na(sub)))                       
                    mymean[i]<-mean(sub,na.rm=TRUE)
                    mysd[i]<-sd(sub,na.rm=TRUE)/sqrt(NoGod)           
                }
        }
        mydif<-TG$TGMDT-mymean
        if(any(is.na(mydif))){
            idna<-which(is.na(mydif))
            RMSuc<-sqrt(sum(mydif[-idna]^2)/length(mydif[-idna]))
        }
        else{
            RMSuc<-sqrt(sum(mydif^2)/length(mydif))
            }
        SDdif<-sd(mydif,na.rm=TRUE)
        bias<-median(mydif,na.rm=TRUE) #this could be improved
        if(bias > 0) new<-TG$TGMDT-bias
        else new<-TG$TGMDT+abs(bias)
        newdif<-mymean-new
        if(any(is.na(newdif))){
            idna<-which(is.na(newdif))
            RMS<-sqrt(sum(newdif[-idna]^2)/length(newdif[-idna]))
        }
        else{
            RMS<-sqrt(sum(newdif^2)/length(newdif))
        }
        comb<-cbind(TG,new,mymean,mysd)
        #low<-newdif-2*mysd
        #high<-newdif+2*mysd
        low<-mydif-2*mysd
        high<-mydif+2*mysd
        myrange<-range(high,low,na.rm=TRUE)
        myrange[1]<-myrange[1]-0.2*(myrange[2]-myrange[1])
        par(mfrow=c(1,2),mar=c(4,4,1,1))
        plot(TG$Latitude,mydif,xlab='Latitude',ylab='MDT_field-MDT_TG',ylim=myrange)
        #plot(TG$Latitude,newdif,xlab='Latitude',ylab='MDT_field-MDT_TG',ylim=myrange)
        arrows(TG$Latitude, y0=low, y1=high, length=0.05, angle=90, code=3,col='black',lwd=2)
        abline(h=0,lty=2,lwd=2,col=gray(0.5))
        abline(h=bias,lty=2,lwd=2,col='red')
        
        legend('bottomleft',legend=c(paste('SD of difference = ',signif(SDdif,3),' m',sep=''),paste('RMS = ',signif(RMSuc,3),' m',sep=''),paste('Bias corrected RMS = ',signif(RMS,3),' m',sep=''),paste('Bias = ',signif(bias,3),' m',sep='')),bty=c('n','n','n','l'),lty=c(NA,NA,NA,2),col=c(NA,NA,NA,'red'))
        hist(newdif,n=20, col='lightblue',xlab='Bias corrected difference [m]')
        par(mfrow=c(1,1))
        if(export){
            data=data.frame(TG,Alt_mean=mymean,Alt_sd=mysd,biasCorrDif=newdif)
            write.csv(data,file=tgfile,row.names=FALSE, quote=FALSE)
        }
        return(list(mean=mymean,sd=mysd,diff=newdif,RMS=RMS,bias=bias))
    }
}

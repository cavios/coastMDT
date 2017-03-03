#coastMDT test script

library(coastMDT)
#get data from web site and save in local folder

getData(localdir='datMDT')

#load
load('DTU15MSS.rda')
load('dDTU15MSS_ref2003_2007.rda')
load('eigen6c4r.rda') 
load('ibCor5Y_2003_2007.rda')
load('landmask8.rda')
load('TG.rda')

#the raw MDT
raw<-DTU15MSS+dDTU15MSS_ref2003_2007+ibCor5Y_2003_2007-eigen6c4r

#TGMDT
TGMDT<-TG$RLR_ell_2005.5+TG$MSL_2003_2007+ ellipsoidTF2MT(TG$Latitude)+wgs2topCorr(TG$Latitude)-TG$eigen6c4rC

TG<-cbind(TG,TGMDT)

#Region
lonlim<-c(350,10)
latlim<-c(47,57)

#sub grids
mask<-getSubGrid(landmask8,lonlim,latlim)
rawSub<-getSubGrid(raw,lonlim,latlim)
idraw<-which(mask$g==0,arr.ind=TRUE)
rawSub$g[idraw]<-NA
TGsub<-getSubTG(TG,lonlim,latlim)

#land value
mylandTG_ALT<-getLandVal(rawSub,mask,lonlim,latlim,TG=TGsub,type="both",intMethod="lin")
#Filtering
boxTG_ALT<-iterativeAveSmoother(rawSub,mask,mylandTG_ALT)

#plot
plotMDT(boxTG_ALT,c(-0.5,0.5),conlev=0.05) 
points(TGsub$Longitude,TGsub$Latitude,col='red',pch=3)


#compare with tide gauge                                        
outTG<-compareWithTG(TGsub, boxTG_ALT, lonlim, latlim,export=TRUE,tgfile="myfile.csv")

#save grid
grid2file(boxTG_ALT,filename='MDT_TG_Alt_NEurope.nc')
#dev.off()





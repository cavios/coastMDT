##' Function to download data from the web
##' @param localdir A character string with the name of the directory where the data will be stored. If no name is given the data is automatically stored in a temporal directory.
##' @param files A character vector with the names of the files to be downloaded. If not specified all data for the coastMDT package is downloaded.
##' @param url A character string with the url, that specifies where the data is located. If not specified, the url is where the data for the coastMDT package is located.
##' @details ...
##' @export
getData<-function(localdir=tempdir(), files=NULL, url="https://raw.githubusercontent.com/cavios/coastMDT/master/data/files"){
  setwd(localdir)
  if(is.null(files)){
    files <- c("dacCor5Y_2003_2007.rda", "dDTU15MSS_ref2003_2007.rda",
               "difmss15eig6c4r.rda", "DTU15MSS.rda", "eigen6c4r.rda",
               "ibCor5Y_2003_2007.rda", "landmask8.rda", "mean2TF_AddThis.rda",
               "TF2mean_AddThis.rda", "TG.rda")
  }  
  d <- lapply(files, function(f)download.file(paste(url,f,sep=""), f))
}

##' Anisotropic gauss kernel 
##' @param sigmau ...
##' @param sigmav ...
##' @param ny ...
##' @param nx ...
##' @details ...
##' @export
gaussAniKernel<-function(sigmau,sigmav,ny,nx){
   nnx<-2*nx+1
   nny<-2*ny+1
   u<-seq(1:ny)
   v<-seq(1:nx)
   uu<-c(rev(u),0,u)
   vv<-c(rev(v),0,v)
   U<-matrix(rep(uu,nnx),ncol=nnx)
   V<-matrix(rep(vv,nny),nrow=nny,byrow=TRUE)
   g<-1/(2*pi*sigmav*sigmau)*exp(-0.5*(U^2/sigmau^2+V^2/sigmav^2))
   return(g) 
}

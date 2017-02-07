##' Function for converting between different permanent tide systems. 
##' @param phi The latitude in degrees.
##' @param convtype A character string giving the type of conversion. The legal strings are; 'MT2ZT', 'ZT2MT', 'ZT2TF', 'TF2ZT', 'MT2TF', and 'TF2MT'. Here, MT is mean tide, ZT is zero tide and TF is tide free (or nontidal)
##' @param k is a love number, the default value is k=0.3
##' @return The conversion correction; An array of height differences in meters.
##' @details The conversion expressions are based on Ekman, 1989. The correction must be added.
##' @export
tideConvert<-function(phi,convtype,k=0.3){
    phi<-phi/180*pi
    c<-0.099-0.296*sin(phi)^2
    
    switch(convtype,
           MT2ZT ={out <- c
               },
           ZT2MT={out <- -c
              },
           ZT2TF={out <- k*c
               },
           TF2ZT={out <- -k*c
              },
           MT2TF={out <- (1+k)*c
              },
           TF2MT={out <- -(1+k)*c
              },
           stop("unknown permanent tide conversion!")
           )
    return(out)
}


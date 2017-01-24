PEdaily_Oudin <- function(JD,Temp,LatRad){

    PE_Oudin_D <- rep(NA,length(Temp));
    for(k in 1:length(Temp)){

      FI <- LatRad  ### latitude in rad
      ### FI <- LatDeg/(180/pi)  ### conversion from deg to rad
      COSFI <- cos(FI)
      AFI <- abs(LatRad/42.)
     
      TETA <- 0.4093*sin(JD[k]/58.1-1.405)
      COSTETA <- cos(TETA)
      COSGZ <- max(0.001,cos(FI-TETA))
      GZ <- acos(COSGZ)
      COSGZ2 <- COSGZ*COSGZ
      if(COSGZ2 >= 1){ SINGZ <- 0. } else { SINGZ <- sqrt(1.-COSGZ2) }
      COSOM <- 1.-COSGZ/COSFI/COSTETA
      if(COSOM < -1.){ COSOM <- -1. }
      if(COSOM >  1.){ COSOM <-  1. }
      COSOM2 <- COSOM*COSOM
      if(COSOM2 >= 1.){ SINOM <- 0. } else { SINOM <- sqrt(1.-COSOM2) }
      OM <- acos(COSOM)
      COSPZ <- COSGZ+COSFI*COSTETA*(SINOM/OM-1.)
      if(COSPZ < 0.001){ COSPZ <- 0.001 }
      ETA <- 1.+cos(JD[k]/58.1)/30.
      GE <- 446.*OM*COSPZ*ETA
      
      if(Temp[k] >= -5.0) { PE_Oudin_D[k] <- GE*(Temp[k]+5.)/100./28.5 } else { PE_Oudin_D[k] <- 0 }

    }

    return(PE_Oudin_D);

}


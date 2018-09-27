PEdaily_Oudin <- function(JD, Temp, LatRad, Lat, LatUnit = c("rad", "deg")) {
  
  if (!missing(LatRad)) {
    warning("Deprecated \"LatRad\" argument. Please, use \"Lat\" instead.")
    if (missing(Lat)) {
      Lat <- LatRad
    }
  }
  
  if (!any(LatUnit %in%c("rad", "deg"))) {
    stop("\"LatUnit\" must be \"rad\" or \"deg\".")
  }
  
  PE_Oudin_D <- rep(NA, length(Temp))
  
  if (LatUnit[1L] == "rad") {
    FI <- Lat
  }
  if (LatUnit[1L] == "deg") {
    FI <- Lat / (180 / pi)
  }
  
  COSFI <- cos(FI)
  AFI <- abs(FI / 42) 
  
  for (k in seq_along(Temp)) {
    
    TETA <- 0.4093 * sin(JD[k] / 58.1 - 1.405)
    COSTETA <- cos(TETA)
    COSGZ <- max(0.001, cos(FI - TETA))
    GZ <- acos(COSGZ)
    COSOM <- 1 - COSGZ / COSFI / COSTETA
    
    if (COSOM < -1) {
      COSOM <- -1
    }
    if (COSOM > 1) {
      COSOM <-  1
    }
    
    COSOM2 <- COSOM * COSOM
    
    if (COSOM2 >= 1) {
      SINOM <- 0
    } else {
      SINOM <- sqrt(1 - COSOM2)
    }
    
    OM <- acos(COSOM)
    COSPZ <- COSGZ + COSFI * COSTETA * (SINOM/OM - 1)
    
    if (COSPZ < 0.001) {
      COSPZ <- 0.001
    }
    
    ETA <- 1 + cos(JD[k] / 58.1) / 30
    GE <- 446 * OM * COSPZ * ETA
    
    if (Temp[k] >= -5.0) {
      PE_Oudin_D[k] <- GE * (Temp[k] + 5) / 100 / 28.5
    } else {
      PE_Oudin_D[k] <- 0
    }
    
  }
  
  return(PE_Oudin_D)
  
}


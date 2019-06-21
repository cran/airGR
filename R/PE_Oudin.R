PE_Oudin <- function(JD, Temp,
                     Lat, LatUnit = c("rad", "deg"),
                     TimeStepIn = "daily", TimeStepOut = "daily") {
  
  
  ## ---------- check arguments
  
  # if (!missing(LatRad)) {
  #   warning("Deprecated 'LatRad' argument. Please, use 'Lat' instead.")
  #   if (missing(Lat)) {
  #     Lat <- LatRad
  #   }
  # }
  if (!(inherits(JD, "numeric") | inherits(JD, "integer"))) {
    stop("'JD' must be of class 'numeric'")
  }
  if (!(inherits(Temp, "numeric") | inherits(Temp, "integer"))) {
    stop("'Temp' must be of class 'numeric'")
  }
  if (length(JD) != length(Temp)) {
    stop("'Temp' and 'LatUnit' must have the same length")
  }
  if (!any(LatUnit %in% c("rad", "deg"))) {
    stop("'LatUnit' must be one of \"rad\" or \"deg\"")
  }
  if (!inherits(Lat, "numeric") | length(Lat) != 1) {
    stop("'Lat' must be a 'numeric' of length one")
  }
  if (LatUnit[1L] == "rad" & ((Lat >= pi/2) | (Lat <= -pi/2))) {
    stop("'Lat' must be comprised between -pi/2 and +pi/2 degrees")
  }
  if (LatUnit[1L] == "deg" & ((Lat >= 90) | (Lat <= -90))) {
    stop("'Lat' must be  comprised between -90 and +90 degrees")
  }
  if (LatUnit[1L] == "rad") {
    FI <- Lat
  }
  if (LatUnit[1L] == "deg") {
    FI <- Lat / (180 / pi)
  }
  if (any(JD < 0) | any(JD > 366)) {
    stop("'JD' must only contain integers from 1 to 366")
  }
  if (!inherits(TimeStepIn, "character") | length(TimeStepIn) != 1) {
    stop("'TimeStepIn' must be a 'character' of length one")
  }
  if (!inherits(TimeStepOut, "character") | length(TimeStepOut) != 1) {
    stop("'TimeStepIn' must be a 'character' of length one")
  }
  if (!(TimeStepIn %in% c("daily", "hourly"))) {
    stop("'TimeStepIn' must be one of \"daily\" or \"hourly\"")
  }
  if (!(TimeStepOut %in% c("daily", "hourly"))) {
    stop("'TimeStepOut' must be one of \"daily\" or \"hourly\"")
  }
  rleJD <- rle(JD)
  if (TimeStepIn == "daily" & any(rleJD$lengths != 1)) {
    stop("each day must have only one identical value of julian days")
  }
  if (TimeStepIn == "hourly" & any(rleJD$lengths != 24)) {
    stop("each day must have 24 identical values of julian days (one for each hour)")
  }
  
  
  ## ---------- hourly inputs aggregation
  
  if (TimeStepIn == "hourly") {
    JD <- rleJD$values
    idJD <- rep(seq_along(JD), each = rleJD$lengths[1L])
    Temp <- as.vector(tapply(X = Temp, INDEX = idJD, FUN = mean))
  }
  
  
  ## ---------- Oudin's formula
  
  PE_Oudin_D <- rep(NA, length(Temp))
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
    
    if (is.na(Temp[k])) {
      PE_Oudin_D[k] <- NA
    } else {
      if (Temp[k] >= -5.0) {
        PE_Oudin_D[k] <- GE * (Temp[k] + 5) / 100 / 28.5
      } else {
        PE_Oudin_D[k] <- 0
      }
    }
    
  }
  
  
  ## ---------- disaggregate PE from daily to hourly
  
  if (TimeStepOut == "hourly") {
    parab_D2H <- c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                   0.035, 0.062, 0.079, 0.097, 0.110, 0.117,
                   0.117, 0.110, 0.097, 0.079, 0.062, 0.035,
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000)
    PE_Oudin_H <- rep(PE_Oudin_D, each = 24) * rep(parab_D2H, times = length(PE_Oudin_D))
  }
  
  
  ## ---------- outputs warnings
  
  if (any(is.na(Temp))) {
    if (any(is.na(PE_Oudin_D))) {
      warning("'Temp' time series, and therefore the returned 'PE' time series, contain missing value(s)")
    } else {
      warning("'Temp' time series contains missing value(s)")
    }
  }
  if (!any(is.na(Temp)) & any(is.na(PE_Oudin_D))) {
    warning("returned 'PE' time series contains missing value(s)")
  }
  
  if (TimeStepOut == "daily") {
    PE_Oudin <- PE_Oudin_D
  } else {
    PE_Oudin <- PE_Oudin_H
  }
  return(PE_Oudin)
  
}

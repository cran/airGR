PE_Oudin <- function(JD, Temp,
                     Lat, LatUnit = c("rad", "deg"),
                     TimeStepIn = "daily", TimeStepOut = "daily",
                     RunFortran = FALSE) {


  ## ---------- check arguments

  if (!(inherits(JD, "numeric") | inherits(JD, "integer"))) {
    stop("'JD' must be of class 'numeric'")
  }
  if (!(inherits(Temp, "numeric") | inherits(Temp, "integer"))) {
    stop("'Temp' must be of class 'numeric'")
  }
  if (length(JD) != length(Temp)) {
    stop("'JD' and 'Temp' must have the same length")
  }
  if (!RunFortran & (!inherits(Lat, "numeric") | length(Lat) != 1)) {
    stop("'Lat' must be a 'numeric' of length one")
  }
  if (RunFortran & (!inherits(Lat, "numeric") | (!length(Lat) %in% c(1, length(Temp))))) {
    stop("'Lat' must be a 'numeric' of length one or of the same length as 'Temp'")
  }
  LatUnit <- match.arg(LatUnit, choices = c("rad", "deg"))
  if (LatUnit[1L] == "rad" & (all(Lat >= pi/2) | all(Lat <= -pi/2))) {
    stop("'Lat' must be comprised between -pi/2 and +pi/2 degrees")
  }
  if (LatUnit[1L] == "deg" & (all(Lat >= 90) | all(Lat <= -90))) {
    stop("'Lat' must be comprised between -90 and +90 degrees")
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
  TimeStep <- c("daily", "hourly")
  TimeStepIn  <- match.arg(TimeStepIn , choices = TimeStep)
  TimeStepOut <- match.arg(TimeStepOut, choices = TimeStep)
  rleJD <- rle(JD)
  msgDaliy <- "each day should have only one identical value of Julian days. The time series is not sorted, or contains duplicate or missing dates"
  msgHourly <- "each day must have 24 identical values of Julian days (one for each hour). The time series is not sorted, or contains duplicate or missing dates"
  if (TimeStepIn == "daily" & any(rleJD$lengths != 1)) {
    warning(msgDaliy)
  }


  ## ---------- hourly inputs aggregation

  if (TimeStepIn == "hourly") {
    JD <- rleJD$values
    idJD <- rep(seq_along(JD), each = rleJD$lengths[1L])
    if (length(Temp) != length(idJD)) {
      stop(msgHourly)
    } else {
      Temp <- as.vector(tapply(X = Temp, INDEX = idJD, FUN = mean))
    }
  }
  if (TimeStepIn == "hourly" & any(rleJD$lengths != 24)) {
    warning(msgHourly)
  }


  ## ---------- Oudin's formula

  if (RunFortran) {

    LInputs = as.integer(length(Temp))

    if (length(FI) == 1) {
      FI <- rep(FI, LInputs)
    }

    RESULTS <- .Fortran("frun_pe_oudin", PACKAGE = "airGR",
                        ##inputs
                        LInputs = LInputs,
                        InputsLAT = as.double(FI),
                        InputsTT = as.double(Temp),
                        InputsJJ = as.double(JD),
                        ##outputs
                        PE_Oudin_D = rep(as.double(-99e9), LInputs)
    )
    PE_Oudin_D = RESULTS$PE_Oudin_D

  } else {

    PE_Oudin_D <- rep(NA, length(Temp))
    COSFI <- cos(FI)

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
        COSOM <- 1
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

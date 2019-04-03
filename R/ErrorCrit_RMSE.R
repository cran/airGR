ErrorCrit_RMSE <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {
  
  
  ##Arguments_check________________________________
  if (!inherits(InputsCrit, "InputsCrit")) {
    stop("'InputsCrit' must be of class 'InputsCrit'")
  }
  if (inherits(InputsCrit, "Multi") | inherits(InputsCrit, "Compo")) {
    stop("'InputsCrit' must be of class 'Single'. Use the 'ErrorCrit' function on objects of class 'Multi' with RMSE")
  }
  if (!inherits(OutputsModel, "OutputsModel")) {
    stop("'OutputsModel' must be of class 'OutputsModel'")
  }
  
  
  ##Initialisation_________________________________
  CritName <- NA
  CritVar  <- InputsCrit$VarObs
  if (InputsCrit$transfo == "") {
    CritName <- "RMSE[CritVar]"
  }
  if (InputsCrit$transfo == "sqrt") {
    CritName <- "RMSE[sqrt(CritVar)]"
  }
  if (InputsCrit$transfo == "log") {
    CritName <- "RMSE[log(CritVar)]"
  }
  if (InputsCrit$transfo == "inv") {
    CritName <- "RMSE[1/CritVar]"
  }
  if (InputsCrit$transfo == "sort") {
    CritName <- "RMSE[sort(CritVar)]"
  }
  CritName      <- gsub(pattern = "CritVar", replacement = CritVar, x = CritName)
  CritValue     <- NA
  CritBestValue <- +1
  Multiplier    <- +1
  ### must be equal to -1 or +1 only
  
  
  ##Data_preparation_______________________________
  VarObs <- InputsCrit$Obs
  VarObs[!InputsCrit$BoolCrit] <- NA
  if (InputsCrit$VarObs == "Q") {
    VarSim <- OutputsModel$Qsim
  }
  if (InputsCrit$VarObs == "SCA") {
    VarSim <- rowMeans(sapply(OutputsModel$CemaNeigeLayers[InputsCrit$idLayer], FUN = "[[", "Gratio"))
  }
  if (InputsCrit$VarObs == "SWE") {
    VarSim <- rowMeans(sapply(OutputsModel$CemaNeigeLayers[InputsCrit$idLayer], FUN = "[[", "SnowPack"))
  }
  VarSim[!InputsCrit$BoolCrit] <- NA
  
  ##Data_transformation
  if (InputsCrit$transfo %in% c("log", "inv") & is.null(InputsCrit$epsilon) & warnings) {
    if (any(VarObs %in% 0)) {
      warning("zeroes detected in 'Qobs': the corresponding time-steps will be excluded from the criteria computation if the epsilon argument of 'CreateInputsCrit' = NULL")
    }
    if (any(VarSim %in% 0)) {
      warning("zeroes detected in 'Qsim': the corresponding time-steps will be excluded from the criteria computation if the epsilon argument of 'CreateInputsCrit' = NULL")
    }  
  }
  if ("epsilon" %in% names(InputsCrit) & !is.null(InputsCrit$epsilon)) {
    VarObs <- VarObs + InputsCrit$epsilon
    VarSim <- VarSim + InputsCrit$epsilon
  }
  if (InputsCrit$transfo == "sqrt") {
    VarObs <- sqrt(VarObs)
    VarSim <- sqrt(VarSim)
  }
  if (InputsCrit$transfo == "log") {
    VarObs <- log(VarObs)
    VarSim <- log(VarSim)
    VarSim[VarSim      < -1e100] <- NA
  }
  if (InputsCrit$transfo == "inv") {
    VarObs <- 1 / VarObs
    VarSim <- 1 / VarSim
    VarSim[abs(VarSim) > 1e+100] <- NA
  }
  if (InputsCrit$transfo == "sort") {
    VarSim[is.na(VarObs)] <- NA
    VarSim <- sort(VarSim, na.last = TRUE)
    VarObs <- sort(VarObs, na.last = TRUE)
    InputsCrit$BoolCrit <-
      sort(InputsCrit$BoolCrit, decreasing = TRUE)
  }
  
  ##TS_ignore
  TS_ignore <- !is.finite(VarObs) | !is.finite(VarSim) | !InputsCrit$BoolCrit
  Ind_TS_ignore <- which(TS_ignore)
  if (length(Ind_TS_ignore) == 0) {
    Ind_TS_ignore <- NULL
  }
  if (sum(!TS_ignore) == 0) {
    OutputsCrit <- list(NA)
    names(OutputsCrit) <- c("CritValue")
    return(OutputsCrit)
  }
  if (inherits(OutputsModel, "hourly")) {
    WarningTS <- 365
  }
  if (inherits(OutputsModel, "daily")) {
    WarningTS <- 365
  }
  if (inherits(OutputsModel, "monthly")) {
    WarningTS <-  12
  }
  if (inherits(OutputsModel, "yearly")) {
    WarningTS <-   3
  }
  if (sum(!TS_ignore) < WarningTS & warnings) {
    warning("\t criterion computed on less than ", WarningTS, " time-steps")
  }
  
  
  ##ErrorCrit______________________________________
  Numer <- sum((VarSim - VarObs)^2, na.rm = TRUE)
  Denom <- sum(!is.na(VarObs))
  
  if (Numer == 0) {
    Crit <- 0
  } else {
    Crit <- sqrt(Numer / Denom)
  }
  if (is.numeric(Crit) & is.finite(Crit)) {
    CritValue <- Crit
  }
  
  
  ##Verbose______________________________________
  if (verbose) {
    message("Crit. ", CritName, " = ", sprintf("%.4f", CritValue), "\n")
  }
  
  
  ##Output_________________________________________
  OutputsCrit <- list(CritValue       = CritValue,
                      CritName        = CritName,
                      CritBestValue   = CritBestValue,
                      Multiplier      = Multiplier,
                      Ind_notcomputed = Ind_TS_ignore
  )
  
  class(OutputsCrit) <- c("RMSE", "ErrorCrit")
  return(OutputsCrit)
  
}

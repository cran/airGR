## =================================================================================
## function to manage inputs of specific ErrorCrit_*() functions
## =================================================================================

.ErrorCrit <- function(InputsCrit, Crit, OutputsModel, warnings) {

  ## Arguments check
  if (!inherits(InputsCrit, "InputsCrit")) {
    stop("'InputsCrit' must be of class 'InputsCrit'", call. = FALSE)
  }
  if (inherits(InputsCrit, "Multi") | inherits(InputsCrit, "Compo")) {
    stop(paste0("'InputsCrit' must be of class 'Single'. Use the 'ErrorCrit' function on objects of class 'Multi' or 'Compo' with ", Crit), call. = FALSE)
  }


  ## Initialisation
  CritName <- NA
  CritVar  <- InputsCrit$VarObs
  if (InputsCrit$transfo == "") {
    CritName <- paste0(Crit, "[CritVar]")
  }
  if (InputsCrit$transfo %in% c("sqrt", "log", "sort", "boxcox")) {
    CritName <- paste0(Crit, "[", InputsCrit$transfo, "(CritVar)]")
  }
  if (InputsCrit$transfo == "inv") {
    CritName <- paste0(Crit, "[1/CritVar]")
  }
  if (grepl("\\^", InputsCrit$transfo)) {
    transfoPow <- suppressWarnings(as.numeric(gsub("\\^", "", InputsCrit$transfo)))
    CritName <- paste0(Crit, "[CritVar^", transfoPow, "]")
  }
  CritName  <- gsub(pattern = "CritVar", replacement = CritVar, x = CritName)
  CritValue <- NA
  if (Crit %in% c("RMSE")) {
    CritBestValue <- +1
    Multiplier    <- +1
  }
  if (Crit %in% c("NSE", "KGE", "KGE2", "GAPX")) {
    CritBestValue <- +1
    Multiplier    <- -1
  }


  ## Data preparation
  VarObs <- InputsCrit$Obs
  VarObs[!InputsCrit$BoolCrit] <- NA
  VarSim <- switch(
    InputsCrit$VarObs,
    Q = OutputsModel$Qsim,
    SCA = rowMeans(sapply(OutputsModel$CemaNeigeLayers[InputsCrit$idLayer], FUN = "[[", "Gratio")),
    SWE = rowMeans(sapply(OutputsModel$CemaNeigeLayers[InputsCrit$idLayer], FUN = "[[", "SnowPack")),
    ParamT = OutputsModel$RunOptions$ParamT
  )

  VarSim[!InputsCrit$BoolCrit] <- NA


  ## Data transformation
  if (InputsCrit$transfo %in% c("log", "inv") & is.null(InputsCrit$epsilon) & warnings) {
    if (any(VarObs %in% 0)) {
      warning("zeroes detected in 'Qobs': the corresponding time-steps will be excluded from the criteria computation if the epsilon argument of 'CreateInputsCrit' = NULL", call. = FALSE)
    }
    if (any(VarSim %in% 0)) {
      warning("zeroes detected in 'Qsim': the corresponding time-steps will be excluded from the criteria computation if the epsilon argument of 'CreateInputsCrit' = NULL", call. = FALSE)
    }
  }
  if ("epsilon" %in% names(InputsCrit) & !is.null(InputsCrit$epsilon) & !(InputsCrit$transfo == "boxcox")) {
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
    InputsCrit$BoolCrit <- sort(InputsCrit$BoolCrit, decreasing = TRUE)
  }
  if (InputsCrit$transfo == "boxcox") {
    muTransfoVarObs <- (0.01 * mean(VarObs, na.rm = TRUE))^0.25
    VarSim <- (VarSim^0.25 - muTransfoVarObs) / 0.25
    VarObs <- (VarObs^0.25 - muTransfoVarObs) / 0.25
  }
  if (grepl("\\^", InputsCrit$transfo)) {
    VarObs <- VarObs^transfoPow
    VarSim <- VarSim^transfoPow
  }


  ## TS_ignore
  TS_ignore <- !is.finite(VarObs) | !is.finite(VarSim) | !InputsCrit$BoolCrit
  Ind_TS_ignore <- which(TS_ignore)
  if (length(Ind_TS_ignore) == 0) {
    Ind_TS_ignore <- NULL
  }
  if (sum(!TS_ignore) == 0 | (sum(!TS_ignore) == 1 & Crit %in% c("KGE", "KGE2"))) {
    CritCompute <- FALSE
  } else {
    CritCompute <- TRUE
  }
  if (Crit != "GAPX") {
    WarningTS <- 10
    if (sum(!TS_ignore) < WarningTS & warnings) {
      warning("\t criterion computed on less than ", WarningTS, " time-steps", call. = FALSE)
    }
  } else {
    WarningTS <- 4 # For at least daily time step models (GR4J)
    if (sum(!TS_ignore) < WarningTS & warnings) {
      warning("\t criterion GAPX computed on less than ", WarningTS, " parameters", call. = FALSE)
    }
  }


  ## Outputs
  OutputsCritCheck <- list(WarningTS = WarningTS,
                           VarObs = VarObs,
                           VarSim = VarSim,
                           CritBestValue = CritBestValue,
                           Multiplier = Multiplier,
                           CritName = CritName,
                           CritVar = CritVar,
                           CritCompute = CritCompute,
                           TS_ignore = TS_ignore,
                           Ind_TS_ignore = Ind_TS_ignore)
}


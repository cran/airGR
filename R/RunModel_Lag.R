RunModel_Lag <- function(InputsModel, RunOptions, Param, QcontribDown) {
  NParam <- 1

  ##Arguments_check
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, "SD")) {
    stop("'InputsModel' must be of class 'SD'")
  }
  if (!inherits(RunOptions, "RunOptions")) {
    stop("'RunOptions' must be of class 'RunOptions'")
  }
  if (!is.vector(Param) | !is.numeric(Param)) {
    stop("'Param' must be a numeric vector")
  }
  if (sum(!is.na(Param)) != NParam) {
    stop(paste("'Param' must be a vector of length", NParam, "and contain no NA"))
  }
  if (inherits(QcontribDown, "OutputsModel")) {
    if (is.null(QcontribDown$Qsim)) {
      stop("'QcontribDown' should contain a key 'Qsim' containing the output of the runoff of the downstream subcatchment")
    }
    OutputsModel <- QcontribDown
    OutputsModel$QsimDown <- OutputsModel$Qsim
  } else if (is.vector(QcontribDown) && is.numeric(QcontribDown)) {
    OutputsModel <- list()
    class(OutputsModel) <- c("OutputsModel", class(OutputsModel))
    OutputsModel$QsimDown <- QcontribDown
  } else {
    stop("'QcontribDown' must be a numeric vector or a 'OutputsModel' object")
  }
  if (length(OutputsModel$QsimDown) != length(RunOptions$IndPeriod_Run)) {
    stop("Time series in 'QcontribDown' should have the same lenght as 'RunOptions$IndPeriod_Run'")
  }

  if (inherits(InputsModel, "hourly")) {
    TimeStep <- 60 * 60
  } else if (inherits(InputsModel, "daily")) {
    TimeStep <- 60 * 60 * 24
  } else {
    stop("'InputsModel' should be of class \"daily\" or \"hourly\"")
  }

  # propagation time from upstream meshes to outlet
  PT <- InputsModel$LengthHydro * 1e3 / Param[1L] / TimeStep
  HUTRANS <- rbind(1 - (PT - floor(PT)), PT - floor(PT))

  NbUpBasins <- length(InputsModel$LengthHydro)
  LengthTs <- length(OutputsModel$QsimDown)
  OutputsModel$Qsim_m3 <- OutputsModel$QsimDown * InputsModel$BasinAreas[length(InputsModel$BasinAreas)] * 1e3

  IniSD <- RunOptions$IniStates[grep("SD", names(RunOptions$IniStates))]
  if (length(IniSD) > 0) {
    if (sum(floor(PT)) + NbUpBasins != length(IniSD)) {
      stop(
        sprintf(
          "SD initial states has a length of %i and a length of %i is required",
          length(IniSD),
          sum(floor(PT)) + NbUpBasins
        )
      )
    }
    IniStates <- lapply(seq_len(NbUpBasins), function(x) {
      iStart <- 1
      if (x > 1) {
        iStart <- iStart + sum(floor(PT[1:x - 1]) + 1)
      }
      IniSD[iStart:(iStart + PT[x])]
    })
  } else {
    IniStates <- lapply(seq_len(NbUpBasins), function(x) {
      rep(0, floor(PT[x] + 1))
    })
  }
  # message("Initstates: ", paste(IniStates, collapse = ", "))

  for (upstream_basin in seq_len(NbUpBasins)) {
    Qupstream <- c(IniStates[[upstream_basin]],
                   InputsModel$Qupstream[RunOptions$IndPeriod_Run, upstream_basin])
    # message("Qupstream[", upstream_basin, "]: ", paste(Qupstream, collapse = ", "))
    OutputsModel$Qsim_m3 <- OutputsModel$Qsim_m3 +
      Qupstream[2:(1 + LengthTs)] * HUTRANS[1, upstream_basin] +
      Qupstream[1:LengthTs] * HUTRANS[2, upstream_basin]
  }
  # Convert back Qsim to mm
  OutputsModel$Qsim <- OutputsModel$Qsim_m3 / sum(InputsModel$BasinAreas, na.rm = TRUE) / 1e3
  # message("Qsim: ", paste(OutputsModel$Qsim, collapse = ", "))

  # Warning for negative flows or NAs only in extended outputs
  if(length(RunOptions$Outputs_Sim) > 2) {
    if (any(OutputsModel$Qsim[!is.na(OutputsModel$Qsim)] < 0)) {
      warning(length(which(OutputsModel$Qsim < 0)), " time steps with negative flow, set to zero.")
      OutputsModel$Qsim[OutputsModel$Qsim < 0] <- 0
    }
    # Warning for NAs
    if (any(is.na(OutputsModel$Qsim))) {
      warning(length(which(is.na(OutputsModel$Qsim))), " time steps with NA values")
    }
  }

  if ("StateEnd" %in% RunOptions$Outputs_Sim) {
    OutputsModel$StateEnd$SD <- lapply(seq(NbUpBasins), function(x) {
      lastTS <- RunOptions$IndPeriod_Run[length(RunOptions$IndPeriod_Run)]
      InputsModel$Qupstream[(lastTS - floor(PT[x])):lastTS, x]
    })
    # message("StateEnd: ", paste(OutputsModel$StateEnd$SD, collapse = ", "))
  }

  return(OutputsModel)
}

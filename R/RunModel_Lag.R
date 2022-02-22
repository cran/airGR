RunModel_Lag <- function(InputsModel, RunOptions, Param, QcontribDown) {
  NParam <- 1

  ## argument check
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
    if (length(QcontribDown$Qsim) != length(RunOptions$IndPeriod_Run)) {
      stop("Time series Qsim in 'QcontribDown' should have the same length as 'RunOptions$IndPeriod_Run'")
    }
    if (!identical(RunOptions$IndPeriod_WarmUp, 0L) && !identical(RunOptions$Outputs_Sim, RunOptions$Outputs_Cal)) {
      # This test is not necessary during calibration but usefull in other cases because
      # WarmUpQsim is then used for downstream sub-basins because of the delay in Qupstream
      if (is.null(QcontribDown$RunOptions$WarmUpQsim) ||
          length(QcontribDown$RunOptions$WarmUpQsim) != length(RunOptions$IndPeriod_WarmUp)) {
        stop("Time series WarmUpQsim in 'QcontribDown' should have the same length as 'RunOptions$IndPeriod_WarmUp'")
      }
    }
  } else if (is.vector(QcontribDown) && is.numeric(QcontribDown)) {
    if (length(QcontribDown) != length(RunOptions$IndPeriod_Run)) {
      stop("'QcontribDown' should have the same length as 'RunOptions$IndPeriod_Run'")
    }
  } else {
    stop("'QcontribDown' must be a numeric vector or a 'OutputsModel' object")
  }

  # data set up
  NbUpBasins <- length(InputsModel$LengthHydro)
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1   <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  LInputSeries <- as.integer(length(IndPeriod1))
  IndPeriod2 <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries

  if (inherits(QcontribDown, "OutputsModel")) {
    OutputsModel <- QcontribDown
    if (is.null(OutputsModel$RunOptions$WarmUpQsim)) {
      OutputsModel$RunOptions$WarmUpQsim <- rep(NA, length(RunOptions$IndPeriod_WarmUp))
    }
    QsimDown <- c(OutputsModel$RunOptions$WarmUpQsim, OutputsModel$Qsim)
  } else if (is.vector(QcontribDown) && is.numeric(QcontribDown)) {
    OutputsModel <- list()
    class(OutputsModel) <- c("OutputsModel", class(RunOptions)[-1])
    QsimDown <- c(rep(NA, length(RunOptions$IndPeriod_WarmUp)),
                  QcontribDown)
  }

  ## propagation time from upstream meshes to outlet
  PT <- InputsModel$LengthHydro * 1e3 / Param[1L] / RunOptions$FeatFUN_MOD$TimeStep
  HUTRANS <- rbind(1 - (PT - floor(PT)), PT - floor(PT))

  ## set up initial states
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
      as.vector(IniSD[iStart:(iStart + PT[x])])
    })
  } else {
    IniStates <- lapply(
      seq_len(NbUpBasins),
      function(iUpBasins) {
        iWarmUp <- seq.int(
          from = max(1, IndPeriod1[1] - floor(PT[iUpBasins]) - 1),
          to   = max(1, IndPeriod1[1] - 1)
        )
        ini <- InputsModel$Qupstream[iWarmUp, iUpBasins]
        if (length(ini) != floor(PT[iUpBasins] + 1)) {
          # If warm-up period is not enough long complete beginning with first value
          ini <- c(rep(ini[1], floor(PT[iUpBasins] + 1) - length(ini)), ini)
        }
        return(as.vector(ini))
      }
    )
  }
  # message("IniStates: ", paste(IniStates, collapse = ", "))

  ## Lag model computation
  Qsim_m3 <- QsimDown *
    InputsModel$BasinAreas[length(InputsModel$BasinAreas)] * 1e3

  for (upstream_basin in seq_len(NbUpBasins)) {
    Qupstream <- c(IniStates[[upstream_basin]],
                   InputsModel$Qupstream[IndPeriod1, upstream_basin])
    # message("Qupstream[", upstream_basin, "]: ", paste(Qupstream, collapse = ", "))
    Qsim_m3 <- Qsim_m3 +
      Qupstream[2:(1 + LInputSeries)] * HUTRANS[1, upstream_basin] +
      Qupstream[1:LInputSeries] * HUTRANS[2, upstream_basin]
  }

  ## OutputsModel

  if ("Qsim_m3" %in% RunOptions$Outputs_Sim) {
    OutputsModel$Qsim_m3 <- Qsim_m3[IndPeriod2]
  }

  if ("Qsim" %in% RunOptions$Outputs_Sim) {
    # Convert back Qsim to mm
    OutputsModel$Qsim <- Qsim_m3[IndPeriod2] / sum(InputsModel$BasinAreas, na.rm = TRUE) / 1e3
    # message("Qsim: ", paste(OutputsModel$Qsim, collapse = ", "))
  }

  if ("QsimDown" %in% RunOptions$Outputs_Sim) {
    # Convert back Qsim to mm
    OutputsModel$QsimDown <- QsimDown[IndPeriod2]
  }

  # Warning for negative flows or NAs only in extended outputs
  if (length(RunOptions$Outputs_Sim) > 2) {
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
    SD <- lapply(seq(NbUpBasins), function(x) {
      lastTS <- RunOptions$IndPeriod_Run[length(RunOptions$IndPeriod_Run)]
      InputsModel$Qupstream[(lastTS - floor(PT[x])):lastTS, x]
    })
    if (is.null(OutputsModel$StateEnd)) {
      OutputsModel$StateEnd <- CreateIniStates(RunModel_Lag, InputsModel, SD = SD)
    } else {
      OutputsModel$StateEnd$SD <- SD
    }
    # message("StateEnd: ", paste(OutputsModel$StateEnd$SD, collapse = ", "))
  }
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$WarmUpQsim <- Qsim_m3[seq_len(length(RunOptions$IndPeriod_WarmUp))] / sum(InputsModel$BasinAreas, na.rm = TRUE) / 1e3
  }

  if ("Param" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$Param <- c(Param, OutputsModel$RunOptions$Param)
  }

  class(OutputsModel) <- c(class(OutputsModel), "SD")

  return(OutputsModel)
}

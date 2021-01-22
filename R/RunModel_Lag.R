RunModel_Lag <- function(InputsModel, RunOptions, Param) {
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
  if (is.null(InputsModel$OutputsModel)) {
    stop(
      "'InputsModel' should contain an 'OutputsModel' key containing the output of the runoff of the downstream subcatchment"
    )
  }
  if (is.null(InputsModel$OutputsModel$Qsim)) {
    stop(
      "'InputsModel$OutputsModel' should contain a key 'Qsim' containing the output of the runoff of the downstream subcatchment"
    )
  }
  if (sum(!is.na(InputsModel$OutputsModel$Qsim)) != length(RunOptions$IndPeriod_Run)) {
    stop(
      "'InputsModel$OutputsModel$Qim' should have the same lenght as 'RunOptions$IndPeriod_Run' and contain no NA"
    )
  }

  OutputsModel <- InputsModel$OutputsModel
  OutputsModel$QsimDown <- OutputsModel$Qsim

  if (inherits(InputsModel, "hourly")) {
    TimeStep <- 60 * 60
  } else if (inherits(InputsModel, "daily")) {
    TimeStep <- 60 * 60 * 24
  } else {
    stop("'InputsModel' should be of class \"daily\" or \"hourly\"")
  }

  # propagation time from upstream meshes to outlet
  PT <- InputsModel$LengthHydro / Param[1L] / TimeStep
  HUTRANS <- rbind(1 - (PT - floor(PT)), PT - floor(PT))

  NbUpBasins <- length(InputsModel$LengthHydro)
  LengthTs <- length(OutputsModel$QsimDown)
  OutputsModel$Qsim <-
    OutputsModel$QsimDown * InputsModel$BasinAreas[length(InputsModel$BasinAreas)] * 1e3

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

  for (upstream_basin in seq_len(NbUpBasins)) {
    Qupstream <-
      InputsModel$Qupstream[RunOptions$IndPeriod_Run, upstream_basin]
    if (!is.na(InputsModel$BasinAreas[upstream_basin])) {
      # Upstream flow with area needs to be converted to m3 by time step
      Qupstream <-
        Qupstream * InputsModel$BasinAreas[upstream_basin] * 1e3
    }
    OutputsModel$Qsim <- OutputsModel$Qsim +
      c(IniStates[[upstream_basin]][-length(IniStates[[upstream_basin]])],
        Qupstream[1:(LengthTs - floor(PT[upstream_basin]))]) *
      HUTRANS[1, upstream_basin] +
      c(IniStates[[upstream_basin]],
        Qupstream[1:(LengthTs - floor(PT[upstream_basin]) - 1)]) *
      HUTRANS[2, upstream_basin]
  }
  # Warning for negative flows
  if (any(OutputsModel$Qsim < 0)) {
    warning(length(which(OutputsModel$Qsim < 0)), " time steps with negative flow, set to zero.")
    OutputsModel$Qsim[OutputsModel$Qsim < 0] <- 0
  }
  # Convert back Qsim to mm
  OutputsModel$Qsim <- OutputsModel$Qsim / sum(InputsModel$BasinAreas, na.rm = TRUE) / 1e3

  if ("StateEnd" %in% RunOptions$Outputs_Sim) {
    OutputsModel$StateEnd$SD <- lapply(seq(NbUpBasins), function(x) {
      Qupstream[(LengthTs - floor(PT[x])):LengthTs]
    })
  }

  return(OutputsModel)
}

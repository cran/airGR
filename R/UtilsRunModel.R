#' Create `OutputsModel` for GR non-Cemaneige models
#'
#' @param InputsModel output of [CreateInputsModel]
#' @param RunOptions output of [CreateRunOptions]
#' @param RESULTS outputs of [.Fortran]
#' @param LInputSeries number of time steps of warm-up + run periods
#' @param Param [numeric] vector of model parameters
#' @param CemaNeigeLayers outputs of Cemaneige pre-process
#'
#' @return OutputsModel object
#' @noRd
#'
.GetOutputsModelGR <- function(InputsModel,
                               RunOptions,
                               RESULTS,
                               LInputSeries,
                               Param,
                               CemaNeigeLayers = NULL) {

  IndPeriod2 <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries
  FortranOutputs <- RunOptions$FortranOutputs$GR

  IndOutputs <- which(FortranOutputs %in% RunOptions$Outputs_Sim)

  OutputsModel <- list()

  if ("DatesR" %in% RunOptions$Outputs_Sim) {
    OutputsModel$DatesR <- InputsModel$DatesR[RunOptions$IndPeriod_Run]
  }

  seqOutputs <- seq_len(RESULTS$NOutputs)
  names(seqOutputs) <- FortranOutputs[IndOutputs]

  OutputsModel <- c(OutputsModel,
                    lapply(seqOutputs, function(i) RESULTS$Outputs[IndPeriod2, i]))

  if (!is.null(CemaNeigeLayers)) {
    OutputsModel$CemaNeigeLayers <- CemaNeigeLayers
  }
  if ("WarmUpQsim" %in% RunOptions$Outputs_Sim && !identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    OutputsModel$RunOptions$WarmUpQsim <- RESULTS$Outputs[seq_len(length(RunOptions$IndPeriod_WarmUp)),
                                               which(FortranOutputs == "Qsim")]
    # class(OutputsModel$RunOptions$WarmUpQsim) <- c("WarmUpOutputsModelItem", class(OutputsModel$RunOptions$WarmUpQsim))
  }

  if ("Param" %in% RunOptions$Outputs_Sim) {
    OutputsModel$RunOptions$Param <- Param
  }

  if ("StateEnd" %in% RunOptions$Outputs_Sim) {
    OutputsModel$StateEnd <- RESULTS$StateEnd
  }


  class(OutputsModel) <- c("OutputsModel", class(RunOptions)[-1])

  return(OutputsModel)
}


#' Check arguments of `RunModel_*GR*` functions
#'
#' @param InputsModel see [CreateInputsModel]
#' @param RunOptions  see [CreateRunOptions]
#' @param Param [numeric] [vector] model calibration parameters
#'
#' @return [NULL]
#' @noRd
#'
.ArgumentsCheckGR <- function(InputsModel, RunOptions, Param) {
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, RunOptions$FeatFUN_MOD$TimeUnit)) {
    stop("'InputsModel' must be of class '", RunOptions$FeatFUN_MOD$TimeUnit, "'")
  }
  if (!inherits(InputsModel, "GR")) {
    stop("'InputsModel' must be of class 'GR'")
  }
  if (class(RunOptions)[1] != "RunOptions") {
    if (!inherits(RunOptions, "RunOptions")) {
      stop("'RunOptions' must be of class 'RunOptions'")
    } else {
      stop("'RunOptions' class of 'RunOptions' must be in first position")
    }
  }
  if (!inherits(RunOptions, "GR")) {
    stop("'RunOptions' must be of class 'GR'")
  }

  if ("CemaNeige" %in% RunOptions$FeatFUN_MOD$Class) {
    if (!inherits(InputsModel, "CemaNeige")) {
      stop("'InputsModel' must be of class 'CemaNeige'")
    }
    if (!inherits(RunOptions, "CemaNeige")) {
      stop("'RunOptions' must be of class 'CemaNeige'")
    }
  }

  if (!is.vector(Param) | !is.numeric(Param)) {
    stop("'Param' must be a numeric vector")
  }
  if (sum(!is.na(Param)) != RunOptions$FeatFUN_MOD$NbParam) {
    stop(paste("'Param' must be a vector of length", RunOptions$FeatFUN_MOD$NbParam, "and contain no NA"))
  }
}

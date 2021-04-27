RunModel_GR2M <- function(InputsModel, RunOptions, Param) {


  ## Initialization of variables
  NParam <- 2
  FortranOutputs <- .FortranOutputs(GR = "GR2M")$GR


  ## Arguments check
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, "monthly")) {
    stop("'InputsModel' must be of class 'monthly'")
  }
  if (!inherits(InputsModel, "GR")) {
    stop("'InputsModel' must be of class 'GR'")
  }
  if (!inherits(RunOptions, "RunOptions")) {
    stop("'RunOptions' must be of class 'RunOptions'")
  }
  if (!inherits(RunOptions, "GR")) {
    stop("'RunOptions' must be of class 'GR'")
  }
  if (!is.vector(Param) | !is.numeric(Param)) {
    stop("'Param' must be a numeric vector")
  }
  if (sum(!is.na(Param)) != NParam) {
    stop(paste("'Param' must be a vector of length", NParam, "and contain no NA"))
  }
  Param <- as.double(Param)

  Param_X1X2_threshold <- 1e-2
  if (Param[1L] < Param_X1X2_threshold) {
    warning(sprintf("Param[1] (X1: production store capacity [mm]) < %.2f\n X1 set to %.2f", Param_X1X2_threshold, Param_X1X2_threshold))
    Param[1L] <- Param_X1X2_threshold
  }
  if (Param[2L] < Param_X1X2_threshold) {
    warning(sprintf("Param[2] (X2: routing store capacity [mm]) < %.2f\n X2 set to %.2f", Param_X1X2_threshold, Param_X1X2_threshold))
    Param[2L] <- Param_X1X2_threshold
  }

  ## Input data preparation
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1   <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  LInputSeries <- as.integer(length(IndPeriod1))
  if ("all" %in% RunOptions$Outputs_Sim) {
    IndOutputs <- as.integer(1:length(FortranOutputs))
  } else {
    IndOutputs <- which(FortranOutputs %in% RunOptions$Outputs_Sim)
  }

  ## Output data preparation
  IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries
  ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim
  ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim

  ## Use of IniResLevels
  if (!is.null(RunOptions$IniResLevels)) {
    RunOptions$IniStates[1] <- RunOptions$IniResLevels[1] * Param[1] ### production store level (mm)
    RunOptions$IniStates[2] <- RunOptions$IniResLevels[2] * Param[2] ### routing store level (mm)
  }

  ## Call GR model Fortan
  RESULTS <- .Fortran("frun_gr2m", PACKAGE = "airGR",
                      ## inputs
                      LInputs = LInputSeries,                             ### length of input and output series
                      InputsPrecip = InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/month]
                      InputsPE = InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/month]
                      NParam = as.integer(length(Param)),                 ### number of model parameter
                      Param = Param,                                      ### parameter set
                      NStates = as.integer(length(RunOptions$IniStates)), ### number of state variables used for model initialising
                      StateStart = RunOptions$IniStates,                  ### state variables used when the model run starts
                      NOutputs = as.integer(length(IndOutputs)),          ### number of output series
                      IndOutputs = IndOutputs,                            ### indices of output series
                      ## outputs
                      Outputs = matrix(as.double(-99e9), nrow = LInputSeries, ncol = length(IndOutputs)), ### output series [mm or mm/month]
                      StateEnd = rep(as.double(-99e9), length(RunOptions$IniStates))                      ### state variables at the end of the model run
  )
  RESULTS$Outputs[RESULTS$Outputs   <= -99e8] <- NA
  RESULTS$StateEnd[RESULTS$StateEnd <= -99e8] <- NA
  if (ExportStateEnd) {
    RESULTS$StateEnd <- CreateIniStates(FUN_MOD = RunModel_GR2M, InputsModel = InputsModel,
                                        ProdStore = RESULTS$StateEnd[1L], RoutStore = RESULTS$StateEnd[2L], ExpStore = NULL,
                                        UH1 = NULL,
                                        UH2 = NULL,
                                        GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                                        verbose = FALSE)
  }

  ## Output data preparation
  ## OutputsModel only
  if (!ExportDatesR & !ExportStateEnd) {
    OutputsModel <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i])
    names(OutputsModel) <- FortranOutputs[IndOutputs]
  }
  ## DatesR and OutputsModel only
  if (ExportDatesR & !ExportStateEnd) {
    OutputsModel <- c(list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                      lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i]))
    names(OutputsModel) <- c("DatesR", FortranOutputs[IndOutputs])
  }
  ## OutputsModel and StateEnd only
  if (!ExportDatesR & ExportStateEnd) {
    OutputsModel <- c(lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i]),
                      list(RESULTS$StateEnd))
    names(OutputsModel) <- c(FortranOutputs[IndOutputs], "StateEnd")
  }
  ## DatesR and OutputsModel and StateEnd
  if ((ExportDatesR & ExportStateEnd) | "all" %in% RunOptions$Outputs_Sim) {
    OutputsModel <- c(list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                       lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i]),
                       list(RESULTS$StateEnd))
    names(OutputsModel) <- c("DatesR", FortranOutputs[IndOutputs], "StateEnd")
  }

  ## End
  rm(RESULTS)
  class(OutputsModel) <- c("OutputsModel", "monthly", "GR")
  return(OutputsModel)

}

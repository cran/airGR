RunModel_GR2M <- function(InputsModel, RunOptions, Param) {

  .ArgumentsCheckGR(InputsModel, RunOptions, Param)

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
    IndOutputs <- as.integer(1:length(RunOptions$FortranOutputs$GR))
  } else {
    IndOutputs <- which(RunOptions$FortranOutputs$GR %in% RunOptions$Outputs_Sim)
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
  ## OutputsModel generation
  .GetOutputsModelGR(InputsModel,
                     RunOptions,
                     RESULTS,
                     LInputSeries,
                     Param)
}

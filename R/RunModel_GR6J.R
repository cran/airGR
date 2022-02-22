RunModel_GR6J <- function(InputsModel, RunOptions, Param) {

  .ArgumentsCheckGR(InputsModel, RunOptions, Param)

  Param <- as.double(Param)

  Param_X1X3X6_threshold <- 1e-2
  Param_X4_threshold     <- 0.5
  if (Param[1L] < Param_X1X3X6_threshold) {
    warning(sprintf("Param[1] (X1: production store capacity [mm]) < %.2f\n X1 set to %.2f", Param_X1X3X6_threshold, Param_X1X3X6_threshold))
    Param[1L] <- Param_X1X3X6_threshold
  }
  if (Param[3L] < Param_X1X3X6_threshold) {
    warning(sprintf("Param[3] (X3: routing store capacity [mm]) < %.2f\n X3 set to %.2f", Param_X1X3X6_threshold, Param_X1X3X6_threshold))
    Param[3L] <- Param_X1X3X6_threshold
  }
  if (Param[4L] < Param_X4_threshold) {
    warning(sprintf("Param[4] (X4: unit hydrograph time constant [d]) < %.2f\n X4 set to %.2f", Param_X4_threshold, Param_X4_threshold))
    Param[4L] <- Param_X4_threshold
  }
  if (Param[6L] < Param_X1X3X6_threshold) {
    warning(sprintf("Param[6] (X6: coefficient for emptying exponential store [mm]) < %.2f\n X6 set to %.2f", Param_X1X3X6_threshold, Param_X1X3X6_threshold))
    Param[6L] <- Param_X1X3X6_threshold
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
    RunOptions$IniStates[2] <- RunOptions$IniResLevels[2] * Param[3] ### routing store level (mm)
    RunOptions$IniStates[3] <- RunOptions$IniResLevels[3]          ### exponential store level (mm)
  }

  ## Call GR model Fortan
  RESULTS <- .Fortran("frun_gr6j", PACKAGE = "airGR",
                      ## inputs
                      LInputs = LInputSeries,                             ### length of input and output series
                      InputsPrecip = InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/d]
                      InputsPE = InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/d]
                      NParam = as.integer(length(Param)),                 ### number of model parameter
                      Param = Param,                                      ### parameter set
                      NStates = as.integer(length(RunOptions$IniStates)), ### number of state variables used for model initialising
                      StateStart = RunOptions$IniStates,                  ### state variables used when the model run starts
                      NOutputs = as.integer(length(IndOutputs)),          ### number of output series
                      IndOutputs = IndOutputs,                            ### indices of output series
                      ## outputs
                      Outputs = matrix(as.double(-99e9), nrow = LInputSeries, ncol = length(IndOutputs)), ### output series [mm or mm/d]
                      StateEnd = rep(as.double(-99e9), length(RunOptions$IniStates))                      ### state variables at the end of the model run
  )
  RESULTS$Outputs[RESULTS$Outputs   <= -99e8] <- NA
  RESULTS$StateEnd[RESULTS$StateEnd <= -99e8] <- NA
  if (ExportStateEnd) {
    RESULTS$StateEnd[-3L] <- ifelse(RESULTS$StateEnd[-3L] < 0, 0, RESULTS$StateEnd[-3L]) ### remove negative values except for the ExpStore location
    RESULTS$StateEnd <- CreateIniStates(FUN_MOD = RunModel_GR6J, InputsModel = InputsModel,
                                        ProdStore = RESULTS$StateEnd[1L], RoutStore = RESULTS$StateEnd[2L], ExpStore = RESULTS$StateEnd[3L],
                                        UH1 = RESULTS$StateEnd[(1:20) + 7],
                                        UH2 = RESULTS$StateEnd[(1:40) + (7+20)],
                                        GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                                        verbose = FALSE)
  }

  ## OutputsModel generation
  .GetOutputsModelGR(InputsModel,
                     RunOptions,
                     RESULTS,
                     LInputSeries,
                     Param)
}

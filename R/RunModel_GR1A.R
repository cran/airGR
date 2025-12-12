RunModel_GR1A <- function(InputsModel, RunOptions, Param) {

  .ArgumentsCheckGR(InputsModel, RunOptions, Param)

  Param <- as.double(Param)


  ## Input data preparation
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1 <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  LInputSeries <- length(IndPeriod1)
  if ("all" %in% RunOptions$Outputs_Sim) {
    IndOutputs <- as.integer(1:length(RunOptions$FortranOutputs$GR))
  } else {
    IndOutputs <- which(RunOptions$FortranOutputs$GR %in% RunOptions$Outputs_Sim)
  }


  ## Output data preparation
  IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries
  ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim
  ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim


  ## Call GR model Fortan
  RESULTS <- .Fortran("frun_gr1a", PACKAGE = "airGR",
                      ## inputs
                      LInputs = LInputSeries,                             ### length of input and output series
                      InputsPrecip = InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/y]
                      InputsPE = InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/y]
                      NParam = length(Param),                             ### number of model parameter
                      Param = Param,                                      ### parameter set
                      NStates = length(RunOptions$IniStates),             ### number of state variables used for model initialising
                      StateStart = RunOptions$IniStates,                  ### state variables used when the model run starts
                      NOutputs = length(IndOutputs),                      ### number of output series
                      IndOutputs = IndOutputs,                            ### indices of output series
                      ## outputs
                      Outputs = matrix(-99e9, nrow = LInputSeries, ncol = length(IndOutputs)), ### output series [mm/y]
                      StateEnd = rep(-99e9, length(RunOptions$IniStates))                      ### state variables at the end of the model run
  )

  ## OutputsModel generation
  .GetOutputsModelGR(InputsModel,
                     RunOptions,
                     RESULTS,
                     LInputSeries,
                     Param)
}

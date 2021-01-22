RunModel_GR1A <- function(InputsModel, RunOptions, Param) {
  
  
  ## Initialization of variables
  NParam <- 1
  FortranOutputs <- .FortranOutputs(GR = "GR1A")$GR
  
  
  ## Arguments check
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, "yearly")) {
    stop("'InputsModel' must be of class 'yearly'")
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
  
  
  ## Input data preparation
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1 <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  LInputSeries <- as.integer(length(IndPeriod1))
  if ("all" %in% RunOptions$Outputs_Sim) {
    IndOutputs <- as.integer(1:length(FortranOutputs))
  } else {
    IndOutputs <- which(FortranOutputs %in% RunOptions$Outputs_Sim)
  }
  
  
  ## Output data preparation
  IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp) + 1):LInputSeries
  ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim
  ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim
  
  
  ## Call GR model Fortan
  RESULTS <- .Fortran("frun_gr1a", PACKAGE = "airGR", 
                      ## inputs
                      LInputs = LInputSeries,                             ### length of input and output series
                      InputsPrecip = InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/y]
                      InputsPE = InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/y]
                      NParam = as.integer(length(Param)),                 ### number of model parameter
                      Param = Param,                                      ### parameter set
                      NStates = as.integer(length(RunOptions$IniStates)), ### number of state variables used for model initialising
                      StateStart = RunOptions$IniStates,                  ### state variables used when the model run starts
                      NOutputs = as.integer(length(IndOutputs)),          ### number of output series
                      IndOutputs = IndOutputs,                            ### indices of output series
                      ## outputs
                      Outputs = matrix(as.double(-999.999), nrow = LInputSeries, ncol=length(IndOutputs)), ### output series [mm]
                      StateEnd = rep(as.double(-999.999), length(RunOptions$IniStates))                   ### state variables at the end of the model run
  )
  RESULTS$Outputs[ round(RESULTS$Outputs , 3) == -999.999] <- NA
  RESULTS$StateEnd[round(RESULTS$StateEnd, 3) == -999.999] <- NA
  
  
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
  ## OutputsModel and SateEnd only
  if (!ExportDatesR & ExportStateEnd) {
    OutputsModel <- c(lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i]), 
                      list(RESULTS$StateEnd))
    names(OutputsModel) <- c(FortranOutputs[IndOutputs], "StateEnd")
  }
  ## DatesR and OutputsModel and SateEnd
  if ((ExportDatesR & ExportStateEnd) | "all" %in% RunOptions$Outputs_Sim) {
    OutputsModel <- c(list(InputsModel$DatesR[RunOptions$IndPeriod_Run]), 
                      lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i]), 
                      list(RESULTS$StateEnd))
    names(OutputsModel) <- c("DatesR", FortranOutputs[IndOutputs], "StateEnd")
  }
  
  
  ## End
  class(OutputsModel) <- c("OutputsModel", "yearly", "GR")
  return(OutputsModel)
  
}


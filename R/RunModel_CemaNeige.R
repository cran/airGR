RunModel_CemaNeige <- function(InputsModel, RunOptions, Param) {

  
  ## Initialization of variables
  IsHyst <- inherits(RunOptions, "hysteresis")
  NParamCN <- ifelse(test = IsHyst, yes = 4L, no = 2L)
  NStates <- 4L
  FortranOutputsCemaNeige <- .FortranOutputs(GR = NULL, isCN = TRUE)$CN
  
  
  ## Arguments check
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, "daily") & !inherits(InputsModel, "hourly")) {
    stop("'InputsModel' must be of class 'daily' or 'hourly'")
  }
  if (!inherits(InputsModel, "CemaNeige")) {
    stop("'InputsModel' must be of class 'CemaNeige'")
  }
  if (!inherits(RunOptions, "RunOptions")) {
    stop("'RunOptions' must be of class 'RunOptions'")
  }
  if (!inherits(RunOptions, "CemaNeige")) {
    stop("'RunOptions' must be of class 'CemaNeige'")
  }
  if (!is.vector(Param) | !is.numeric(Param)) {
    stop("'Param' must be a numeric vector")
  }
  if (sum(!is.na(Param)) != NParamCN) {
    stop(sprintf("'Param' must be a vector of length %i and contain no NA", NParamCN))
  }
  if (inherits(InputsModel, "daily")) {
    time_step <- "daily"
    time_mult <- 1
  }
  if (inherits(InputsModel, "hourly")) {
    time_step <- "hourly"
    time_mult <- 24
  }

  ## Input data preparation
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1     <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):length(IndPeriod1)
  ## Output data preparation
  ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim
  ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim
  
    
  ## CemaNeige________________________________________________________________________________
  ParamCemaNeige <- Param
  NLayers        <- length(InputsModel$LayerPrecip)
  
  if (sum(is.na(ParamCemaNeige)) != 0) {
    stop("Param contains missing values")
  }
  if ("all" %in% RunOptions$Outputs_Sim) {
    IndOutputsCemaNeige <- 1:length(FortranOutputsCemaNeige)
    
  } else {
    IndOutputsCemaNeige <- which(FortranOutputsCemaNeige %in% RunOptions$Outputs_Sim)
  }
  CemaNeigeLayers <- list()
  CemaNeigeStateEnd <- NULL
  NameCemaNeigeLayers <- "CemaNeigeLayers"
  
  
  ## Call CemaNeige Fortran_________________________
  for (iLayer in 1:NLayers) {
    
    if (!IsHyst) {
      StateStartCemaNeige <- RunOptions$IniStates[(7 + 20*time_mult + 40*time_mult) + c(iLayer, iLayer+NLayers)]
    } else {
      StateStartCemaNeige <- RunOptions$IniStates[(7 + 20*time_mult + 40*time_mult) + c(iLayer, iLayer+NLayers, iLayer+2*NLayers, iLayer+3*NLayers)]
    }
    RESULTS <- .Fortran("frun_cemaneige", PACKAGE = "airGR",
                        ## inputs
                        LInputs = as.integer(length(IndPeriod1)),                                       ### length of input and output series
                        InputsPrecip = InputsModel$LayerPrecip[[iLayer]][IndPeriod1],                   ### input series of total precipitation [mm/time step]
                        InputsFracSolidPrecip = InputsModel$LayerFracSolidPrecip[[iLayer]][IndPeriod1], ### input series of fraction of solid precipitation [0-1]
                        InputsTemp = InputsModel$LayerTemp[[iLayer]][IndPeriod1],                       ### input series of air mean temperature [degC]
                        MeanAnSolidPrecip = RunOptions$MeanAnSolidPrecip[iLayer],                       ### value of annual mean solid precip [mm/y]
                        NParam = as.integer(NParamCN),                                                  ### number of model parameters = 2 or 4
                        Param = as.double(ParamCemaNeige),                                              ### parameter set
                        NStates = as.integer(NStates),                                                  ### number of state variables used for model initialising = 4
                        StateStart = StateStartCemaNeige,                                               ### state variables used when the model run starts
                        IsHyst = as.integer(IsHyst),                                                    ### use of hysteresis
                        NOutputs = as.integer(length(IndOutputsCemaNeige)),                             ### number of output series
                        IndOutputs = IndOutputsCemaNeige,                                               ### indices of output series
                        ## outputs
                        Outputs = matrix(as.double(-99e9), nrow = length(IndPeriod1), ncol = length(IndOutputsCemaNeige)), ### output series [mm, mm/time step or degC]
                        StateEnd = rep(as.double(-99e9), NStates)                                                          ### state variables at the end of the model run
    )
    RESULTS$Outputs[RESULTS$Outputs   <= -99e8] <- NA
    RESULTS$StateEnd[RESULTS$StateEnd <= -99e8] <- NA
    
    ## Data storage
    CemaNeigeLayers[[iLayer]] <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i])
    names(CemaNeigeLayers[[iLayer]]) <- FortranOutputsCemaNeige[IndOutputsCemaNeige]
    if (ExportStateEnd) {
      CemaNeigeStateEnd <- c(CemaNeigeStateEnd, RESULTS$StateEnd)
    }
    rm(RESULTS)
  } ### ENDFOR iLayer
  
  names(CemaNeigeLayers) <- sprintf("Layer%02i", seq_len(NLayers))
  
  if (ExportStateEnd) { 
    idNStates <- seq_len(NStates*NLayers) %% NStates
    CemaNeigeStateEnd <- CreateIniStates(FUN_MOD = RunModel_CemaNeige, InputsModel = InputsModel, IsHyst = IsHyst,
                                         ProdStore = NULL, RoutStore = NULL, ExpStore = NULL,
                                         UH1 = NULL, UH2 = NULL,
                                         GCemaNeigeLayers       = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 1]],
                                         eTGCemaNeigeLayers     = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 2]],
                                         GthrCemaNeigeLayers    = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 3]], 
                                         GlocmaxCemaNeigeLayers = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 0]],
                                         verbose = FALSE)
  }
  
  ## Output data preparation
  ## OutputsModel only
  if (!ExportDatesR & !ExportStateEnd) {
    OutputsModel <- list(CemaNeigeLayers)
    names(OutputsModel) <- NameCemaNeigeLayers
  }
  ## DatesR and OutputsModel only
  if (ExportDatesR & !ExportStateEnd) {
    OutputsModel <- c(list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                      list(CemaNeigeLayers))
    names(OutputsModel) <- c("DatesR", NameCemaNeigeLayers)
  }
  ## OutputsModel and StateEnd only
  if (!ExportDatesR & ExportStateEnd) {
    OutputsModel <- c(list(CemaNeigeLayers),
                      list(CemaNeigeStateEnd))
    names(OutputsModel) <- c(NameCemaNeigeLayers, "StateEnd")
  }
  ## DatesR and OutputsModel and StateEnd
  if (ExportDatesR & ExportStateEnd) {
    OutputsModel <- c(list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                      list(CemaNeigeLayers),
                      list(CemaNeigeStateEnd))
    names(OutputsModel) <- c("DatesR", NameCemaNeigeLayers, "StateEnd")
  }
  
  ## End
  class(OutputsModel) <- c("OutputsModel", time_step, "CemaNeige")
  if (IsHyst) {
    class(OutputsModel) <- c(class(OutputsModel), "hysteresis")
  }
  return(OutputsModel)

}

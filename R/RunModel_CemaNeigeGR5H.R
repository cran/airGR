RunModel_CemaNeigeGR5H <- function(InputsModel, RunOptions, Param) {


  ## Initialization of variables
  IsHyst <- inherits(RunOptions, "hysteresis")
  NParamCN <- RunOptions$FeatFUN_MOD$NbParam - 5L
  NStates <- 4L
  IsIntStore <- inherits(RunOptions, "interception")
  if (IsIntStore) {
    Imax <- RunOptions$Imax
  } else {
    Imax <- -99
  }

  .ArgumentsCheckGR(InputsModel, RunOptions, Param)

  Param <- as.double(Param)


  Param_X1X3_threshold <- 1e-2
  Param_X4_threshold   <- 0.5
  if (Param[1L] < Param_X1X3_threshold) {
    warning(sprintf("Param[1] (X1: production store capacity [mm]) < %.2f\n X1 set to %.2f", Param_X1X3_threshold, Param_X1X3_threshold))
    Param[1L] <- Param_X1X3_threshold
  }
  if (Param[3L] < Param_X1X3_threshold) {
    warning(sprintf("Param[3] (X3: routing store capacity [mm]) < %.2f\n X3 set to %.2f", Param_X1X3_threshold, Param_X1X3_threshold))
    Param[3L] <- Param_X1X3_threshold
  }
  if (Param[4L] < Param_X4_threshold) {
    warning(sprintf("Param[4] (X4: unit hydrograph time constant [h]) < %.2f\n X4 set to %.2f", Param_X4_threshold, Param_X4_threshold))
    Param[4L] <- Param_X4_threshold
  }

  ## Input data preparation
  if (identical(RunOptions$IndPeriod_WarmUp, 0L)) {
    RunOptions$IndPeriod_WarmUp <- NULL
  }
  IndPeriod1     <- c(RunOptions$IndPeriod_WarmUp, RunOptions$IndPeriod_Run)
  LInputSeries   <- as.integer(length(IndPeriod1))
  IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries
  ParamCemaNeige <- Param[(length(Param) - 1 - 2 * as.integer(IsHyst)):length(Param)]
  NParamMod      <- as.integer(length(Param) - (2 + 2 * as.integer(IsHyst)))
  ParamMod       <- Param[1:NParamMod]
  NLayers        <- length(InputsModel$LayerPrecip)
  NStatesMod     <- as.integer(length(RunOptions$IniStates) - NStates * NLayers)

  ## Output data preparation
  ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim
  ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim


  ## CemaNeige________________________________________________________________________________
  if (inherits(RunOptions, "CemaNeige")) {
    if ("all" %in% RunOptions$Outputs_Sim) {
      IndOutputsCemaNeige <- as.integer(1:length(RunOptions$FortranOutputs$CN))
    } else {
      IndOutputsCemaNeige <- which(RunOptions$FortranOutputs$CN %in% RunOptions$Outputs_Sim)
    }
    CemaNeigeLayers <- list()
    CemaNeigeStateEnd <- NULL
    NameCemaNeigeLayers <- "CemaNeigeLayers"


    ## Call CemaNeige Fortran_________________________
    for (iLayer in 1:NLayers) {

      if (!IsHyst) {
        StateStartCemaNeige <- RunOptions$IniStates[(7 + 20*24 + 40*24) + c(iLayer, iLayer+NLayers)]
      } else {
        StateStartCemaNeige <- RunOptions$IniStates[(7 + 20*24 + 40*24) + c(iLayer, iLayer+NLayers, iLayer+2*NLayers, iLayer+3*NLayers)]
      }
      RESULTS <- .Fortran("frun_cemaneige", PACKAGE = "airGR",
                          ## inputs
                          LInputs = LInputSeries,                                                         ### length of input and output series
                          InputsPrecip = InputsModel$LayerPrecip[[iLayer]][IndPeriod1],                   ### input series of total precipitation [mm/h]
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
                          Outputs = matrix(as.double(-99e9), nrow = LInputSeries, ncol = length(IndOutputsCemaNeige)), ### output series [mm, mm/h or degC]
                          StateEnd = rep(as.double(-99e9), as.integer(NStates))                                        ### state variables at the end of the model run
      )
      RESULTS$Outputs[RESULTS$Outputs   <= -99e8] <- NA
      RESULTS$StateEnd[RESULTS$StateEnd <= -99e8] <- NA

      ## Data storage
      CemaNeigeLayers[[iLayer]] <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2, i])
      names(CemaNeigeLayers[[iLayer]]) <- RunOptions$FortranOutputs$CN[IndOutputsCemaNeige]
      IndPliqAndMelt <- which(names(CemaNeigeLayers[[iLayer]]) == "PliqAndMelt")
      if (iLayer == 1) {
        CatchMeltAndPliq <- RESULTS$Outputs[, IndPliqAndMelt] / NLayers
      }
      if (iLayer > 1) {
        CatchMeltAndPliq <- CatchMeltAndPliq + RESULTS$Outputs[, IndPliqAndMelt] / NLayers
      }
      if (ExportStateEnd) {
        CemaNeigeStateEnd <- c(CemaNeigeStateEnd, RESULTS$StateEnd)
      }
      rm(RESULTS)
    } ### ENDFOR iLayer
    names(CemaNeigeLayers) <- sprintf("Layer%02i", seq_len(NLayers))
  } ### ENDIF RunSnowModule
  if (!inherits(RunOptions, "CemaNeige")) {
    CemaNeigeLayers <- list()
    CemaNeigeStateEnd <- NULL
    NameCemaNeigeLayers <- NULL
    CatchMeltAndPliq <- InputsModel$Precip[IndPeriod1]
  }



  ## GR model
  if ("all" %in% RunOptions$Outputs_Sim) {
    IndOutputsMod <- as.integer(1:length(RunOptions$FortranOutputs$GR))
  } else {
    IndOutputsMod <- which(RunOptions$FortranOutputs$GR %in% RunOptions$Outputs_Sim)
  }

  ## Use of IniResLevels
  if (!is.null(RunOptions$IniResLevels)) {
    RunOptions$IniStates[1] <- RunOptions$IniResLevels[1] * ParamMod[1] ### production store level (mm)
    RunOptions$IniStates[2] <- RunOptions$IniResLevels[2] * ParamMod[3] ### routing store level (mm)
    if (IsIntStore) {
      RunOptions$IniStates[4] <- RunOptions$IniResLevels[4] * Imax ### interception store level (mm)
    }
  }

  ## Call GR model Fortan
  RESULTS <- .Fortran("frun_gr5h", PACKAGE = "airGR",
                      ## inputs
                      LInputs = LInputSeries,                          ### length of input and output series
                      InputsPrecip = CatchMeltAndPliq,                 ### input series of total precipitation [mm/h]
                      InputsPE = InputsModel$PotEvap[IndPeriod1],      ### input series potential evapotranspiration [mm/h]
                      NParam = NParamMod,                              ### number of model parameter
                      Param = ParamMod,                                ### parameter set
                      NStates = NStatesMod,                            ### number of state variables used for model initialising
                      StateStart = RunOptions$IniStates[1:NStatesMod], ### state variables used when the model run starts
                      Imax = Imax,                                     ### maximal capacity of interception store
                      NOutputs = as.integer(length(IndOutputsMod)),    ### number of output series
                      IndOutputs = IndOutputsMod,                      ### indices of output series
                      ## outputs
                      Outputs = matrix(as.double(-99e9), nrow = LInputSeries, ncol = length(IndOutputsMod)), ### output series [mm or mm/h]
                      StateEnd = rep(as.double(-99e9), NStatesMod)                                           ### state variables at the end of the model run
  )
  RESULTS$Outputs[RESULTS$Outputs   <= -99e8] <- NA
  RESULTS$StateEnd[RESULTS$StateEnd <= -99e8] <- NA
  if (ExportStateEnd) {
    RESULTS$StateEnd[-3L] <- ifelse(RESULTS$StateEnd[-3L] < 0, 0, RESULTS$StateEnd[-3L]) ### remove negative values except for the ExpStore location
    idNStates <- seq_len(NStates*NLayers) %% NStates
    RESULTS$StateEnd <- CreateIniStates(FUN_MOD = RunModel_CemaNeigeGR5H, InputsModel = InputsModel, IsHyst = IsHyst,
                                        ProdStore = RESULTS$StateEnd[1L], RoutStore = RESULTS$StateEnd[2L], ExpStore = NULL,
                                        IntStore = RESULTS$StateEnd[4L],
                                        UH1 = NULL,
                                        UH2 = RESULTS$StateEnd[(1:(40*24)) + (7+20*24)],
                                        GCemaNeigeLayers       = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 1]],
                                        eTGCemaNeigeLayers     = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 2]],
                                        GthrCemaNeigeLayers    = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 3]],
                                        GlocmaxCemaNeigeLayers = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 0]],
                                        verbose = FALSE)
  }

  if (inherits(RunOptions, "CemaNeige") & "Precip" %in% RunOptions$Outputs_Sim) {
    RESULTS$Outputs[, which(RunOptions$FortranOutputs$GR[IndOutputsMod] == "Precip")] <-
      InputsModel$Precip[IndPeriod1]
  }

  ## OutputsModel generation
  .GetOutputsModelGR(InputsModel,
                     RunOptions,
                     RESULTS,
                     LInputSeries,
                     Param,
                     CemaNeigeLayers)
}

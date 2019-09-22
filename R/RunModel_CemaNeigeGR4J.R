RunModel_CemaNeigeGR4J <- function(InputsModel,RunOptions,Param){

  
  ## Initialization of variables
  IsHyst <- inherits(RunOptions, "hysteresis")
  NParam <- ifelse(test = IsHyst, yes = 8L, no = 6L)
  NParamCN <- NParam - 4L
  NStates <- 4L
  FortranOutputs <- .FortranOutputs(GR = "GR4J", isCN = TRUE)

    
    ##Arguments_check
      if(!inherits(InputsModel,"InputsModel")){ stop("'InputsModel' must be of class 'InputsModel'") }  
      if(!inherits(InputsModel,"daily"      )){ stop("'InputsModel' must be of class 'daily'      ") }  
      if(!inherits(InputsModel,"GR"         )){ stop("'InputsModel' must be of class 'GR'         ") }  
      if(!inherits(InputsModel,"CemaNeige"  )){ stop("'InputsModel' must be of class 'CemaNeige'  ") }  
      if(!inherits(RunOptions,"RunOptions"  )){ stop("'RunOptions' must be of class 'RunOptions'  ") }  
      if(!inherits(RunOptions,"GR"          )){ stop("'RunOptions' must be of class 'GR'          ") }  
      if(!inherits(RunOptions,"CemaNeige"   )){ stop("'RunOptions' must be of class 'CemaNeige'   ") }  
      if(!is.vector(Param) | !is.numeric(Param)){ stop("'Param' must be a numeric vector") }
      if(sum(!is.na(Param))!=NParam){ stop(paste("'Param' must be a vector of length ",NParam," and contain no NA",sep="")) }
      Param <- as.double(Param);

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
        warning(sprintf("Param[4] (X4: unit hydrograph time constant [d]) < %.2f\n X4 set to %.2f", Param_X4_threshold, Param_X4_threshold))
        Param[4L] <- Param_X4_threshold
      }      
      
    ##Input_data_preparation
      if(identical(RunOptions$IndPeriod_WarmUp,as.integer(0))){ RunOptions$IndPeriod_WarmUp <- NULL; }
      IndPeriod1     <- c(RunOptions$IndPeriod_WarmUp,RunOptions$IndPeriod_Run);
      LInputSeries   <- as.integer(length(IndPeriod1))
      IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries;
      ParamCemaNeige <- Param[(length(Param)-1-2*as.integer(IsHyst)):length(Param)];
      NParamMod      <- as.integer(length(Param)-(2+2*as.integer(IsHyst)));
      ParamMod       <- Param[1:NParamMod];
      NLayers        <- length(InputsModel$LayerPrecip);
      NStatesMod     <- as.integer(length(RunOptions$IniStates)-NStates*NLayers);
      ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim;
      ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim;

      
    ##SNOW_MODULE________________________________________________________________________________##
    if(inherits(RunOptions,"CemaNeige")){
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputsCemaNeige <- as.integer(1:length(FortranOutputs$CN)); 
      } else { IndOutputsCemaNeige <- which(FortranOutputs$CN %in% RunOptions$Outputs_Sim);  }
      CemaNeigeLayers <- list(); CemaNeigeStateEnd <- NULL; NameCemaNeigeLayers <- "CemaNeigeLayers";


    ##Call_DLL_CemaNeige_________________________
      for(iLayer in 1:NLayers){
        if (!IsHyst) {
          StateStartCemaNeige <- RunOptions$IniStates[(7 + 20 + 40) + c(iLayer, iLayer+NLayers)]
        } else {
          StateStartCemaNeige <- RunOptions$IniStates[(7 + 20 + 40) + c(iLayer, iLayer+NLayers, iLayer+2*NLayers, iLayer+3*NLayers)]
        }
        RESULTS <- .Fortran("frun_cemaneige",PACKAGE="airGR",
                        ##inputs
                            LInputs=LInputSeries,                                                         ### length of input and output series
                            InputsPrecip=InputsModel$LayerPrecip[[iLayer]][IndPeriod1],                   ### input series of total precipitation [mm/d]
                            InputsFracSolidPrecip=InputsModel$LayerFracSolidPrecip[[iLayer]][IndPeriod1], ### input series of fraction of solid precipitation [0-1]
                            InputsTemp=InputsModel$LayerTemp[[iLayer]][IndPeriod1],                       ### input series of air mean temperature [degC]
                            MeanAnSolidPrecip=RunOptions$MeanAnSolidPrecip[iLayer],                       ### value of annual mean solid precip [mm/y]
                            NParam=as.integer(NParamCN),                                                  ### number of model parameters = 2 or 4
                            Param=as.double(ParamCemaNeige),                                              ### parameter set
                            NStates=as.integer(NStates),                                                  ### number of state variables used for model initialising = 4
                            StateStart=StateStartCemaNeige,                                               ### state variables used when the model run starts
                            IsHyst = as.integer(IsHyst),                                                  ### use of hysteresis
                            NOutputs=as.integer(length(IndOutputsCemaNeige)),                             ### number of output series
                            IndOutputs=IndOutputsCemaNeige,                                               ### indices of output series
                        ##outputs                                                               
                            Outputs=matrix(as.double(-999.999),nrow=LInputSeries,ncol=length(IndOutputsCemaNeige)), ### output series [mm]
                            StateEnd=rep(as.double(-999.999),as.integer(NStates))                                   ### state variables at the end of the model run
                         )
        RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
        RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;

        ##Data_storage
        CemaNeigeLayers[[iLayer]] <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]);
        names(CemaNeigeLayers[[iLayer]]) <- FortranOutputs$CN[IndOutputsCemaNeige];
        IndPliqAndMelt <- which(names(CemaNeigeLayers[[iLayer]]) == "PliqAndMelt");
        if(iLayer==1){ CatchMeltAndPliq <- RESULTS$Outputs[,IndPliqAndMelt]/NLayers; }
        if(iLayer >1){ CatchMeltAndPliq <- CatchMeltAndPliq + RESULTS$Outputs[,IndPliqAndMelt]/NLayers; }
        if(ExportStateEnd){ CemaNeigeStateEnd <- c(CemaNeigeStateEnd,RESULTS$StateEnd); }
        rm(RESULTS); 
      } ###ENDFOR_iLayer
      names(CemaNeigeLayers) <- sprintf("Layer%02i", seq_len(NLayers))
    } ###ENDIF_RunSnowModule
    if(!inherits(RunOptions,"CemaNeige")){
      CemaNeigeLayers <- list(); CemaNeigeStateEnd <- NULL; NameCemaNeigeLayers <- NULL;
      CatchMeltAndPliq  <- InputsModel$Precip[IndPeriod1]; }



    ##MODEL______________________________________________________________________________________##
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputsMod <- as.integer(1:length(FortranOutputs$GR)); 
      } else { IndOutputsMod <- which(FortranOutputs$GR %in% RunOptions$Outputs_Sim);  }

    ##Use_of_IniResLevels
      if(!is.null(RunOptions$IniResLevels)){
        RunOptions$IniStates[1] <- RunOptions$IniResLevels[1]*ParamMod[1];  ### production store level (mm)
        RunOptions$IniStates[2] <- RunOptions$IniResLevels[2]*ParamMod[3];  ### routing store level (mm)
      }

    ##Call_fortan
      RESULTS <- .Fortran("frun_gr4j",PACKAGE="airGR",
                 ##inputs
                     LInputs=LInputSeries,                          ### length of input and output series
                     InputsPrecip=CatchMeltAndPliq,                 ### input series of total precipitation [mm/d]
                     InputsPE=InputsModel$PotEvap[IndPeriod1],      ### input series potential evapotranspiration [mm/d]
                     NParam=NParamMod,                              ### number of model parameter
                     Param=ParamMod,                                ### parameter set
                     NStates=NStatesMod,                            ### number of state variables used for model initialising
                     StateStart=RunOptions$IniStates[1:NStatesMod], ### state variables used when the model run starts
                     NOutputs=as.integer(length(IndOutputsMod)),    ### number of output series
                     IndOutputs=IndOutputsMod,                      ### indices of output series
                 ##outputs                                        
                     Outputs=matrix(as.double(-999.999),nrow=LInputSeries,ncol=length(IndOutputsMod)), ### output series [mm]
                     StateEnd=rep(as.double(-999.999),NStatesMod)                                      ### state variables at the end of the model run
                     )
      RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
      RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;
      if (ExportStateEnd) {
        idNStates <- seq_len(NStates*NLayers) %% NStates
        RESULTS$StateEnd <- CreateIniStates(FUN_MOD = RunModel_CemaNeigeGR4J, InputsModel = InputsModel, IsHyst = IsHyst,
                                            ProdStore = RESULTS$StateEnd[1L], RoutStore = RESULTS$StateEnd[2L], ExpStore = NULL,
                                            UH1 = RESULTS$StateEnd[(1:20)+7], UH2 = RESULTS$StateEnd[(1:40)+(7+20)],
                                            GCemaNeigeLayers       = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 1]],
                                            eTGCemaNeigeLayers     = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 2]],
                                            GthrCemaNeigeLayers    = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 3]], 
                                            GlocmaxCemaNeigeLayers = CemaNeigeStateEnd[seq_len(NStates*NLayers)[idNStates == 0]],
                                            verbose = FALSE)
      }
      
      if(inherits(RunOptions,"CemaNeige") & "Precip" %in% RunOptions$Outputs_Sim){ RESULTS$Outputs[,which(FortranOutputs$GR[IndOutputsMod]=="Precip")] <- InputsModel$Precip[IndPeriod1]; }

    ##Output_data_preparation
      ##OutputsModel_only
      if(!ExportDatesR & !ExportStateEnd){
        OutputsModel <- c( lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers) );
        names(OutputsModel) <- c(FortranOutputs$GR[IndOutputsMod],NameCemaNeigeLayers); }
      ##DatesR_and_OutputsModel_only
      if( ExportDatesR & !ExportStateEnd){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers) );
        names(OutputsModel) <- c("DatesR",FortranOutputs$GR[IndOutputsMod],NameCemaNeigeLayers);      }
      ##OutputsModel_and_SateEnd_only
      if(!ExportDatesR & ExportStateEnd){
        OutputsModel <- c( lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers),
                           list(RESULTS$StateEnd) );
        names(OutputsModel) <- c(FortranOutputs$GR[IndOutputsMod],NameCemaNeigeLayers,"StateEnd");      }
      ##DatesR_and_OutputsModel_and_SateEnd
      if( ExportDatesR & ExportStateEnd){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers),
                           list(RESULTS$StateEnd) );
        names(OutputsModel) <- c("DatesR",FortranOutputs$GR[IndOutputsMod],NameCemaNeigeLayers,"StateEnd");      }

    ##End
      rm(RESULTS); 
   
      class(OutputsModel) <- c("OutputsModel","daily","GR","CemaNeige");
      if(IsHyst) {
        class(OutputsModel) <- c(class(OutputsModel), "hysteresis")
      }
      return(OutputsModel);

}

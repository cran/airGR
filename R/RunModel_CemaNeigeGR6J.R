RunModel_CemaNeigeGR6J <- function(InputsModel,RunOptions,Param){

    NParam <- 8;
    FortranOutputsCemaNeige <- c("Pliq","Psol","SnowPack","ThermalState","Gratio","PotMelt","Melt","PliqAndMelt", "Temp");
    FortranOutputsMod       <- c("PotEvap","Precip","Prod","AE","Perc","PR","Q9","Q1","Rout","Exch","AExch","QR","QR1","Exp","QD","Qsim");

    ##Arguments_check
      if(inherits(InputsModel,"InputsModel")==FALSE){ stop("InputsModel must be of class 'InputsModel' \n"); return(NULL); }  
      if(inherits(InputsModel,"daily"      )==FALSE){ stop("InputsModel must be of class 'daily'       \n"); return(NULL); }  
      if(inherits(InputsModel,"GR"         )==FALSE){ stop("InputsModel must be of class 'GR'          \n"); return(NULL); }  
      if(inherits(InputsModel,"CemaNeige"  )==FALSE){ stop("InputsModel must be of class 'CemaNeige'   \n"); return(NULL); }  
      if(inherits(RunOptions,"RunOptions"  )==FALSE){ stop("RunOptions must be of class 'RunOptions'   \n"); return(NULL); }  
      if(inherits(RunOptions,"GR"          )==FALSE){ stop("RunOptions must be of class 'GR'           \n"); return(NULL); }  
      if(inherits(RunOptions,"CemaNeige"   )==FALSE){ stop("RunOptions must be of class 'CemaNeige'    \n"); return(NULL); }  
      if(!is.vector(Param)){ stop("Param must be a vector \n"); return(NULL); }
      if(sum(!is.na(Param))!=NParam){ stop(paste("Param must be a vector of length ",NParam," and contain no NA \n",sep="")); return(NULL); }
      Param <- as.double(Param);

    ##Input_data_preparation
      if(identical(RunOptions$IndPeriod_WarmUp,as.integer(0))){ RunOptions$IndPeriod_WarmUp <- NULL; }
      IndPeriod1     <- c(RunOptions$IndPeriod_WarmUp,RunOptions$IndPeriod_Run);
      LInputSeries   <- as.integer(length(IndPeriod1))
      IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries;
      ParamCemaNeige <- Param[(length(Param)-1):length(Param)];
      NParamMod      <- as.integer(length(Param)-2);
      ParamMod       <- Param[1:NParamMod];
      NLayers        <- length(InputsModel$LayerPrecip);
      NStatesMod     <- as.integer(length(RunOptions$IniStates)-2*NLayers);
      ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim;
      ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim;



    ##SNOW_MODULE________________________________________________________________________________##
    if(RunOptions$RunSnowModule==TRUE){
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputsCemaNeige <- as.integer(1:length(FortranOutputsCemaNeige)); 
      } else { IndOutputsCemaNeige <- which(FortranOutputsCemaNeige %in% RunOptions$Outputs_Sim);  }
      CemaNeigeLayers <- list(); CemaNeigeStateEnd <- NULL; NameCemaNeigeLayers <- "CemaNeigeLayers";

    ##Call_DLL_CemaNeige_________________________
      for(iLayer in 1:NLayers){
        StateStartCemaNeige <- RunOptions$IniStates[ (NStatesMod+2*(iLayer-1)+1):(NStatesMod+2*(iLayer-1)+2) ];
        RESULTS <- .Fortran("frun_CemaNeige",PACKAGE="airGR",
                        ##inputs
                            LInputs=LInputSeries,                                                          ### length of input and output series
                            InputsPrecip=InputsModel$LayerPrecip[[iLayer]][IndPeriod1],                    ### input series of total precipitation [mm/d]
                            InputsFracSolidPrecip=InputsModel$LayerFracSolidPrecip[[iLayer]][IndPeriod1],  ### input series of fraction of solid precipitation [0-1]
                            InputsTemp=InputsModel$LayerTemp[[iLayer]][IndPeriod1],                        ### input series of air mean temperature [degC]
                            MeanAnSolidPrecip=RunOptions$MeanAnSolidPrecip[iLayer],                        ### value of annual mean solid precip [mm/y]
                            NParam=as.integer(2),                                                          ### number of model parameter = 2
                            Param=ParamCemaNeige,                                                          ### parameter set
                            NStates=as.integer(2),                                                         ### number of state variables used for model initialising = 2
                            StateStart=StateStartCemaNeige,                                                ### state variables used when the model run starts
                            NOutputs=as.integer(length(IndOutputsCemaNeige)),                              ### number of output series
                            IndOutputs=IndOutputsCemaNeige,                                                ### indices of output series
                        ##outputs                                                               
                            Outputs=matrix(as.double(-999.999),nrow=LInputSeries,ncol=length(IndOutputsCemaNeige)),  ### output series [mm]
                            StateEnd=rep(as.double(-999.999),as.integer(2))                                          ### state variables at the end of the model run (reservoir levels [mm] and HU)
                         )
        RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
        RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;

        ##Data_storage
        CemaNeigeLayers[[iLayer]] <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]);
        names(CemaNeigeLayers[[iLayer]]) <- FortranOutputsCemaNeige[IndOutputsCemaNeige];
        IndPliqAndMelt <- which(names(CemaNeigeLayers[[iLayer]]) == "PliqAndMelt");
        if(iLayer==1){ CatchMeltAndPliq <- RESULTS$Outputs[,IndPliqAndMelt]/NLayers; }
        if(iLayer >1){ CatchMeltAndPliq <- CatchMeltAndPliq + RESULTS$Outputs[,IndPliqAndMelt]/NLayers; }
        if(ExportStateEnd){ CemaNeigeStateEnd <- c(CemaNeigeStateEnd,RESULTS$StateEnd); }
        rm(RESULTS); 
      } ###ENDFOR_iLayer
      names(CemaNeigeLayers) <- paste("Layer",formatC(1:NLayers,width=2,flag="0"),sep="");
    } ###ENDIF_RunSnowModule
    if(RunOptions$RunSnowModule==FALSE){
      CemaNeigeLayers <- list(); CemaNeigeStateEnd <- NULL; NameCemaNeigeLayers <- NULL;
      CatchMeltAndPliq  <- InputsModel$Precip[IndPeriod1]; }



    ##MODEL______________________________________________________________________________________##
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputsMod <- as.integer(1:length(FortranOutputsMod)); 
      } else { IndOutputsMod <- which(FortranOutputsMod %in% RunOptions$Outputs_Sim);  }

    ##Use_of_IniResLevels
      if("IniResLevels" %in% RunOptions){
        RunOptions$IniStates[1] <- RunOptions$IniResLevels[1]*ParamMod[1];  ### production store level (mm)
        RunOptions$IniStates[2] <- RunOptions$IniResLevels[2]*ParamMod[3];  ### routing store level (mm)
      }

    ##Call_fortan
      RESULTS <- .Fortran("frun_GR6J",PACKAGE="airGR",
                 ##inputs
                     LInputs=LInputSeries,                         ### length of input and output series
                     InputsPrecip=CatchMeltAndPliq,                ### input series of total precipitation [mm/d]
                     InputsPE=InputsModel$PotEvap[IndPeriod1],     ### input series potential evapotranspiration [mm/d]
                     NParam=NParamMod,                             ### number of model parameter
                     Param=ParamMod,                               ### parameter set
                     NStates=NStatesMod,                           ### number of state variables used for model initialising
                     StateStart=RunOptions$IniStates[1:NStatesMod], ### state variables used when the model run starts
                     NOutputs=as.integer(length(IndOutputsMod)),   ### number of output series
                     IndOutputs=IndOutputsMod,                     ### indices of output series
                 ##outputs                                        
                     Outputs=matrix(as.double(-999.999),nrow=LInputSeries,ncol=length(IndOutputsMod)),  ### output series [mm]
                     StateEnd=rep(as.double(-999.999),NStatesMod)                     ### state variables at the end of the model run
                     )
      RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
      RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;
      if(RunOptions$RunSnowModule & "Precip" %in% RunOptions$Outputs_Sim){ RESULTS$Outputs[,which(FortranOutputsMod[IndOutputsMod]=="Precip")] <- InputsModel$Precip[IndPeriod1]; }

    ##Output_data_preparation
      ##OutputsModel_only
      if(ExportDatesR==FALSE & ExportStateEnd==FALSE){
        OutputsModel <- c( lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers) );
        names(OutputsModel) <- c(FortranOutputsMod[IndOutputsMod],NameCemaNeigeLayers); }
      ##DatesR_and_OutputsModel_only
      if(ExportDatesR==TRUE & ExportStateEnd==FALSE){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers) );
        names(OutputsModel) <- c("DatesR",FortranOutputsMod[IndOutputsMod],NameCemaNeigeLayers);      }
      ##OutputsModel_and_SateEnd_only
      if(ExportDatesR==FALSE & ExportStateEnd==TRUE){
        OutputsModel <- c( lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers),
                           list(c(RESULTS$StateEnd,CemaNeigeStateEnd)) );
        names(OutputsModel) <- c(FortranOutputsMod[IndOutputsMod],NameCemaNeigeLayers,"StateEnd");      }
      ##DatesR_and_OutputsModel_and_SateEnd
      if(ExportDatesR==TRUE & ExportStateEnd==TRUE){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(CemaNeigeLayers),
                           list(c(RESULTS$StateEnd,CemaNeigeStateEnd)) );
        names(OutputsModel) <- c("DatesR",FortranOutputsMod[IndOutputsMod],NameCemaNeigeLayers,"StateEnd");      }

    ##End
      rm(RESULTS); 
      class(OutputsModel) <- c("OutputsModel","daily","GR","CemaNeige");
      return(OutputsModel);

}


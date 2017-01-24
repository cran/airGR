RunModel_CemaNeige <- function(InputsModel,RunOptions,Param){

    NParam <- 2;
    FortranOutputsCemaNeige <- c("Pliq","Psol","SnowPack","ThermalState","Gratio","PotMelt","Melt","PliqAndMelt", "Temp");

    ##Arguments_check
      if(inherits(InputsModel,"InputsModel")==FALSE){ stop("InputsModel must be of class 'InputsModel' \n"); return(NULL); }  
      if(inherits(InputsModel,"daily"      )==FALSE){ stop("InputsModel must be of class 'daily'       \n"); return(NULL); }  
      if(inherits(InputsModel,"CemaNeige"  )==FALSE){ stop("InputsModel must be of class 'CemaNeige'   \n"); return(NULL); }  
      if(inherits(RunOptions,"RunOptions"  )==FALSE){ stop("RunOptions must be of class 'RunOptions'   \n"); return(NULL); }  
      if(inherits(RunOptions,"CemaNeige"   )==FALSE){ stop("RunOptions must be of class 'CemaNeige'    \n"); return(NULL); }  
      if(!is.vector(Param)){ stop("Param must be a vector \n"); return(NULL); }
      if(sum(!is.na(Param))!=NParam){ stop(paste("Param must be a vector of length ",NParam," and contain no NA \n",sep="")); return(NULL); }
      Param <- as.double(Param);

    ##Input_data_preparation
      if(identical(RunOptions$IndPeriod_WarmUp,0)){ RunOptions$IndPeriod_WarmUp <- NULL; }
      IndPeriod1    <- c(RunOptions$IndPeriod_WarmUp,RunOptions$IndPeriod_Run);
      IndPeriod2    <- (length(RunOptions$IndPeriod_WarmUp)+1):length(IndPeriod1);
      ExportDatesR    <- "DatesR"   %in% RunOptions$Outputs_Sim;
      ExportStateEnd  <- "StateEnd" %in% RunOptions$Outputs_Sim;



    ##SNOW_MODULE________________________________________________________________________________##
      ParamCemaNeige  <- Param;
      NLayers         <- length(InputsModel$LayerPrecip);
      if(sum(is.na(ParamCemaNeige))!=0){ stop("Param contains missing values \n"); return(NULL); }
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputsCemaNeige <- as.integer(1:length(FortranOutputsCemaNeige)); 
      } else { IndOutputsCemaNeige <- which(FortranOutputsCemaNeige %in% RunOptions$Outputs_Sim);  }
      CemaNeigeLayers <- list(); CemaNeigeStateEnd <- NULL; NameCemaNeigeLayers <- "CemaNeigeLayers";

    ##Call_DLL_CemaNeige_________________________
      for(iLayer in 1:NLayers){
        StateStartCemaNeige <- RunOptions$IniStates[ (2*(iLayer-1)+1):(2*(iLayer-1)+2) ];
        RESULTS <- .Fortran("frun_CemaNeige",PACKAGE="airGR",
                        ##inputs
                            LInputs=as.integer(length(IndPeriod1)),                                        ### length of input and output series
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
                            Outputs=matrix(as.double(-999.999),nrow=length(IndPeriod1),ncol=length(IndOutputsCemaNeige)),  ### output series [mm]
                            StateEnd=rep(as.double(-999.999),as.integer(2))                                                ### state variables at the end of the model run (reservoir levels [mm] and HU)
                         )
        RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
        RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;

        ##Data_storage
        CemaNeigeLayers[[iLayer]] <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]);
        names(CemaNeigeLayers[[iLayer]]) <- FortranOutputsCemaNeige[IndOutputsCemaNeige];
        if(ExportStateEnd){ CemaNeigeStateEnd <- c(CemaNeigeStateEnd,RESULTS$StateEnd); }
        rm(RESULTS); 
      } ###ENDFOR_iLayer
      names(CemaNeigeLayers) <- paste("Layer",formatC(1:NLayers,width=2,flag="0"),sep="");

      ##Output_data_preparation
      if(ExportDatesR==FALSE & ExportStateEnd==FALSE){
        OutputsModel <- list(CemaNeigeLayers);
        names(OutputsModel) <- NameCemaNeigeLayers;   }
      if(ExportDatesR==TRUE & ExportStateEnd==FALSE){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           list(CemaNeigeLayers) );
        names(OutputsModel) <- c("DatesR",NameCemaNeigeLayers);   }
      if(ExportDatesR==FALSE & ExportStateEnd==TRUE){
        OutputsModel <- c( list(CemaNeigeLayers),
                           list(CemaNeigeStateEnd));
        names(OutputsModel) <- c(NameCemaNeigeLayers,"StateEnd");   }
      if(ExportDatesR==TRUE & ExportStateEnd==TRUE){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           list(CemaNeigeLayers),
                           list(CemaNeigeStateEnd));
        names(OutputsModel) <- c("DatesR",NameCemaNeigeLayers,"StateEnd");   }

    ##End
      class(OutputsModel) <- c("OutputsModel","daily","CemaNeige");
      return(OutputsModel);

}
  

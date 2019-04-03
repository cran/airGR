RunModel_GR1A <- function(InputsModel,RunOptions,Param){

    NParam <- 1;
    FortranOutputs <- .FortranOutputs(GR = "GR1A")$GR

    ##Arguments_check
      if(inherits(InputsModel,"InputsModel")==FALSE){ stop("'InputsModel' must be of class 'InputsModel'") }  
      if(inherits(InputsModel,"yearly"     )==FALSE){ stop("'InputsModel' must be of class 'yearly'     ") }  
      if(inherits(InputsModel,"GR"         )==FALSE){ stop("'InputsModel' must be of class 'GR'         ") }  
      if(inherits(RunOptions,"RunOptions"  )==FALSE){ stop("'RunOptions' must be of class 'RunOptions'  ") }  
      if(inherits(RunOptions,"GR"          )==FALSE){ stop("'RunOptions' must be of class 'GR'          ") }  
      if(!is.vector(Param) | !is.numeric(Param)){ stop("'Param' must be a numeric vector") }
      if(sum(!is.na(Param))!=NParam){ stop(paste("'Param' must be a vector of length ",NParam," and contain no NA",sep="")) }
      Param <- as.double(Param);

    ##Input_data_preparation
      if(identical(RunOptions$IndPeriod_WarmUp,as.integer(0))){ RunOptions$IndPeriod_WarmUp <- NULL; }
      IndPeriod1   <- c(RunOptions$IndPeriod_WarmUp,RunOptions$IndPeriod_Run);
      LInputSeries <- as.integer(length(IndPeriod1))
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputs <- as.integer(1:length(FortranOutputs)); 
      } else { IndOutputs <- which(FortranOutputs %in% RunOptions$Outputs_Sim);  }

    ##Output_data_preparation
      IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries;
      ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim;
      ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim;
    
    BOOL_Fortran <- FALSE; if(BOOL_Fortran){
    ##Call_fortan
      RESULTS <- .Fortran("frun_gr1a",PACKAGE="airGR",
                 ##inputs
                     LInputs=LInputSeries,                             ### length of input and output series
                     InputsPrecip=InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/y]
                     InputsPE=InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/y]
                     NParam=as.integer(length(Param)),                 ### number of model parameter
                     Param=Param,                                      ### parameter set
                     NStates=as.integer(length(RunOptions$IniStates)), ### number of state variables used for model initialising
                     StateStart=RunOptions$IniStates,                  ### state variables used when the model run starts
                     NOutputs=as.integer(length(IndOutputs)),          ### number of output series
                     IndOutputs=IndOutputs,                            ### indices of output series
                 ##outputs
                     Outputs=matrix(as.double(-999.999),nrow=LInputSeries,ncol=length(IndOutputs)),  ### output series [mm]
                     StateEnd=rep(as.double(-999.999),length(RunOptions$IniStates))                  ### state variables at the end of the model run
                     )
      RESULTS$Outputs[ round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
      RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;

    } else {
    ##R_version
      L  <- length(IndPeriod1)
      P0 <- InputsModel$Precip[ IndPeriod1][1:(L-1)]
      P1 <- InputsModel$Precip[ IndPeriod1][2: L   ]
      E1 <- InputsModel$PotEvap[IndPeriod1][2: L   ]
      Q1 <- P1*(1.-1./(1.+((0.7*P1+0.3*P0)/Param[1]/E1)^2.0)^0.5)
      PEQ <- rbind(c(NA,NA,NA),cbind(P1,E1,Q1))
      Outputs <- PEQ[,IndOutputs]
      if(is.vector(Outputs)){ Outputs <- cbind(Outputs); }
      RESULTS <- list(NOutputs=length(IndOutputs),IndOutputs=IndOutputs,Outputs=Outputs,StatesEnd=NA)
   }
        
    
    ##Output_data_preparation
      ##OutputsModel_only
      if(ExportDatesR==FALSE & ExportStateEnd==FALSE){
        OutputsModel <- lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]);
        names(OutputsModel) <- FortranOutputs[IndOutputs];      }
      ##DatesR_and_OutputsModel_only
      if(ExportDatesR==TRUE & ExportStateEnd==FALSE){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]) );
        names(OutputsModel) <- c("DatesR",FortranOutputs[IndOutputs]);      }
      ##OutputsModel_and_SateEnd_only
      if(ExportDatesR==FALSE & ExportStateEnd==TRUE){
        OutputsModel <- c( lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(RESULTS$StateEnd) );
        names(OutputsModel) <- c(FortranOutputs[IndOutputs],"StateEnd");      }
      ##DatesR_and_OutputsModel_and_SateEnd
      if((ExportDatesR==TRUE & ExportStateEnd==TRUE) | "all" %in% RunOptions$Outputs_Sim){
        OutputsModel <- c( list(InputsModel$DatesR[RunOptions$IndPeriod_Run]),
                           lapply(seq_len(RESULTS$NOutputs), function(i) RESULTS$Outputs[IndPeriod2,i]),
                           list(RESULTS$StateEnd) );
        names(OutputsModel) <- c("DatesR",FortranOutputs[IndOutputs],"StateEnd");      }


    ##End
      rm(RESULTS); 
      class(OutputsModel) <- c("OutputsModel","yearly","GR");
      return(OutputsModel);

}


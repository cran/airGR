RunModel_GR2M <- function(InputsModel,RunOptions,Param){

    NParam <- 2;
    FortranOutputs <- c("PotEvap","Precip","AE","Perc","P3","Exch","Prod","Rout","Qsim");

    ##Arguments_check
      if(inherits(InputsModel,"InputsModel")==FALSE){ stop("InputsModel must be of class 'InputsModel' \n"); return(NULL); }  
      if(inherits(InputsModel,"monthly"    )==FALSE){ stop("InputsModel must be of class 'monthly'      \n"); return(NULL); }  
      if(inherits(InputsModel,"GR"         )==FALSE){ stop("InputsModel must be of class 'GR'          \n"); return(NULL); }  
      if(inherits(RunOptions,"RunOptions"  )==FALSE){ stop("RunOptions must be of class 'RunOptions'   \n"); return(NULL); }  
      if(inherits(RunOptions,"GR"          )==FALSE){ stop("RunOptions must be of class 'GR'           \n"); return(NULL); }  
      if(!is.vector(Param)){ stop("Param must be a vector \n"); return(NULL); }
      if(sum(!is.na(Param))!=NParam){ stop(paste("Param must be a vector of length ",NParam," and contain no NA \n",sep="")); return(NULL); }
      Param <- as.double(Param);

    ##Input_data_preparation
      if(identical(RunOptions$IndPeriod_WarmUp,as.integer(0))){ RunOptions$IndPeriod_WarmUp <- NULL; }
      IndPeriod1   <- c(RunOptions$IndPeriod_WarmUp,RunOptions$IndPeriod_Run);
      LInputSeries <- as.integer(length(IndPeriod1))
      if("all" %in% RunOptions$Outputs_Sim){ IndOutputs <- as.integer(1:length(FortranOutputs)); 
      } else { IndOutputs <- which(FortranOutputs %in% RunOptions$Outputs_Sim);  }

    ##Use_of_IniResLevels
      if("IniResLevels" %in% names(RunOptions)){
        RunOptions$IniStates[1] <- RunOptions$IniResLevels[1]*Param[1];  ### production store level (mm)
      }

    ##Call_fortan
      RESULTS <- .Fortran("frun_GR2M",PACKAGE="airGR",
                 ##inputs
                     LInputs=LInputSeries,                             ### length of input and output series
                     InputsPrecip=InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/month]
                     InputsPE=InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/month]
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
      RESULTS$Outputs [round(RESULTS$Outputs ,3)==(-999.999)] <- NA;
      RESULTS$StateEnd[round(RESULTS$StateEnd,3)==(-999.999)] <- NA;

    ##Output_data_preparation
      IndPeriod2     <- (length(RunOptions$IndPeriod_WarmUp)+1):LInputSeries;
      ExportDatesR   <- "DatesR"   %in% RunOptions$Outputs_Sim;
      ExportStateEnd <- "StateEnd" %in% RunOptions$Outputs_Sim;
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
      class(OutputsModel) <- c("OutputsModel","monthly","GR");
      return(OutputsModel);

}


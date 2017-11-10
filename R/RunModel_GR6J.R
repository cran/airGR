RunModel_GR6J <- function(InputsModel,RunOptions,Param){

    NParam <- 6;
    FortranOutputs <- c("PotEvap", "Precip", "Prod", "Pn", "Ps", "AE", "Perc", "PR", "Q9", "Q1",
			"Rout", "Exch", "AExch1", "AExch2", "AExch", "QR", "QRExp", "Exp", "QD", "Qsim");

    ##Arguments_check
      if(inherits(InputsModel,"InputsModel")==FALSE){ stop("InputsModel must be of class 'InputsModel' \n"); return(NULL); }  
      if(inherits(InputsModel,"daily"      )==FALSE){ stop("InputsModel must be of class 'daily'       \n"); return(NULL); }  
      if(inherits(InputsModel,"GR"         )==FALSE){ stop("InputsModel must be of class 'GR'          \n"); return(NULL); }  
      if(inherits(RunOptions,"RunOptions"  )==FALSE){ stop("RunOptions must be of class 'RunOptions'   \n"); return(NULL); }  
      if(inherits(RunOptions,"GR"          )==FALSE){ stop("RunOptions must be of class 'GR'           \n"); return(NULL); }  
      if(!is.vector(Param) | !is.numeric(Param)){ stop("Param must be a numeric vector \n"); return(NULL); }
      if(sum(!is.na(Param))!=NParam){ stop(paste("Param must be a vector of length ",NParam," and contain no NA \n",sep="")); return(NULL); }
      Param <- as.double(Param);
      
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
      
    ##Use_of_IniResLevels
      if(!is.null(RunOptions$IniResLevels)){
        RunOptions$IniStates[1] <- RunOptions$IniResLevels[1]*Param[1];  ### production store level (mm)
        RunOptions$IniStates[2] <- RunOptions$IniResLevels[2]*Param[3];  ### routing store level (mm)
        RunOptions$IniStates[3] <- RunOptions$IniResLevels[3]            ### exponential store level (mm)
      }

    ##Call_fortan
      RESULTS <- .Fortran("frun_GR6J",PACKAGE="airGR",
                 ##inputs
                     LInputs=LInputSeries,                             ### length of input and output series
                     InputsPrecip=InputsModel$Precip[IndPeriod1],      ### input series of total precipitation [mm/d]
                     InputsPE=InputsModel$PotEvap[IndPeriod1],         ### input series potential evapotranspiration [mm/d]
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
      if (ExportStateEnd) { 
        RESULTS$StateEnd <- CreateIniStates(FUN_MOD = RunModel_GR6J, InputsModel = InputsModel,
                                            ProdStore = RESULTS$StateEnd[1L], RoutStore = RESULTS$StateEnd[2L], ExpStore = RESULTS$StateEnd[3L],
                                            UH1 = RESULTS$StateEnd[(1:20)+7], UH2 = RESULTS$StateEnd[(1:40)+(7+20)],
                                            GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                                            verbose = FALSE)
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
      class(OutputsModel) <- c("OutputsModel","daily","GR");
      return(OutputsModel);

}
  

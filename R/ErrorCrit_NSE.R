ErrorCrit_NSE <- function(InputsCrit,OutputsModel, warnings = TRUE, verbose = TRUE){


##Arguments_check________________________________
  if(inherits(InputsCrit,"InputsCrit")==FALSE){ stop("InputsCrit must be of class 'InputsCrit' \n"); return(NULL); }  
  if(inherits(OutputsModel,"OutputsModel")==FALSE){ stop("OutputsModel must be of class 'OutputsModel' \n"); return(NULL); }  


##Initialisation_________________________________
  CritName <- NA;
  if(InputsCrit$transfo==""    ){ CritName <- "NSE[Q]"      ; }
  if(InputsCrit$transfo=="sqrt"){ CritName <- "NSE[sqrt(Q)]"; }
  if(InputsCrit$transfo=="log" ){ CritName <- "NSE[log(Q)]" ; }
  if(InputsCrit$transfo=="inv" ){ CritName <- "NSE[1/Q]"    ; }
  if(InputsCrit$transfo=="sort"){ CritName <- "NSE[sort(Q)]"; }
  CritValue       <- NA;
  CritBestValue   <- +1;
  Multiplier      <- -1; ### must be equal to -1 or +1 only


##Data_preparation_______________________________
  VarObs <- InputsCrit$Qobs  ; VarObs[!InputsCrit$BoolCrit] <- NA; 
  VarSim <- OutputsModel$Qsim; VarSim[!InputsCrit$BoolCrit] <- NA;  
  ##Data_transformation
  if("Ind_zeroes" %in% names(InputsCrit) & "epsilon" %in% names(InputsCrit)){ if(length(InputsCrit$Ind_zeroes)>0){
    VarObs <- VarObs + InputsCrit$epsilon;
    VarSim <- VarSim + InputsCrit$epsilon;
  } }
  if(InputsCrit$transfo=="sqrt"){ VarObs <- sqrt(VarObs); VarSim <- sqrt(VarSim); }
  if(InputsCrit$transfo=="log" ){ VarObs <- log(VarObs) ; VarSim <- log(VarSim) ; VarSim[VarSim      < -1E100] <- NA; }
  if(InputsCrit$transfo=="inv" ){ VarObs <- 1/VarObs    ; VarSim <- 1/VarSim    ; VarSim[abs(VarSim) > 1E+100] <- NA; }
  if(InputsCrit$transfo=="sort"){
	VarSim[is.na(VarObs)] <- NA
	VarSim <- sort(VarSim, na.last = TRUE)
	VarObs <- sort(VarObs, na.last = TRUE)
	InputsCrit$BoolCrit <- sort(InputsCrit$BoolCrit, decreasing = TRUE)
  }
  ##TS_ignore
  TS_ignore <- !is.finite(VarObs) | !is.finite(VarSim) | !InputsCrit$BoolCrit ;
  Ind_TS_ignore <- which(TS_ignore); if(length(Ind_TS_ignore)==0){ Ind_TS_ignore <- NULL; }
  if(sum(!TS_ignore)==0){ OutputsCrit <- list(NA); names(OutputsCrit) <- c("CritValue"); return(OutputsCrit); }
  if(inherits(OutputsModel,"hourly" )){ WarningTS <- 365; }
  if(inherits(OutputsModel,"daily"  )){ WarningTS <- 365; }
  if(inherits(OutputsModel,"monthly")){ WarningTS <-  12; }
  if(inherits(OutputsModel,"yearly" )){ WarningTS <-   3; }
  if(sum(!TS_ignore)<WarningTS & warnings){ warning("\t criterion computed on less than ", WarningTS, " time-steps") }
  ##Other_variables_preparation
  meanVarObs <- mean(VarObs[!TS_ignore]);
  meanVarSim <- mean(VarSim[!TS_ignore]);
  
  
##ErrorCrit______________________________________
  Emod <- sum((VarSim[!TS_ignore]-VarObs[!TS_ignore])^2);
  Eref <- sum((VarObs[!TS_ignore]-mean(VarObs[!TS_ignore]))^2);
  if(Emod==0 & Eref==0){ Crit <- 0; } else { Crit <- (1-Emod/Eref); }
  if(is.numeric(Crit) & is.finite(Crit)){ CritValue <- Crit; }


##Verbose______________________________________
  if(verbose) {
    message("Crit. ", CritName, " = ", sprintf("%.4f", CritValue))
  }
  
  
##Output_________________________________________
  OutputsCrit <- list(CritValue = CritValue, CritName = CritName,
                      CritBestValue = CritBestValue,
                      Multiplier = Multiplier, Ind_notcomputed = Ind_TS_ignore)
  return(OutputsCrit)

}



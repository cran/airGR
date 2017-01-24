ErrorCrit_KGE <- function(InputsCrit,OutputsModel, warnings = TRUE, verbose = TRUE){


##Arguments_check________________________________
  if(inherits(InputsCrit,"InputsCrit")==FALSE){ stop("InputsCrit must be of class 'InputsCrit' \n"); return(NULL); }  
  if(inherits(OutputsModel,"OutputsModel")==FALSE){ stop("OutputsModel must be of class 'OutputsModel' \n"); return(NULL); }  


##Initialisation_________________________________
  CritName <- NA;
  if(InputsCrit$transfo==""    ){ CritName <- "KGE[Q]"      ; }
  if(InputsCrit$transfo=="sqrt"){ CritName <- "KGE[sqrt(Q)]"; }
  if(InputsCrit$transfo=="log" ){ CritName <- "KGE[log(Q)]" ; }
  if(InputsCrit$transfo=="inv" ){ CritName <- "KGE[1/Q]"    ; }
  if(InputsCrit$transfo=="sort"){ CritName <- "KGE[sort(Q)]"; }
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
  if(sum(!TS_ignore)==1){ OutputsCrit <- list(NA); names(OutputsCrit) <- c("CritValue"); return(OutputsCrit); } ### to avoid a problem in standard deviation computation
  if(inherits(OutputsModel,"hourly" )){ WarningTS <- 365; }
  if(inherits(OutputsModel,"daily"  )){ WarningTS <- 365; }
  if(inherits(OutputsModel,"monthly")){ WarningTS <-  12; }
  if(inherits(OutputsModel,"yearly" )){ WarningTS <-   3; }
  if(sum(!TS_ignore)<WarningTS & warnings){ warning("\t criterion computed on less than ", WarningTS, " time-steps ") }
  ##Other_variables_preparation
  meanVarObs <- mean(VarObs[!TS_ignore]);
  meanVarSim <- mean(VarSim[!TS_ignore]);
  iCrit           <- 0;
  SubCritNames    <- NULL;
  SubCritValues   <- NULL;



##SubErrorCrit_____KGE_rPearson__________________
  iCrit <- iCrit+1;
  SubCritNames[iCrit]  <- paste(CritName," cor(sim, obs, \"pearson\") =", sep = "")
  SubCritValues[iCrit] <- NA;
  Numer <- sum( (VarObs[!TS_ignore]-meanVarObs)*(VarSim[!TS_ignore]-meanVarSim) );
  Deno1 <- sqrt( sum((VarObs[!TS_ignore]-meanVarObs)^2) );
  Deno2 <- sqrt( sum((VarSim[!TS_ignore]-meanVarSim)^2) );
  if(Numer==0){ if(Deno1==0 & Deno2==0){ Crit <- 1; } else { Crit <- 0; } 
  } else { Crit  <- Numer/(Deno1*Deno2); }
  if(is.numeric(Crit) & is.finite(Crit)){ SubCritValues[iCrit] <- Crit; }


##SubErrorCrit_____KGE_alpha_____________________
  iCrit <- iCrit+1;
  SubCritNames[iCrit]  <-  paste(CritName," sd(sim)/sd(obs)          =", sep = "")
  SubCritValues[iCrit] <- NA;
  Numer <- sd(VarSim[!TS_ignore]); 
  Denom <- sd(VarObs[!TS_ignore]); 
  if(Numer==0 & Denom==0){ Crit <- 1; } else { Crit <- Numer/Denom ; }
  if(is.numeric(Crit) & is.finite(Crit)){ SubCritValues[iCrit] <- Crit; }


##SubErrorCrit_____KGE_beta______________________  
  iCrit <- iCrit+1;
  SubCritNames[iCrit]  <-  paste(CritName," mean(sim)/mean(obs)      =", sep = "")
  SubCritValues[iCrit] <- NA;
  if(meanVarSim==0 & meanVarObs==0){ Crit <- 1; } else { Crit <- meanVarSim/meanVarObs ; }
  if(is.numeric(Crit) & is.finite(Crit)){ SubCritValues[iCrit] <- Crit; }
  

##ErrorCrit______________________________________
  if(sum(is.na(SubCritValues))==0){
  CritValue <- ( 1 - sqrt( (SubCritValues[1]-1)^2 + (SubCritValues[2]-1)^2 + (SubCritValues[3]-1)^2 ) );
  }


##Verbose______________________________________
  if(verbose) {
    message("Crit. ", CritName, " = ", sprintf("%.4f", CritValue))
    message(paste("\tSubCrit.", SubCritNames, sprintf("%.4f", SubCritValues), "\n", sep = " "))
  }
  

##Output_________________________________________
  OutputsCrit <- list(CritValue = CritValue, CritName = CritName,
                      SubCritValues = SubCritValues, SubCritNames = SubCritNames, CritBestValue = CritBestValue,
                      Multiplier = Multiplier, Ind_notcomputed = Ind_TS_ignore)
  return(OutputsCrit)

}


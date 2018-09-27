ErrorCrit_KGE2 <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {
  
  
    ##Arguments_check________________________________
    if (!inherits(InputsCrit, "InputsCrit")) {
      stop("InputsCrit must be of class 'InputsCrit' \n")
      return(NULL)
    }
    if (!inherits(OutputsModel, "OutputsModel")) {
      stop("OutputsModel must be of class 'OutputsModel' \n")
      return(NULL)
    }
    
    
    ##Initialisation_________________________________
    CritName <- NA
    if (InputsCrit$transfo == "") {
      CritName <- "KGE'[Q]"
    }
    if (InputsCrit$transfo == "sqrt") {
      CritName <- "KGE'[sqrt(Q)]"
    }
    if (InputsCrit$transfo == "log") {
      CritName <- "KGE'[log(Q)]"
    }
    if (InputsCrit$transfo == "inv") {
      CritName <- "KGE'[1/Q]"
    }
    if (InputsCrit$transfo == "sort") {
      CritName <- "KGE'[sort(Q)]"
    }
    CritValue       <- NA
    CritBestValue   <- +1
    Multiplier      <- -1
    ### must be equal to -1 or +1 only
    
    
    ##Data_preparation_______________________________
    VarObs <- InputsCrit$Qobs
    VarObs[!InputsCrit$BoolCrit] <- NA
    VarSim <- OutputsModel$Qsim
    VarSim[!InputsCrit$BoolCrit] <- NA
    
    ##Data_transformation
    if (InputsCrit$transfo %in% c("log", "inv") & is.null(InputsCrit$epsilon) & verbose) {
      if (any(VarObs %in% 0)) {
        warning("zeroes detected in Qobs: the corresponding time-steps will be exclude from the criteria computation if the epsilon agrument of 'CreateInputsCrit' = NULL")
      }
      if (any(VarSim %in% 0)) {
        warning("zeroes detected in Qsim: the corresponding time-steps will be exclude from the criteria computation if the epsilon agrument of 'CreateInputsCrit' = NULL")
      }  
    }
    if ("epsilon" %in% names(InputsCrit) & !is.null(InputsCrit$epsilon)) {
      VarObs <- VarObs + InputsCrit$epsilon
      VarSim <- VarSim + InputsCrit$epsilon
    }
    if (InputsCrit$transfo == "sqrt") {
      VarObs <- sqrt(VarObs)
      VarSim <- sqrt(VarSim)
    }
    if (InputsCrit$transfo == "log") {
      VarObs <- log(VarObs)
      VarSim <- log(VarSim)
      VarSim[VarSim      < -1e100] <- NA
    }
    if (InputsCrit$transfo == "inv") {
      VarObs <- 1 / VarObs
      VarSim <- 1 / VarSim
      VarSim[abs(VarSim) > 1e+100] <- NA
    }
    if (InputsCrit$transfo == "sort") {
      VarSim[is.na(VarObs)] <- NA
      VarSim <- sort(VarSim, na.last = TRUE)
      VarObs <- sort(VarObs, na.last = TRUE)
      InputsCrit$BoolCrit <- sort(InputsCrit$BoolCrit, decreasing = TRUE)
    }
    
    ##TS_ignore
    TS_ignore <- !is.finite(VarObs) | !is.finite(VarSim) | !InputsCrit$BoolCrit
    Ind_TS_ignore <- which(TS_ignore)
    if (length(Ind_TS_ignore) == 0) {
      Ind_TS_ignore <- NULL
    }
    if (sum(!TS_ignore) == 0) {
      OutputsCrit <- list(NA)
      names(OutputsCrit) <- c("CritValue")
      return(OutputsCrit)
    }
    if (sum(!TS_ignore) == 1) {
      OutputsCrit <- list(NA)
      names(OutputsCrit) <- c("CritValue")
      return(OutputsCrit)
    } ### to avoid a problem in standard deviation computation
    if (inherits(OutputsModel, "hourly")) {
      WarningTS <- 365
    }
    if (inherits(OutputsModel, "daily")) {
      WarningTS <- 365
    }
    if (inherits(OutputsModel, "monthly")) {
      WarningTS <-  12
    }
    if (inherits(OutputsModel, "yearly")) {
      WarningTS <-   3
    }
    if (sum(!TS_ignore) < WarningTS & warnings) {
      warning("\t criterion computed on less than ", WarningTS, " time-steps")
    }
    
    ##Other_variables_preparation
    meanVarObs <- mean(VarObs[!TS_ignore])
    meanVarSim <- mean(VarSim[!TS_ignore])
    
    iCrit           <- 0
    SubCritPrint    <- NULL
    SubCritNames    <- NULL
    SubCritValues   <- NULL
    
    
    ##SubErrorCrit_____KGE_rPearson__________________
    iCrit <- iCrit + 1
    SubCritPrint[iCrit]  <- paste(CritName, " cor(sim, obs, \"pearson\") =", sep = "")
    SubCritValues[iCrit] <- NA
    SubCritNames[iCrit]  <- "r"
    
    Numer <- sum((VarObs[!TS_ignore] - meanVarObs) * (VarSim[!TS_ignore] - meanVarSim))
    Deno1 <- sqrt(sum((VarObs[!TS_ignore] - meanVarObs)^2))
    Deno2 <- sqrt(sum((VarSim[!TS_ignore] - meanVarSim)^2))
    
    if (Numer == 0) {
      if (Deno1 == 0 & Deno2 == 0) {
        Crit <- 1
      } else {
        Crit <- 0
      }
    } else {
      Crit  <- Numer / (Deno1 * Deno2)
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[iCrit] <- Crit
    }
    
    
    ##SubErrorCrit_____KGE_gamma______________________
    iCrit <- iCrit + 1
    SubCritPrint[iCrit]  <- paste(CritName, " cv(sim)/cv(obs)          =", sep = "")
    SubCritValues[iCrit] <- NA
    SubCritNames[iCrit]  <- "gamma"
    
    if (meanVarSim == 0) {
      if (sd(VarSim[!TS_ignore]) == 0) {
        CVsim <- 1
      } else {
        CVsim <- 99999
      }
    } else {
      CVsim <- sd(VarSim[!TS_ignore]) / meanVarSim
      
    }
    if (meanVarObs == 0) {
      if (sd(VarObs[!TS_ignore]) == 0) {
        CVobs <- 1
      } else {
        CVobs <- 99999
      }
    } else {
      CVobs <- sd(VarObs[!TS_ignore]) / meanVarObs
    }
    if (CVsim == 0 &
        CVobs == 0) {
      Crit <- 1
    } else {
      Crit <- CVsim / CVobs
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[iCrit] <- Crit
    }
    
    
    ##SubErrorCrit_____KGE_beta______________________
    iCrit <- iCrit + 1
    SubCritPrint[iCrit]  <- paste(CritName, " mean(sim)/mean(obs)      =", sep = "")
    SubCritValues[iCrit] <- NA
    SubCritNames[iCrit]  <- "beta"
    
    if (meanVarSim == 0 & meanVarObs == 0) {
      Crit <- 1
    } else {
      Crit <- meanVarSim / meanVarObs
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[iCrit] <- Crit
    }
    
    
    ##ErrorCrit______________________________________
    if (sum(is.na(SubCritValues)) == 0) {
      CritValue <- (1 - sqrt((SubCritValues[1] - 1)^2 + (SubCritValues[2] - 1)^2 + (SubCritValues[3] - 1)^2))
    }
    
    
    ##Verbose______________________________________
    if (verbose) {
      message("Crit. ", CritName, " = ", sprintf("%.4f", CritValue))
      message(paste("\tSubCrit.", SubCritPrint, sprintf("%.4f", SubCritValues), "\n", sep = " "))
    }
    
    
    ##Output_________________________________________
    OutputsCrit <- list(CritValue       = CritValue,
                        CritName        = CritName,
                        SubCritValues   = SubCritValues,
                        SubCritNames    = SubCritNames,
                        CritBestValue   = CritBestValue,
                        Multiplier      = Multiplier,
                        Ind_notcomputed = Ind_TS_ignore
    )
    return(OutputsCrit)
    
  }

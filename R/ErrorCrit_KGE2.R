ErrorCrit_KGE2 <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {

  ## Arguments check
  if (!inherits(OutputsModel, "OutputsModel")) {
    stop("'OutputsModel' must be of class 'OutputsModel'")
  }

  EC <- .ErrorCrit(InputsCrit = InputsCrit, Crit = "KGE2", OutputsModel = OutputsModel, warnings = warnings)

  CritValue <- NA
  SubCritValues <- rep(NA, 3)
  SubCritNames  <- c("r", "gamma", "beta")
  SubCritPrint  <- rep(NA, 3)

  if (EC$CritCompute) {
    ## Other variables preparation
    meanVarObs <- mean(EC$VarObs[!EC$TS_ignore])
    meanVarSim <- mean(EC$VarSim[!EC$TS_ignore])

    ## SubErrorCrit KGE rPearson
    SubCritPrint[1L] <- paste0(EC$CritName, " cor(sim, obs, \"pearson\") =")

    Numer <- sum((EC$VarObs[!EC$TS_ignore] - meanVarObs) * (EC$VarSim[!EC$TS_ignore] - meanVarSim))
    Deno1 <- sqrt(sum((EC$VarObs[!EC$TS_ignore] - meanVarObs)^2))
    Deno2 <- sqrt(sum((EC$VarSim[!EC$TS_ignore] - meanVarSim)^2))

    if (Numer == 0) {
      if (Deno1 == 0 & Deno2 == 0) {
        Crit <- 1
      } else {
        Crit <- 0
      }
    } else {
      Crit <- Numer / (Deno1 * Deno2)
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[1L] <- Crit
    }

    ## SubErrorCrit KGE gamma
    SubCritPrint[2L] <- paste0(EC$CritName, " cv(sim)/cv(obs)          =")

    if (meanVarSim == 0) {
      if (sd(EC$VarSim[!EC$TS_ignore]) == 0) {
        CVsim <- 1
      } else {
        CVsim <- 99999
      }
    } else {
      CVsim <- sd(EC$VarSim[!EC$TS_ignore]) / meanVarSim

    }
    if (meanVarObs == 0) {
      if (sd(EC$VarObs[!EC$TS_ignore]) == 0) {
        CVobs <- 1
      } else {
        CVobs <- 99999
      }
    } else {
      CVobs <- sd(EC$VarObs[!EC$TS_ignore]) / meanVarObs
    }
    if (CVsim == 0 &
        CVobs == 0) {
      Crit <- 1
    } else {
      Crit <- CVsim / CVobs
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[2L] <- Crit
    }

    ## SubErrorCrit KGE beta
    SubCritPrint[3L] <- paste0(EC$CritName, " mean(sim)/mean(obs)      =")

    if (meanVarSim == 0 & meanVarObs == 0) {
      Crit <- 1
    } else {
      Crit <- meanVarSim / meanVarObs
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      SubCritValues[3L] <- Crit
    }

    ## ErrorCrit
    if (sum(is.na(SubCritValues)) == 0) {
      CritValue <- (1 - sqrt((SubCritValues[1L] - 1)^2 + (SubCritValues[2L] - 1)^2 + (SubCritValues[3L] - 1)^2))
    }

    ## Verbose
    if (verbose) {
      message(sprintf("Crit. %s = %.4f", EC$CritName, CritValue))
      message(paste("\tSubCrit.", SubCritPrint, sprintf("%.4f", SubCritValues), "\n", sep = " "))
    }
  }


  ## Output
  OutputsCrit <- list(CritValue       = CritValue,
                      CritName        = EC$CritName,
                      SubCritValues   = SubCritValues,
                      SubCritNames    = SubCritNames,
                      CritBestValue   = EC$CritBestValue,
                      Multiplier      = EC$Multiplier,
                      Ind_notcomputed = EC$Ind_TS_ignore)

  class(OutputsCrit) <- c("KGE2", "ErrorCrit")
  return(OutputsCrit)

}

class(ErrorCrit_KGE2) <- c("FUN_CRIT", class(ErrorCrit_KGE2))

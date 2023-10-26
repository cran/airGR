ErrorCrit_NSE <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {

  ## Arguments check
  if (!inherits(OutputsModel, "OutputsModel")) {
    stop("'OutputsModel' must be of class 'OutputsModel'")
  }

  EC <- .ErrorCrit(InputsCrit = InputsCrit, Crit = "NSE", OutputsModel = OutputsModel, warnings = warnings)

  CritValue <- NA

  if (EC$CritCompute) {
    ## ErrorCrit
    Emod <- sum((EC$VarSim[!EC$TS_ignore] - EC$VarObs[!EC$TS_ignore])^2)
    Eref <- sum((EC$VarObs[!EC$TS_ignore] - mean(EC$VarObs[!EC$TS_ignore]))^2)

    if (Emod == 0 & Eref == 0) {
      Crit <- 0
    } else {
      Crit <- (1 - Emod / Eref)
    }
    if (is.numeric(Crit) & is.finite(Crit)) {
      CritValue <- Crit
    }

    ## Verbose
    if (verbose) {
      message(sprintf("Crit. %s = %.4f", EC$CritName, CritValue))
    }
  }


  ## Output
  OutputsCrit <- list(CritValue       = CritValue,
                      CritName        = EC$CritName,
                      CritBestValue   = EC$CritBestValue,
                      Multiplier      = EC$Multiplier,
                      Ind_notcomputed = EC$Ind_TS_ignore)

  class(OutputsCrit) <- c("NSE", "ErrorCrit")
  return(OutputsCrit)

}

class(ErrorCrit_NSE) <- c("FUN_CRIT", class(ErrorCrit_NSE))

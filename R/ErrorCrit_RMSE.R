ErrorCrit_RMSE <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {

  ## Arguments check
  if (!inherits(OutputsModel, "OutputsModel")) {
    stop("'OutputsModel' must be of class 'OutputsModel'")
  }

  EC <- .ErrorCrit(InputsCrit = InputsCrit, Crit = "RMSE", OutputsModel = OutputsModel, warnings = warnings)

  CritValue <- NA

  if (EC$CritCompute) {
    ## ErrorCrit
    Numer <- sum((EC$VarSim - EC$VarObs)^2, na.rm = TRUE)
    Denom <- sum(!is.na(EC$VarObs))

    if (Numer == 0) {
      Crit <- 0
    } else {
      Crit <- sqrt(Numer / Denom)
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

  class(OutputsCrit) <- c("RMSE", "ErrorCrit")
  return(OutputsCrit)

}

class(ErrorCrit_RMSE) <- c("FUN_CRIT", class(ErrorCrit_RMSE))

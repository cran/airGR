CreateErrorCrit_GAPX <- function(FUN_TRANSFO) {

  FUN_CRIT <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {
    ## Arguments check
    if (!inherits(OutputsModel, "OutputsModel")) {
      stop("'OutputsModel' must be of class 'OutputsModel'")
    }

    OutputsModel$RunOptions$ParamT <- FUN_TRANSFO(OutputsModel$RunOptions$Param, "RT")

    EC <- .ErrorCrit(InputsCrit = InputsCrit, Crit = "GAPX", OutputsModel = OutputsModel, warnings = warnings)

    CritValue <- NA

    if (EC$CritCompute) {

      ParamApr <- EC$VarObs[!EC$TS_ignore]
      ParamOpt <- EC$VarSim[!EC$TS_ignore]

      ## ErrorCrit
      Crit <- 1 - sum(((ParamApr - ParamOpt) / 20)^2)^0.5

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

    class(OutputsCrit) <- c("GAPX", "ErrorCrit")
    return(OutputsCrit)
  }

  class(FUN_CRIT) <- c("FUN_CRIT", class(FUN_CRIT))

  return(FUN_CRIT)
}

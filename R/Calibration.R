Calibration <- function(InputsModel,
                        RunOptions,
                        InputsCrit,
                        CalibOptions,
                        FUN_MOD,
                        FUN_CRIT,                      # deprecated
                        FUN_CALIB = Calibration_Michel,
                        FUN_TRANSFO = NULL,
                        verbose = TRUE,
                        ...) {

  FUN_MOD   <- match.fun(FUN_MOD)

  if (!missing(FUN_CRIT)) {
    FUN_CRIT <- match.fun(FUN_CRIT)
  }

  FUN_CALIB <- match.fun(FUN_CALIB)

  if (!is.null(FUN_TRANSFO)) {
    FUN_TRANSFO <- match.fun(FUN_TRANSFO)
  }

  return(FUN_CALIB(InputsModel = InputsModel, RunOptions = RunOptions, InputsCrit = InputsCrit,
                   CalibOptions = CalibOptions,
                   FUN_MOD = FUN_MOD, FUN_TRANSFO = FUN_TRANSFO,
                   verbose = verbose, ...))
}


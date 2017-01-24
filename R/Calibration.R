Calibration <- function(InputsModel, RunOptions, InputsCrit, CalibOptions, FUN_MOD, FUN_CRIT, FUN_CALIB = Calibration_Michel, FUN_TRANSFO = NULL, verbose = TRUE) {
  return(FUN_CALIB(InputsModel, RunOptions, InputsCrit, CalibOptions, FUN_MOD, FUN_CRIT, FUN_TRANSFO, verbose = verbose))
}


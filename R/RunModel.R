RunModel <- function(InputsModel, RunOptions, Param, FUN_MOD, ...) {

  FUN_MOD <- match.fun(FUN_MOD)

  if (inherits(InputsModel, "SD") && !identical(FUN_MOD, RunModel_Lag)) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
    RunOptions$FeatFUN_MOD$NbParam <- RunOptions$FeatFUN_MOD$NbParam - 1
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }

  OutputsModel <- FUN_MOD(InputsModel = InputsModel, RunOptions = RunOptions,
                          Param = Param[iFirstParamRunOffModel:length(Param)], ...)

  if (inherits(InputsModel, "SD") && !identical(FUN_MOD, RunModel_Lag)) {
    OutputsModel <- RunModel_Lag(InputsModel, RunOptions, Param[1], OutputsModel)
  }
  return(OutputsModel)
}

RunModel <- function(InputsModel, RunOptions, Param, FUN_MOD) {
  
  FUN_MOD <- match.fun(FUN_MOD)
  
  if (inherits(InputsModel, "SD")) {
    # LAG Model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }
  
  OutputsModel <- FUN_MOD(InputsModel = InputsModel, RunOptions = RunOptions,
                          Param = Param[iFirstParamRunOffModel:length(Param)])
  
  if (inherits(InputsModel, "SD")) {
    InputsModel$OutputsModel <- OutputsModel
    OutputsModel <- RunModel_Lag(InputsModel, RunOptions, Param[1])
  }
  return(OutputsModel)
}
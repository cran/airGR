RunModel <- function(InputsModel, RunOptions, Param, FUN_MOD) {
  
  FUN_MOD <- match.fun(FUN_MOD)
  
  return(FUN_MOD(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param))
  
}

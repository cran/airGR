TransfoParam_GR6J <- function(ParamIn, Direction) {
  
  
  ## number of model parameters
  NParam <- 6L
  
  
  ## check arguments
  isVecParamIn <- is.vector(ParamIn)
  if (isVecParamIn) {
    ParamIn <- matrix(ParamIn, nrow = 1)
  }  
  if (!inherits(ParamIn, "matrix")) {
    stop("'ParamIn' must be of class 'matrix'")
  }
  if (!inherits(Direction, "character") | length(Direction) != 1 | any(!Direction %in% c("RT", "TR"))) {
    stop("'Direction' must be a character vector of length 1 equal to 'RT' or 'TR'")
  }
  if (ncol(ParamIn) != NParam) {
    stop(sprintf("the GR6J model requires %i parameters", NParam))
  }
  
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- exp(ParamIn[, 1])                         ### GR6J X1 (production store capacity)
    ParamOut[, 2] <- sinh(ParamIn[, 2])                        ### GR6J X2 (groundwater exchange coefficient 1)
    ParamOut[, 3] <- exp(ParamIn[, 3])                         ### GR6J X3 (routing store capacity)
    ParamOut[, 4] <- 20 + 19.5 * (ParamIn[, 4] - 9.99) / 19.98 ### GR6J X4 (unit hydrograph time constant)
    ParamOut[, 5] <- ParamIn[, 5] / 5                          ### GR6J X5 (groundwater exchange coefficient 2)
    ParamOut[, 6] <- exp(ParamIn[, 6])                         ### GR6J X6 (coefficient for emptying exponential store)
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- log(ParamIn[, 1])                         ### GR6J X1 (production store capacity)
    ParamOut[, 2] <- asinh(ParamIn[, 2])                       ### GR6J X2 (groundwater exchange coefficient 1)
    ParamOut[, 3] <- log(ParamIn[, 3])                         ### GR6J X3 (routing store capacity)
    ParamOut[, 4] <- 9.99 + 19.98 * (ParamIn[, 4] - 20) / 19.5 ### GR6J X4 (unit hydrograph time constant)
    ParamOut[, 5] <- ParamIn[, 5] * 5                          ### GR6J X5 (groundwater exchange coefficient 2)
    ParamOut[, 6] <- log(ParamIn[, 6])                         ### GR6J X6 (coefficient for emptying exponential store)
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}

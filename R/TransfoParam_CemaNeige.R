TransfoParam_CemaNeige <- function(ParamIn, Direction) {
  
  ## number of model parameters
  NParam <- 2L
  
  
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
    stop(sprintf("the CemaNeige module requires %i parameters", NParam))
  }
  
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- (ParamIn[, 1] + 9.99) / 19.98 ### CemaNeige X1 (weighting coefficient for snow pack thermal state)
    ParamOut[, 2] <- exp(ParamIn[, 2]) / 200       ### CemaNeige X2 (degree-day melt coefficient)
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- ParamIn[, 1] * 19.98 - 9.99  ### CemaNeige X1 (weighting coefficient for snow pack thermal state)
    ParamOut[, 2] <- log(ParamIn[, 2] * 200)       ### CemaNeige X2 (degree-day melt coefficient)
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}

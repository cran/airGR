TransfoParam_CemaNeigeHyst <- function(ParamIn, Direction) {
  
  ## number of model parameters
  NParam <- 4L
  
  
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
    stop(sprintf("the CemaNeige module with linear hysteresis requires %i parameters", NParam))
  }
  
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- (ParamIn[, 1] + 9.99) / 19.98 ### CemaNeige X1 (weighting coefficient for snow pack thermal state)
    ParamOut[, 2] <- exp(ParamIn[, 2]) / 200       ### CemaNeige X2 (degree-day melt coefficient)
    ParamOut[, 3] <- (ParamIn[, 3] * 5) + 50       ### Hyst Gaccum
    ParamOut[, 4] <- (ParamIn[, 4] / 19.98) + 0.5  ### Hyst CV
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- ParamIn[, 1] * 19.98 - 9.99  ### CemaNeige X1 (weighting coefficient for snow pack thermal state)
    ParamOut[, 2] <- log(ParamIn[, 2] * 200)       ### CemaNeige X2 (degree-day melt coefficient)
    ParamOut[, 3] <- (ParamIn[, 3] - 50) / 5       ### Hyst Gaccum
    ParamOut[, 4] <- (ParamIn[, 4] - 0.5) * 19.98  ### Hyst CV
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}


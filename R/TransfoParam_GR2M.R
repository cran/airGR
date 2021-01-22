TransfoParam_GR2M <- function(ParamIn, Direction) {
  
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
    stop(sprintf("the GR2M model requires %i parameters", NParam))
  }
  

  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- exp(ParamIn[, 1])
    ParamOut[, 2] <- ParamIn[, 2] / 4 + 2.5
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- log(ParamIn[, 1])
    ParamOut[, 2] <- (ParamIn[, 2] - 2.5) * 4
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}

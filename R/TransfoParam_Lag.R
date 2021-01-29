TransfoParam_Lag <- function(ParamIn, Direction) {
  
  ## number of model parameters
  NParam <- 1L
  
  
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
    stop(sprintf("the Lag model requires %i parameters", NParam))
  }
  
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- 20 * (ParamIn + 10) / 20.0
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn * 20.0 / 20 - 10 
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}

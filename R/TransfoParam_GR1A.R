TransfoParam_GR1A <- function(ParamIn, Direction) {
  
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
    stop(sprintf("the GR1A model requires %i parameters", NParam))
  }
  

  ## transformation
  if (Direction == "TR") {
    ParamOut <- (ParamIn + 10.0) / 8
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn * 8 - 10.0
  }
  
  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)
  
}

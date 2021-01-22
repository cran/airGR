TransfoParam_GR4H <- function(ParamIn, Direction) {

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
    stop(sprintf("the GR4H model requires %i parameters", NParam))
  }
  
  
  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- exp(ParamIn[, 1])                                 ### GR4H X1 (production store capacity)    
    ParamOut[, 2] <- sinh(ParamIn[, 2] / 3)                            ### GR4H X2 (groundwater exchange coefficient)      
    ParamOut[, 3] <- exp(ParamIn[, 3])                                 ### GR4H X3 (routing store capacity)       
    ParamOut[, 4] <- 480 + (480 - 0.5) * (ParamIn[, 4] - 9.99) / 19.98 ### GR4H X4 (unit hydrograph time constant)
  }	 
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- log(ParamIn[, 1])                                 ### GR4H X1 (production store capacity)    
    ParamOut[, 2] <- 3 * asinh(ParamIn[, 2])                           ### GR4H X2 (groundwater exchange coefficient)      
    ParamOut[, 3] <- log(ParamIn[, 3])                                 ### GR4H X3 (routing store capacity)       
    ParamOut[, 4] <- (ParamIn[, 4] - 480) * 19.98 / (480 - 0.5) + 9.99 ### GR4H X4 (unit hydrograph time constant)
  }	

  
  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }
  
  return(ParamOut)

}


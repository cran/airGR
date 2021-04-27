TransfoParam_GR5H <- function(ParamIn, Direction) {

  ## number of model parameters
  NParam <- 5L


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
    stop(sprintf("the GR5H model requires %i parameters", NParam))
  }


  ## transformation
  if (Direction == "TR") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- exp(ParamIn[, 1])                             ### GR5H X1 (production store capacity)
    ParamOut[, 2] <- sinh(ParamIn[, 2])                            ### GR5H X2 (groundwater exchange coefficient)
    ParamOut[, 3] <- exp(ParamIn[, 3])                             ### GR5H X3 (routing store capacity)
    ParamOut[, 4] <- 480 + (480 - 0.01) * (ParamIn[, 4] - 10) / 20 ### GR5H X4 (unit hydrograph time constant)
    ParamOut[, 5] <- (ParamIn[, 5] + 10) / 20                      ### GR5H X5 (groundwater exchange coefficient 2)
  }
  if (Direction == "RT") {
    ParamOut <- ParamIn
    ParamOut[, 1] <- log(ParamIn[, 1])                             ### GR5H X1 (production store capacity)
    ParamOut[, 2] <- asinh(ParamIn[, 2])                           ### GR5H X2 (groundwater exchange coefficient)
    ParamOut[, 3] <- log(ParamIn[, 3])                             ### GR5H X3 (routing store capacity)
    ParamOut[, 4] <- (ParamIn[, 4] - 480) * 20 / (480 - 0.01) + 10 ### GR5H X4 (unit hydrograph time constant)
    ParamOut[, 5] <- ParamIn[, 5] * 20 - 10                        ### GR5H X5 (groundwater exchange coefficient 2)
  }


  ## check output
  if (isVecParamIn) {
    ParamOut <- as.vector(ParamOut)
  }

  return(ParamOut)

}


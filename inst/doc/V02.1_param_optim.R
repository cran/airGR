## ---- warning=FALSE, include=FALSE, fig.keep='none', results='hide'-----------
library(airGR)
library(DEoptim)
#library(hydroPSO)
library(Rmalschains)
# source("airGR.R")
set.seed(321)
load(system.file("vignettesData/vignetteParamOptim.rda", package = "airGR"))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  example("Calibration_Michel")

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  OptimGR4J <- function(ParamOptim) {
#    ## Transformation of the parameter set to real space
#    RawParamOptim <- airGR::TransfoParam_GR4J(ParamIn = ParamOptim,
#                                              Direction = "TR")
#    ## Simulation given a parameter set
#    OutputsModel <- airGR::RunModel_GR4J(InputsModel = InputsModel,
#                                         RunOptions = RunOptions,
#                                         Param = RawParamOptim)
#    ## Computation of the value of the performance criteria
#    OutputsCrit <- airGR::ErrorCrit_RMSE(InputsCrit = InputsCrit,
#                                         OutputsModel = OutputsModel,
#                                         verbose = FALSE)
#    return(OutputsCrit$CritValue)
#  }

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  lowerGR4J <- rep(-9.99, times = 4)
#  upperGR4J <- rep(+9.99, times = 4)

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  startGR4J <- c(4.1, 3.9, -0.9, -8.7)
#  optPORT <- stats::nlminb(start = startGR4J,
#                           objective = OptimGR4J,
#                           lower = lowerGR4J, upper = upperGR4J,
#                           control = list(trace = 1))

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  startGR4J <- expand.grid(data.frame(CalibOptions$StartParamDistrib))
#  optPORT_ <- function(x) {
#    opt <- stats::nlminb(start = x,
#                         objective = OptimGR4J,
#                         lower = lowerGR4J, upper = upperGR4J,
#                         control = list(trace = 1))
#  }
#  listOptPORT <- apply(startGR4J, MARGIN = 1, FUN = optPORT_)

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  parPORT <- t(sapply(listOptPORT, function(x) x$par))
#  objPORT <- sapply(listOptPORT, function(x) x$objective)
#  resPORT <- data.frame(parPORT, RMSE = objPORT)

## ---- warning=FALSE-----------------------------------------------------------
summary(resPORT)

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  optDE <- DEoptim::DEoptim(fn = OptimGR4J,
#                            lower = lowerGR4J, upper = upperGR4J,
#                            control = DEoptim::DEoptim.control(NP = 40, trace = 10))

## ---- warning=FALSE, results='hide', message=FALSE, eval=FALSE----------------
#  optPSO <- hydroPSO::hydroPSO(fn = OptimGR4J,
#                               lower = lowerGR4J, upper = upperGR4J,
#                               control = list(write2disk = FALSE, verbose = FALSE))

## ---- warning=FALSE, results='hide', eval=FALSE-------------------------------
#  optMALS <- Rmalschains::malschains(fn = OptimGR4J,
#                                     lower = lowerGR4J, upper = upperGR4J,
#                                     maxEvals = 2000)

## ---- warning=FALSE, echo=FALSE, eval=FALSE-----------------------------------
#  resGLOB <- data.frame(Algo = c("airGR", "PORT", "DE", "PSO", "MA-LS"),
#                        round(rbind(
#                          OutputsCalib$ParamFinalR                          ,
#                          airGR::TransfoParam_GR4J(ParamIn = optPORT$par                    , Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = as.numeric(optDE$optim$bestmem), Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = as.numeric(optPSO$par)         , Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = optMALS$sol                    , Direction = "TR")),
#                          digits = 3))

## ---- warning=FALSE, echo=FALSE-----------------------------------------------
resGLOB


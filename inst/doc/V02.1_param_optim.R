## ----setup, warning=FALSE, include=FALSE, fig.keep='none', results='hide'-----
library(airGR)
library(DEoptim)
# library(hydroPSO) # Needs R version >= 3.6 or latticeExtra <= 0.6-28 on R 3.5. Archived on 2023-10-16 as requires archived packages 'hydroTSM' and 'hydroGOF'.
library(Rmalschains)
library(caRamel)
library(ggplot2)
library(GGally)
# source("airGR.R")
set.seed(321)
load(system.file("vignettesData/vignetteParamOptim.rda", package = "airGR"))
load(system.file("vignettesData/vignetteParamOptimCaramel.rda", package = "airGR"))

## ----Calibration_Michel, echo=TRUE, eval=FALSE--------------------------------
#  example("Calibration_Michel")

## ----RunOptions, results='hide', eval=FALSE-----------------------------------
#  RunOptions <- airGR::CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
#                                        IndPeriod_Run = Ind_Run,
#                                        Outputs_Sim = "Qsim")

## ----OptimGR4J, warning=FALSE, results='hide', eval=FALSE---------------------
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

## ----boundsGR4J, warning=FALSE, results='hide', eval=FALSE--------------------
#  lowerGR4J <- rep(-9.99, times = 4)
#  upperGR4J <- rep(+9.99, times = 4)

## ----local1, warning=FALSE, results='hide', eval=FALSE------------------------
#  startGR4J <- c(4.1, 3.9, -0.9, -8.7)
#  optPORT <- stats::nlminb(start = startGR4J,
#                           objective = OptimGR4J,
#                           lower = lowerGR4J, upper = upperGR4J,
#                           control = list(trace = 1))

## ----local2, warning=FALSE, results='hide', eval=FALSE------------------------
#  startGR4JDistrib <- TransfoParam_GR4J(ParamIn = CalibOptions$StartParamDistrib,
#                                        Direction = "RT")
#  startGR4J <- expand.grid(data.frame(startGR4JDistrib))
#  optPORT_ <- function(x) {
#    opt <- stats::nlminb(start = x,
#                         objective = OptimGR4J,
#                         lower = lowerGR4J, upper = upperGR4J,
#                         control = list(trace = 1))
#  }
#  listOptPORT <- apply(startGR4J, MARGIN = 1, FUN = optPORT_)

## ----local3, warning=FALSE, results='hide', eval=FALSE------------------------
#  parPORT <- t(sapply(listOptPORT, function(x) x$par))
#  objPORT <- sapply(listOptPORT, function(x) x$objective)
#  resPORT <- data.frame(parPORT, RMSE = objPORT)

## ----local4, warning=FALSE----------------------------------------------------
summary(resPORT)

## ----optDE, warning=FALSE, results='hide', eval=FALSE-------------------------
#  optDE <- DEoptim::DEoptim(fn = OptimGR4J,
#                            lower = lowerGR4J, upper = upperGR4J,
#                            control = DEoptim::DEoptim.control(NP = 40, trace = 10))

## ----hydroPSO1, warning=FALSE, results='hide', message=FALSE, eval=FALSE------
#  # to install the package temporary removed from CRAN
#  # Rtools needed (windows : https://cran.r-project.org/bin/windows/Rtools/)
#  # install.packages("https://cran.r-project.org/src/contrib/Archive/hydroPSO/hydroPSO_0.5-1.tar.gz",
#  #                  repos = NULL, type = "source", dependencies = TRUE)

## ----hydroPSO2, warning=FALSE, results='hide', message=FALSE, eval=FALSE------
#  optPSO <- hydroPSO::hydroPSO(fn = OptimGR4J,
#                               lower = lowerGR4J, upper = upperGR4J,
#                               control = list(write2disk = FALSE, verbose = FALSE))

## ----warning=FALSE, results='hide', eval=FALSE--------------------------------
#  optMALS <- Rmalschains::malschains(fn = OptimGR4J,
#                                     lower = lowerGR4J, upper = upperGR4J,
#                                     maxEvals = 2000)

## ----resGLOB, warning=FALSE, echo=FALSE, eval=FALSE---------------------------
#  resGLOB <- data.frame(Algo = c("airGR", "PORT", "DE", "PSO", "MA-LS"),
#                        round(rbind(
#                          OutputsCalib$ParamFinalR,
#                          airGR::TransfoParam_GR4J(ParamIn = optPORT$par                    , Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = as.numeric(optDE$optim$bestmem), Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = as.numeric(optPSO$par)         , Direction = "TR"),
#                          airGR::TransfoParam_GR4J(ParamIn = optMALS$sol                    , Direction = "TR")),
#                          digits = 3))

## ----warning=FALSE, echo=FALSE------------------------------------------------
resGLOB

## ----warning=FALSE, results='hide', eval=FALSE--------------------------------
#  InputsCrit_inv <- InputsCrit
#  InputsCrit_inv$transfo <- "inv"
#  
#  MOptimGR4J <- function(i) {
#    if (algo == "caRamel") {
#      ParamOptim <- x[i, ]
#    }
#    ## Transformation of the parameter set to real space
#    RawParamOptim <- airGR::TransfoParam_GR4J(ParamIn = ParamOptim,
#                                              Direction = "TR")
#    ## Simulation given a parameter set
#    OutputsModel <- airGR::RunModel_GR4J(InputsModel = InputsModel,
#                                         RunOptions = RunOptions,
#                                         Param = RawParamOptim)
#    ## Computation of the value of the performance criteria
#    OutputsCrit1 <- airGR::ErrorCrit_KGE(InputsCrit = InputsCrit,
#                                         OutputsModel = OutputsModel,
#                                         verbose = FALSE)
#    ## Computation of the value of the performance criteria
#    OutputsCrit2 <- airGR::ErrorCrit_KGE(InputsCrit = InputsCrit_inv,
#                                         OutputsModel = OutputsModel,
#                                         verbose = FALSE)
#    return(c(OutputsCrit1$CritValue, OutputsCrit2$CritValue))
#  }

## ----warning=FALSE, results='hide', eval=FALSE--------------------------------
#  algo <- "caRamel"
#  optMO <- caRamel::caRamel(nobj = 2,
#                            nvar = 4,
#                            minmax = rep(TRUE, 2),
#                            bounds = matrix(c(lowerGR4J, upperGR4J), ncol = 2),
#                            func = MOptimGR4J,
#                            popsize = 100,
#                            archsize = 100,
#                            maxrun = 15000,
#                            prec = rep(1.e-3, 2),
#                            carallel = FALSE,
#                            graph = FALSE)

## ----fig.width=6, fig.height=6, warning=FALSE---------------------------------
ggplot() +
  geom_point(aes(optMO$objectives[, 1], optMO$objectives[, 2])) +
  coord_equal(xlim = c(0.4, 0.9), ylim = c(0.4, 0.9)) +
  xlab("KGE") + ylab("KGE [1/Q]") +
  theme_bw()

## ----fig.height=6, fig.width=6, message=FALSE, warning=FALSE------------------
param_optMO <- apply(optMO$parameters, MARGIN = 1, FUN = function(x) {
  airGR::TransfoParam_GR4J(x, Direction = "TR")
  })
GGally::ggpairs(data.frame(t(param_optMO)), diag = NULL) + theme_bw()

## ----fig.height=6, fig.width=12, message=FALSE, warning=FALSE-----------------
RunOptions$Outputs_Sim <- "Qsim"
run_optMO <- apply(optMO$parameters, MARGIN = 1, FUN = function(x) {
  airGR::RunModel_GR4J(InputsModel = InputsModel,
                       RunOptions = RunOptions,
                       Param = x)
  }$Qsim)
run_optMO <- data.frame(run_optMO)

ggplot() +
  geom_line(aes(x = as.POSIXct(InputsModel$DatesR[Ind_Run]),
                y = run_optMO$X1)) +
  geom_line(aes(x = as.POSIXct(InputsModel$DatesR[Ind_Run]),
                y = run_optMO$X54),
            colour = "darkred") +
  scale_x_datetime(limits = c(as.POSIXct("1998-01-01"), NA)) +
  ylab("Discharge [mm/d]") + xlab("Date") +
  scale_y_sqrt() +
  theme_bw()


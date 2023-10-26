## ----include=FALSE, fig.keep='none', results='hide'---------------------------
library(airGR)
library(coda)
library(FME)
library(ggmcmc)
set.seed(123)
load(system.file("vignettesData/vignetteParamMCMC.rda", package = "airGR"))

## ----echo=TRUE, eval=FALSE, eval=FALSE----------------------------------------
#  example("Calibration_Michel")

## ----results='hide', eval=FALSE-----------------------------------------------
#  RunOptions <- airGR::CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
#                                        IndPeriod_Run = Ind_Run,
#                                        Outputs_Sim = "Qsim")

## ----results='hide', eval=FALSE-----------------------------------------------
#  LogLikeGR4J <- function(ParamOptim) {
#    ## Transformation to real space
#    RawParamOptim <- airGR::TransfoParam_GR4J(ParamIn = ParamOptim,
#                                                Direction = "TR")
#    ## Simulation given a parameter set
#    OutputsModel <- airGR::RunModel_GR4J(InputsModel = InputsModel,
#                                         RunOptions = RunOptions,
#                                         Param = RawParamOptim)
#    ## Computation of the log-likelihood: N * log(SS)
#    ObsY <- InputsCrit$Obs
#    ModY <- OutputsModel$Qsim
#    LogLike <- sum(!is.na(ObsY)) * log(sum((ObsY - ModY)^2, na.rm = TRUE))
#  }

## ----results='hide', eval=FALSE-----------------------------------------------
#  optPORT <- stats::nlminb(start = c(4.1, 3.9, -0.9, -8.7),
#                           objective = LogLikeGR4J,
#                           lower = rep(-9.9, times = 4), upper = rep(9.9, times = 4),
#                           control = list(trace = 1))
#  iniParPORT <- optPORT$par

## ----results='hide', eval=FALSE-----------------------------------------------
#  iniParPORT <- data.frame(Chain1 = iniParPORT,
#                           Chain2 = iniParPORT,
#                           Chain3 = iniParPORT,
#                           row.names = paste0("X", 1:4))
#  iniParPORT <- sweep(iniParPORT, MARGIN = 2, STATS = c(1, 0.9, 1.1), FUN = "*")
#  iniParPORT[iniParPORT < -9.9] <- -9.9
#  iniParPORT[iniParPORT > +9.9] <- +9.9
#  
#  mcmcDRAM <- apply(iniParPORT, MARGIN = 2, FUN = function(iIniParPORT) {
#    FME::modMCMC(f            = LogLikeGR4J,
#                 p            = iIniParPORT,
#                 lower        = rep(-9.9, times = 4), ## lower bounds for GR4J
#                 upper        = rep(+9.9, times = 4), ## upper bounds for GR4J
#                 niter        = 2000,
#                 jump         = 0.01,
#                 outputlength = 2000,
#                 burninlength = 0,
#                 updatecov    = 100, ## adaptative Metropolis (AM)
#                 ntrydr       = 2)   ## delayed rejection (RD)
#  })

## ----results='hide', eval=FALSE-----------------------------------------------
#  multDRAM <- coda::as.mcmc.list(lapply(mcmcDRAM, FUN = function(x) {
#    coda::as.mcmc(airGR::TransfoParam_GR4J(as.matrix(x$pars), Direction = "TR"))
#    }))
#  gelRub <- coda::gelman.diag(multDRAM, autoburnin = TRUE)$mpsrf

## -----------------------------------------------------------------------------
gelRub

## ----fig.width=6, fig.height=9, warning=FALSE---------------------------------
parDRAM <- ggmcmc::ggs(multDRAM) ## to convert object for using by all ggs_* graphical functions
ggmcmc::ggs_traceplot(parDRAM)

## ----fig.width=6, fig.height=9, warning=FALSE---------------------------------
burnParDRAM <- parDRAM[parDRAM$Iteration > 1000, ] # to keep only the second half of the series
ggmcmc::ggs_density(burnParDRAM)

## ----fig.width=6, fig.height=6, results='hide'--------------------------------
ggmcmc::ggs_pairs(burnParDRAM, lower = list(continuous = "density"))


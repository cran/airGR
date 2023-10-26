\encoding{UTF-8}


\name{Imax}
\alias{Imax}


\title{Computation of the maximum capacity of the GR5H interception store}


\description{
Function which calculates the maximal capacity of the GR5H interception store. This function compares the interception evapotranspiration from the GR5H interception store for different maximal capacity values with the interception evapotranspiration classically used in the daily GR models (e.g. GR4J). Among all the \code{TestedValues}, the value that gives the closest interception evapotranspiration flux over the whole period is kept.
}


\usage{
Imax(InputsModel, IndPeriod_Run,
     TestedValues = seq(from = 0.1, to = 3, by = 0.1))
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{IndPeriod_Run}{[numeric] index of period to be used for the model run [-]}

\item{TestedValues}{[numeric] vector of tested Imax values [mm]}
}


\value{
Optimal Imax value [mm].
}


\examples{
library(airGR)

## loading catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2006-01-01 00"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2006-12-31 23"))

## Imax computation
Imax <- Imax(InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
             TestedValues = seq(from = 0, to = 3, by = 0.2))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR5H, Imax = Imax,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 706.912, X2 = -0.163, X3 = 188.880, X4 = 2.575, X5 = 0.104)
OutputsModel <- RunModel_GR5H(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Guillaume Thirel, Olivier Delaigue
}


\references{
Ficchi, A. (2017).
  An adaptive hydrological model for multiple time-steps:
  Diagnostics and improvements based on fluxes consistency.
  PhD thesis, UPMC - Irstea Antony, Paris, France.
\cr\cr
Ficchi, A., Perrin, C. and Andréassian, V. (2019).
  Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching.
  Journal of Hydrology, 575, 1308-1327, \doi{10.1016/j.jhydrol.2019.05.084}.
}


\seealso{
\code{\link{RunModel_GR5H}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


\encoding{UTF-8}
\name{RunModel}
\alias{RunModel}
\title{Run with the provided hydrological model function}
\usage{
RunModel(InputsModel, RunOptions, Param, FUN_MOD)
}
\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of model parameters}

\item{FUN_MOD}{[function] hydrological model function (e.g. RunModel_GR4J, RunModel_CemaNeigeGR4J)}
}
\value{
[list] see \code{\link{RunModel_GR4J}} or \code{\link{RunModel_CemaNeigeGR4J}} for details
}
\description{
Function which performs a single model run with the provided function over the selected period.
}
\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="01/01/1990 00:00"), 
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="31/12/1999 00:00"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(734.568, -0.840, 109.809, 1.971)
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param, 
                         FUN_MOD = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}
\author{
Laurent Coron (June 2014)
}
\seealso{
\code{\link{RunModel_GR4J}}, \code{\link{RunModel_CemaNeigeGR4J}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


\encoding{UTF-8}


\name{RunModel_Lag}
\alias{RunModel_Lag}


\title{Run with the Lag model}


\description{
Function which performs a single run for the Lag model over the test period.
}


\usage{
RunModel_Lag(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}]  created with SD model inputs, see \code{\link{CreateInputsModel}} for details. The object should also contain a key \emph{OutputsModel}] of class \code{\link{CreateInputsModel}} coming from the simulation of the downstream subcatchement runoff.}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 1 parameter
  \tabular{ll}{
    Lag \tab Mean flow velocity [m/s]
  }}
}


\value{
[list] see \code{\link{RunModel_GR4J}} or \code{\link{RunModel_CemaNeigeGR4J}} for details.

The list value contains an extra item named \code{QsimDown} which is a copy of InputsModel\$OutputsModel\$Qsim, a numeric series of simulated discharge [mm/time step] related to the run-off contribution of the downstream sub-catchment.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## Simulating a reservoir
# Withdrawing 1 m3/s with an instream flow of 1 m3/s
Qupstream <- matrix(- unlist(lapply(BasinObs$Qls / 1000 - 1, function(x) {
  min(1, max(0,x, na.rm = TRUE))
})), ncol = 1)
# Except between July and November when releasing 3 m3/s
month <- as.numeric(format(BasinObs$DatesR,"\%m"))
Qupstream[month >= 7 & month <= 9] <- 3
# Conversion in m3/day
Qupstream <- Qupstream * 86400

## The reservoir is not an upstream subcachment: its areas is NA
BasinAreas <- c(NA, BasinInfo$BasinArea)

## Delay time between the reservoir and the catchment outlet is 2 days and the distance is 150 km
LenghtHydro <- 150
Lag <- LenghtHydro / 2 / 86400 * 1000 # Conversion km/day -> m/s

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E,
                                 Qupstream = Qupstream, LengthHydro = LenghtHydro,
                                 BasinAreas = BasinAreas)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation of dowstream subcatchment
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModelDown <- RunModel_GR4J(InputsModel = InputsModel,
                                  RunOptions = RunOptions, Param = Param)

InputsModel$OutputsModel <- OutputsModelDown
OutputsModel <- RunModel_Lag(InputsModel = InputsModel,
                             RunOptions = RunOptions, Param = Lag)

## results preview of comparison between naturalised (observed) and influenced flow (simulated)
plot(OutputsModel, Qobs = OutputsModel$QsimDown)
}


\author{
Olivier Delaigue, David Dorchies, Guillaume Thirel
}


\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


\encoding{UTF-8}


\name{RunModel_Lag}
\alias{RunModel_Lag}


\title{Run with the Lag model}


\description{
Function which performs a single run for the Lag model over the test period.
}


\usage{
RunModel_Lag(InputsModel, RunOptions, Param, QcontribDown)
}


\arguments{
  \item{InputsModel}{[object of class \emph{InputsModel}] created with SD model inputs, see \code{\link{CreateInputsModel}} for details. The object should also contain a key \emph{OutputsModel} of class \code{\link{CreateInputsModel}} coming from the simulation of the downstream subcatchment runoff.}

  \item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

  \item{Param}{[numeric] vector of 1 parameter
    \tabular{ll}{
      Velocity \tab mean flow velocity [m/s]
    }
  }
  \item{QcontribDown}{[numeric] vector or [OutputsModel] containing the time series of the runoff contribution of the downstream sub-basin}

}


\value{
[list] see \code{\link{RunModel_GR4J}} or \code{\link{RunModel_CemaNeigeGR4J}} for details.

The list value contains an extra item named \code{QsimDown} which is a copy of the runoff contribution of the downstream sub-basin contained in argument \code{QcontribDown} in [mm/time step].
}


\examples{
#####################################################################
## Simulation of a reservoir with a purpose of low-flow mitigation ##
#####################################################################

## ---- preparation of the InputsModel object

## loading package and catchment data
library(airGR)
data(L0123001)

## ---- simulation of the hydrological catchment with GR4J

InputsModelDown <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                     Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## creation of the RunOptions object
RunOptionsDown <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModelDown, IndPeriod_Run = Ind_Run)

## simulation of the runoff of the catchment with a GR4J model
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModelDown <- RunModel_GR4J(InputsModel = InputsModelDown,
                                  RunOptions = RunOptionsDown, Param = Param)

## ---- specifications of the reservoir

## the reservoir withdraws 1 m3/s when it's possible considering the flow observed in the basin
Qupstream <- matrix(-sapply(BasinObs$Qls / 1000 - 1, function(x) {
  min(1, max(0, x, na.rm = TRUE))
}), ncol = 1)

## except between July and September when the reservoir releases 3 m3/s for low-flow mitigation
month <- as.numeric(format(BasinObs$DatesR, "\%m"))
Qupstream[month >= 7 & month <= 9] <- 3
Qupstream <- Qupstream * 86400 ## Conversion in m3/day

## the reservoir is not an upstream subcachment: its areas is NA
BasinAreas <- c(NA, BasinInfo$BasinArea)

## delay time between the reservoir and the catchment outlet is 2 days and the distance is 150 km
LengthHydro <- 150

## ---- simulation of the basin with the reservoir influence

InputsModelInf <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                    Precip = BasinObs$P, PotEvap = BasinObs$E,
                                    Qupstream = Qupstream, LengthHydro = LengthHydro,
                                    BasinAreas = BasinAreas)
## creation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModelInf, IndPeriod_Run = Ind_Run)

## with a delay of 2 days for 150 km, the flow velocity is 75 km per day
Velocity <- (LengthHydro * 1e3 / 2) / (24 * 60 * 60) ## Conversion km/day -> m/s

## run the lag model which routes precipitation-runoff model and upstream flows
OutputsModel <- RunModel_Lag(InputsModel = InputsModelInf,
                             RunOptions = RunOptions,
                             Param = Velocity,
                             QcontribDown = OutputsModelDown)

## results preview of comparison between naturalised (observed) and influenced flow (simulated)
plot(OutputsModel, Qobs = OutputsModel$QsimDown)
}


\author{
Olivier Delaigue, David Dorchies, Guillaume Thirel
}


\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


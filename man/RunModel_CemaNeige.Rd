\encoding{UTF-8}


\name{RunModel_CemaNeige}
\alias{RunModel_CemaNeige}


\title{Run with the CemaNeige snow module}


\description{
Function which performs a single run for the CemaNeige snow module at the daily or hourly time step.
}


\usage{
RunModel_CemaNeige(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 2 (or 4 parameters if \code{IsHyst = TRUE}, see \code{\link{CreateRunOptions}} for details)
  \tabular{ll}{
    CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-]                   \cr
    CemaNeige X2 \tab degree-day melt coefficient [mm/°C/time step]                           \cr
    CemaNeige X3 \tab (optional) accumulation threshold [mm] (needed if \code{IsHyst = TRUE}) \cr
    CemaNeige X4 \tab (optional) percentage (between 0 and 1) of annual snowfall defining the melt threshold [-] (needed if \code{IsHyst = TRUE}) \cr
  }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR }                                \tab [POSIXlt] series of dates                                            \cr
    \emph{$CemaNeigeLayers}                        \tab [list] CemaNeige outputs (1 element per layer)                       \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Pliq        } \tab [numeric] series of liquid precip. [mm/time step]                    \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Psol        } \tab [numeric] series of solid precip. [mm/time step]                     \cr
    \emph{$CemaNeigeLayers[[iLayer]]$SnowPack    } \tab [numeric] series of snow pack (snow water equivalent) [mm]           \cr
    \emph{$CemaNeigeLayers[[iLayer]]$ThermalState} \tab [numeric] series of snow pack thermal state [°C]                     \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Gratio      } \tab [numeric] series of Gratio [0-1]                                     \cr
    \emph{$CemaNeigeLayers[[iLayer]]$PotMelt     } \tab [numeric] series of potential snow melt [mm/time step]               \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Melt        } \tab [numeric] series of actual snow melt [mm/time step]                  \cr
    \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt } \tab [numeric] series of liquid precip. + actual snow melt [mm/time step] \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Temp        } \tab [numeric] series of air temperature [°C]                             \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Gthreshold  } \tab [numeric] series of melt threshold [mm]                              \cr
    \emph{$CemaNeigeLayers[[iLayer]]$Glocalmax   } \tab [numeric] series of local melt threshold for hysteresis [mm]         \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run: CemaNeige states [mm & °C]. See \code{\link{CreateIniStates}} for more details \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
The choice of the CemaNeige version (i.e. with or without hysteresis) is explained in \code{\link{CreateRunOptions}}. \cr
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## load of catchment data
data(L0123002)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeige, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P,TempMean = BasinObs$T,
                                 ZInputs = BasinInfo$HypsoData[51], HypsoData=BasinInfo$HypsoData,
                                 NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))


## --- original version of CemaNeige

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeige, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(CNX1 = 0.962, CNX2 = 2.249)
OutputsModel <- RunModel_CemaNeige(InputsModel = InputsModel,
                                   RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel)


## --- version of CemaNeige with the Linear Hysteresis

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeige, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run, IsHyst = TRUE)

## simulation
Param <- c(CNX1 = 0.962, CNX2 = 2.249, CNX3 = 100, CNX4 = 0.4)
OutputsModel <- RunModel_CemaNeige(InputsModel = InputsModel,
                                   RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel)
}


\author{
Laurent Coron, Audrey Valéry, Vazken Andréassian, Olivier Delaigue, Guillaume Thirel
}


\references{
Riboust, P., Thirel, G., Le Moine, N. and Ribstein, P. (2019).
  Revisiting a simple degree-day model for integrating satellite data: Implementation of SWE-SCA hystereses.
  Journal of Hydrology and Hydromechanics, 67(1), 70–81, \doi{10.2478/johh-2018-0004}.
\cr\cr
Valéry, A., Andréassian, V. and Perrin, C. (2014).
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine?
  Part 1 - Comparison of six snow accounting routines on 380 catchments.
  Journal of Hydrology, 517(0), 1166-1175, \doi{10.1016/j.jhydrol.2014.04.059}.
\cr\cr
Valéry, A., Andréassian, V. and Perrin, C. (2014).
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine?
  Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments.
  Journal of Hydrology, 517(0), 1176-1187, \doi{10.1016/j.jhydrol.2014.04.058}.
}


\seealso{
\code{\link{RunModel_CemaNeigeGR4J}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}},
\code{\link{CreateIniStates}}, \code{\link{CreateCalibOptions}}.
}


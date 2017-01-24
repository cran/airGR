\encoding{UTF-8}
\name{CreateInputsModel}
\alias{CreateInputsModel}
\title{Creation of the InputsModel object required to the RunModel functions}
\usage{
CreateInputsModel(FUN_MOD, DatesR, Precip, PrecipScale = TRUE, PotEvap = NULL,
  TempMean = NULL, TempMin = NULL, TempMax = NULL, ZInputs = NULL, HypsoData = NULL,
  NLayers = 5, verbose = TRUE)
}
\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. \code{RunModel_GR4J}, \code{RunModel_CemaNeigeGR4J})}

\item{DatesR}{[POSIXt] vector of dates required to create the GR model and CemaNeige module inputs}

\item{Precip}{[numeric] time series of total precipitation (catchment average) [mm], required to create the GR model and CemaNeige module inputs}

\item{PrecipScale}{(optional) [boolean] indicating if the mean of the precipitation interpolated on the elevation layers must be kept or not, required to create CemaNeige module inputs, default = \code{TRUE} (the mean of the precipitation is kept to the original value)}

\item{PotEvap}{[numeric] time series of potential evapotranspiration (catchment average) [mm], required to create the GR model inputs}

\item{TempMean}{(optional) [numeric] time series of mean air temperature [°C], required to create the CemaNeige module inputs}

\item{TempMin}{(optional) [numeric] time series of min air temperature [°C], possibly used to create the CemaNeige module inputs}

\item{TempMax}{(optional) [numeric] time series of max air temperature [°C], possibly used to create the CemaNeige module inputs}

\item{ZInputs}{(optional) [numeric] real giving the mean elevation of the Precip and Temp series (before extrapolation) [m], possibly used to create the CemaNeige module inputs}

\item{HypsoData}{(optional) [numeric] vector of 101 reals: min, q01 to q99 and max of catchment elevation distribution [m], if not defined a single elevation is used for CemaNeige}

\item{NLayers}{(optional) [numeric] integer giving the number of elevation layers requested [-], required to create CemaNeige module inputs, default=5}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default= \code{TRUE}}
}
\value{
[list] object of class \emph{InputsModel} containing the data required to evaluate the model outputs; it can include the following:
         \tabular{ll}{
         \emph{$DatesR              }  \tab   [POSIXlt] vector of dates \cr
         \emph{$Precip              }  \tab   [numeric] time series of total precipitation (catchment average) [mm] \cr
         \emph{$PotEvap             }  \tab   [numeric] time series of potential evapotranspiration (catchment average) [mm], \cr\tab defined if FUN_MOD includes GR4H, GR4J, GR5J, GR6J, GR2M or GR1A \cr \cr
         \emph{$LayerPrecip         }  \tab   [list] list of time series of precipitation (layer average) [mm], \cr\tab defined if  \code{FUN_MOD} includes CemaNeige \cr \cr
         \emph{$LayerTempMean       }  \tab   [list] list of time series of mean air temperature (layer average) [°C], \cr\tab defined if  \code{FUN_MOD} includes CemaNeige \cr \cr
         \emph{$LayerFracSolidPrecip}  \tab   [list] list of time series of solid precipitation fraction (layer average) [-], \cr\tab defined if  \code{FUN_MOD} includes CemaNeige \cr \cr
         }
}
\description{
Creation of the InputsModel object required to the RunModel functions.
}
\details{
Users wanting to use  \code{FUN_MOD} functions that are not included in 
the package must create their own InputsModel object accordingly.
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
\code{\link{RunModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}, \code{\link{DataAltiExtrapolation_Valery}}
}


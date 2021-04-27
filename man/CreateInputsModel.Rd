\encoding{UTF-8}


\name{CreateInputsModel}
\alias{CreateInputsModel}
\alias{[.InputsModel}


\title{Creation of the InputsModel object required to the RunModel functions}


\description{
Creation of the \emph{InputsModel} object required to the \code{RunModel*} functions.
}


\usage{
CreateInputsModel(FUN_MOD, DatesR, Precip, PrecipScale = TRUE, PotEvap = NULL,
                  TempMean = NULL, TempMin = NULL, TempMax = NULL,
                  ZInputs = NULL, HypsoData = NULL, NLayers = 5,
                  Qupstream = NULL, LengthHydro = NULL, BasinAreas = NULL,
                  QupstrUnit = "mm", verbose = TRUE)

\method{[}{InputsModel}(x, i)
}


\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. \code{RunModel_GR4J}, \code{RunModel_CemaNeigeGR4J})}

\item{DatesR}{[POSIXt] vector of dates required to create the GR model and CemaNeige module inputs}

\item{Precip}{[numeric] time series of total precipitation (catchment average) [mm/time step], required to create the GR model and CemaNeige module inputs}

\item{PrecipScale}{(optional) [boolean] indicating if the mean of the precipitation interpolated on the elevation layers must be kept or not, required to create CemaNeige module inputs, default = \code{TRUE} (the mean of the precipitation is kept to the original value)}

\item{PotEvap}{[numeric] time series of potential evapotranspiration (catchment average) [mm/time step], required to create the GR model inputs}

\item{TempMean}{(optional) [numeric] time series of mean air temperature [°C], required to create the CemaNeige module inputs}

\item{TempMin}{(optional) [numeric] time series of min air temperature [°C], possibly used to create the CemaNeige module inputs}

\item{TempMax}{(optional) [numeric] time series of max air temperature [°C], possibly used to create the CemaNeige module inputs}

\item{ZInputs}{(optional) [numeric] real giving the mean elevation of the Precip and Temp series (before extrapolation) [m], possibly used to create the CemaNeige module inputs}

\item{HypsoData}{(optional) [numeric] vector of 101 reals: min, q01 to q99 and max of catchment elevation distribution [m], if not defined a single elevation is used for CemaNeige}

\item{NLayers}{(optional) [numeric] integer giving the number of elevation layers requested [-], required to create CemaNeige module inputs, default=5}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}

\item{Qupstream}{(optional) [numerical matrix] time series of upstream flows (catchment average), its unit is defined by the \code{QupstrUnit} parameter, required to create the SD model inputs. See details}

\item{LengthHydro}{(optional) [numeric] real giving the distance between the downstream outlet and each upstream inlet of the sub-catchment [km], required to create the SD model inputs . See details}

\item{BasinAreas}{(optional) [numeric] real giving the area of each upstream sub-catchment [km2] and the area of the downstream sub-catchment in the last item, required to create the SD model inputs . See details}

\item{QupstrUnit}{(optional) [character] unit of the flow in the argument \code{Qupstream}, available units are: "mm" for mm/time-step (default), "m3" for m3/time-step, "m3/s" and "l/s". See details}

\item{x}{[InputsModel] object of class InputsModel}

\item{i}{[integer] of the indices to subset a time series or [character] names of the elements to extract}
}


\value{
[list] object of class \emph{InputsModel} containing the data required to evaluate the model outputs; it can include the following:
  \tabular{ll}{
    \emph{$DatesR              } \tab [POSIXlt] vector of dates \cr
    \emph{$Precip              } \tab [numeric] time series of total precipitation (catchment average) [mm/time step] \cr
    \emph{$PotEvap             } \tab [numeric] time series of potential evapotranspiration (catchment average) [mm/time step], \cr\tab defined if FUN_MOD includes GR4H, GR5H, GR4J, GR5J, GR6J, GR2M or GR1A \cr \cr
    \emph{$LayerPrecip         } \tab [list] list of time series of precipitation (layer average) [mm/time step], \cr\tab defined if \code{FUN_MOD} includes CemaNeige \cr \cr
    \emph{$LayerTempMean       } \tab [list] list of time series of mean air temperature (layer average) [°C], \cr\tab defined if \code{FUN_MOD} includes CemaNeige \cr \cr
    \emph{$LayerFracSolidPrecip} \tab [list] list of time series of solid precipitation fraction (layer average) [-], \cr\tab defined if \code{FUN_MOD} includes CemaNeige \cr \cr
         }
}


\details{
Users wanting to use \code{FUN_MOD} functions that are not included in
the package must create their own InputsModel object accordingly. \cr
Please note that if CemaNeige is used, and \code{ZInputs} is different than \code{HypsoData}, then precipitation and temperature are interpolated with the \code{DataAltiExtrapolation_Valery} function.

Users wanting to use a semi-distributed (SD) model should provide valid \code{Qupstream}, \code{LengthHydro}, and \code{BasinAreas} arguments. Each upstream sub-catchment is described by an upstream flow time series (one column in \code{Qupstream} matrix), a distance between the downstream outlet and the upstream inlet (one item in \code{LengthHydro}) and an area (one item in \code{BasinAreas}).
The order of the columns or of the items should be consistent for all these parameters.
\code{BasinAreas} should contain one extra information (stored in the last item) which is the area of the downstream sub-catchment.
Upstream flows that are not related to a sub-catchment such as release or withdraw spots are identified by an area equal to \code{NA}, and if \code{unit="mm"} the upstream flow must be expressed in m3/time step instead of mm/time step which is not possible in absence of a related area.
Please note that the use of SD model requires to use the \code{\link{RunModel}} function instead of \code{\link{RunModel_GR4J}} or the other \code{RunModel_*} functions.
}

\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 734.568, X2 = -0.840, X3 = 109.809, X4 = 1.971)
OutputsModel <- RunModel(InputsModel = InputsModel,
                         RunOptions = RunOptions, Param = Param,
                         FUN_MOD = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron
}


\seealso{
\code{\link{RunModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateInputsCrit}},
\code{\link{CreateCalibOptions}}, \code{\link{DataAltiExtrapolation_Valery}}
}

\encoding{UTF-8}


\name{DataAltiExtrapolation_Valery}
\alias{DataAltiExtrapolation_Valery}


\title{Altitudinal extrapolation of precipitation and temperature series described by A. Valery}


\description{
Function which extrapolates the precipitation and air temperature series for different elevation layers (method from Valéry, 2010).
}


\usage{
DataAltiExtrapolation_Valery(DatesR, Precip, PrecipScale = TRUE,
                             TempMean, TempMin = NULL, TempMax = NULL,
                             ZInputs, HypsoData, NLayers, verbose = TRUE)
}


\arguments{
\item{DatesR}{[POSIXt] vector of dates}

\item{Precip}{[numeric] time series of daily total precipitation (catchment average) [mm/time step]}

\item{PrecipScale}{(optional) [boolean] indicating if the mean of the precipitation interpolated on the elevation layers must be kept or not, required to create CemaNeige module inputs, default = \code{TRUE} (the mean of the precipitation is kept to the original value)}

\item{TempMean}{[numeric] time series of daily mean air temperature [°C]}

\item{TempMin}{(optional) [numeric] time series of daily min air temperature [°C]}

\item{TempMax}{(optional) [numeric] time series of daily max air temperature [°C]}

\item{ZInputs}{[numeric] real giving the mean elevation of the Precip and Temp series (before extrapolation) [m]}

\item{HypsoData}{[numeric] vector of 101 reals: min, q01 to q99 and max of catchment elevation distribution [m]}

\item{NLayers}{[numeric] integer giving the number of elevation layers requested [-]}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}
}
\value{
list containing the extrapolated series of precip. and air temp. on each elevation layer
         \tabular{ll}{
           \emph{$LayerPrecip         } \tab [list] list of time series of daily precipitation (layer average) [mm/time step] \cr
           \emph{$LayerTempMean       } \tab [list] list of time series of daily mean air temperature (layer average) [°C]    \cr
           \emph{$LayerTempMin        } \tab [list] list of time series of daily min air temperature (layer average) [°C]     \cr
           \emph{$LayerTempMax        } \tab [list] list of time series of daily max air temperature (layer average) [°C]     \cr
           \emph{$LayerFracSolidPrecip} \tab [list] list of time series of daily solid precip. fract. (layer average) [-]     \cr
           \emph{$ZLayers             } \tab [numeric] vector of median elevation for each layer                              \cr
         }
}


\details{
Elevation layers of equal surface are created the 101 elevation quantiles (\code{HypsoData})
and the number requested elevation layers (\code{NLayers}). \cr
Forcing data (precipitation and air temperature) are extrapolated using gradients from Valery (2010).
(e.g. gradP = 0.0004 [m-1] for France and gradT = 0.434 [°C/100m] for January, 1st). \cr
This function is used by the \code{\link{CreateInputsModel}} function.
}


\author{
Laurent Coron, Audrey Valéry, Olivier Delaigue, Pierre Brigode, Guillaume Thirel
}


\references{
Turcotte, R., Fortin, L.-G., Fortin, V., Fortin, J.-P. and Villeneuve, J.-P. (2007).
  Operational analysis of the spatial distribution and the temporal evolution of the snowpack water equivalent in southern Quebec, Canada.
  Nordic Hydrology, 38(3), 211, \doi{10.2166/nh.2007.009}.
\cr\cr
Valéry, A. (2010),
  Modélisation précipitations-débit sous influence nivale ? : Elaboration d'un module neige et évaluation sur 380 bassins versants.
  PhD thesis (in French), AgroParisTech - Cemagref Antony, Paris, France.
\cr\cr
USACE (1956),
  Snow Hydrology, pp. 437.
  U.S. Army Coprs of Engineers (USACE) North Pacific Division, Portland, Oregon, USA.
}


\seealso{
\code{\link{CreateInputsModel}}, \code{\link{RunModel_CemaNeigeGR4J}}
}


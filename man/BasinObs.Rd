\docType{data}
\encoding{UTF-8}


\name{BasinObs}
\alias{BasinObs}
\alias{L0123001}
\alias{L0123002}
\alias{L0123003}
\alias{X0310010}


\title{Data sample: time series of observations of different catchments}



\description{
L0123001, L0123002 or L0123003 are fictional catchments.
\cr
X0310010 contains actual data from the Durance River at Embrun [La Clapière] (Hautes-Alpes, France).
The flows are provided by Electricity of France (EDF) and were retrieved from the Banque Hydro database (http://www.hydro.eaufrance.fr).
The meteorological forcing are derived from the SAFRAN reanalysis from Météo-France (Vidal et al., 2010).
\cr
R-object containing the times series of precipitation, temperature, potential evapotranspiration and discharge.
X0310010 contains in addition MODIS snow cover area (SCA) data retrieved from the National Snow and Ice Data Center (NSIDC) repository (https://nsidc.org/). Five SCA time series are given, corresponding to 5 elevation bands of the CemaNeige model (default configuration). SCA data for days with important cloudiness (> 40 \%) were set to missing values for the sake of data representativeness. .
\cr \cr
Times series for L0123001, L0123002 and X0310010 are at the daily time step for use with daily models such as GR4J, GR5J, GR6J, CemaNeigeGR4J, CemaNeigeGR5J and CemaNeigeGR6J. \cr
Times series for X0310010 are provided in order to test hysteresis version of CemaNeige (see \code{\link{CreateRunOptions}} (Riboust et al., 2019). \cr
Times series for L0123003 are at the hourly time step for use with hourly models such as GR4H or GR5H.
}


\format{Data frame named 'BasinObs' containing
\itemize{
\item one POSIXct vector: time series dates in the POSIXct format
\item five numeric vectors: time series of catchment average precipitation [mm/time step], catchment average air temperature [°C], catchment average potential evapotranspiration [mm/time step], outlet discharge [l/s], outlet discharge [mm/time step]
}}


\references{
Riboust, P., Thirel, G., Le Moine, N. and Ribstein P. (2019).
  Revisiting a simple degree-day model for integrating satellite data: Implementation of SWE-SCA hystereses.
  Journal of Hydrology and Hydromechanics, 67(1), 70–81, \doi{10.2478/johh-2018-0004}.
\cr\cr
Vidal, J.-P., Martin, E., Franchistéguy, L., Baillon, M. and Soubeyroux, J. (2010).
  A 50-year high-resolution atmospheric reanalysis over France with the Safran system.
  International Journal of Climatology, 30, 1627–1644, \doi{10.1002/joc.2003}.
}


\seealso{
  \code{\link{BasinInfo}}.
}


\examples{
library(airGR)
data(L0123001)
str(BasinObs)
}


\docType{data}
\encoding{UTF-8}
\name{BasinObs}
\alias{BasinObs}
\title{Data sample: time series of observations of a fictional catchment (L0123001, L0123002 or L0123003)}
\format{Data frame named 'BasinObs' containing
\itemize{
\item one POSIXct vector: time series dates in the POSIXct format
\item five numeric vectors: time series of catchment average precipitation [mm/time step], catchment average air temperature [°C], catchment average potential evapotranspiration [mm/time step], outlet discharge [l/s], outlet discharge [mm/time step]
}}
\description{
R-object containing the times series of precipitation, temperature, potential evapotranspiration and discharges. \cr
Times series for L0123001 or L0123002 are at the daily time step for use with daily models such as GR4J, GR5J, GR6J, CemaNeigeGR4J, CemaNeigeGR5J and CemaNeigeGR6J.
Times series for L0123003 are at the hourly time step for use with hourly models such as GR4H.
}
\examples{
   library(airGR)
   data(L0123001)
   str(BasinObs)
}


\docType{data}
\encoding{UTF-8}
\name{BasinInfo}
\alias{BasinInfo}
\title{Data sample: characteristics of a fictional catchment (L0123001, L0123002 or L0123003)}
\format{List named 'BasinInfo' containing
\itemize{
\item two strings: catchment's code and station's name
\item one float: catchment's area in km2
\item one numeric vector: catchment's hypsometric curve (min, quantiles 01 to 99 and max) in metres
}}
\description{
R-object containing the code, station's name, area and hypsometric curve of the catchment.
}
\examples{
   library(airGR)
   data(L0123001)
   str(BasinInfo)
}


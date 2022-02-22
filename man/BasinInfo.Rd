\docType{data}
\encoding{UTF-8}


\name{BasinInfo}
\alias{BasinInfo}


\title{Data sample: characteristics of a different catchments}


\description{
L0123001, L0123002 and L0123003 are fictional catchments. \cr
X0310010 contains actual data from the Durance River at Embrun [La Clapière] (Hautes-Alpes, France). \cr
\cr
R-object containing the code, station's name, area and hypsometric curve of the catchment.
}


\format{List named 'BasinInfo' containing
\itemize{
\item two strings: catchment's code and station's name
\item one float: catchment's area in km2
\item one numeric vector: catchment's hypsometric curve (min, quantiles 01 to 99 and max) in metres
}}


\seealso{
  \code{\link{BasinObs}}.
}


\examples{
library(airGR)
data(L0123001)
str(BasinInfo)
}


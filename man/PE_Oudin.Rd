\encoding{UTF-8}


\name{PE_Oudin}
\alias{PE_Oudin}


\title{Computation of series of potential evapotranspiration at the daily or hourly time steps with Oudin's formula}


\usage{
PE_Oudin(JD, Temp, Lat, LatUnit =  c("rad", "deg"),
         TimeStepIn = "daily", TimeStepOut = "daily",
         RunFortran = FALSE)
}


\arguments{
\item{JD}{[numeric] time series of Julian day of the year [-]; see details}

\item{Temp}{[numeric] time series of daily (or hourly) mean air temperature [°C]}

\item{Lat}{[numeric] latitude of measurement for the temperature series [radians or degrees]. Atomic vector, except if \code{RunFortran = TRUE}, it can be a vector of the same length as \code{Temp}}

\item{LatUnit}{[character] latitude unit (default = \code{"rad"} or \code{"deg"})}

\item{TimeStepIn}{[character] time step of inputs (e.g. \code{"daily"} or \code{"hourly"}, default = \code{"daily"})}

\item{TimeStepOut}{[character] time step of outputs (e.g. \code{"daily"} or \code{"hourly"}, default = \code{"daily"})}

\item{RunFortran}{[boolean] to run the code in the Fortran mode or in the R mode (default)}
}


\value{
[numeric] time series of daily potential evapotranspiration [mm/time step]
}


\description{
Function which computes PE using the formula from Oudin et al. (2005). PE can be computed at the daily time step from hourly or daily temperature and at the hourly time step with hourly or daily temperature through a disaggregation of daily PE . See details.
}

\details{
To calculate basin-wide Oudin potential evapotranspiration, it is advised, when possible, to use either station temperature or gridded temperature data to calculate PE and then average these PE values at the basin scale.

In the \code{JD} argument, the Julian day of the year of the 1st of January is equal to 1 and the 31st of December to 365 (366 in leap years). If the Julian day of the year is computed on an object of the \code{POSIXlt} class, the user has to add 1 to the returned value (e.g. \code{as.POSIXlt("2016-12-31")$yday + 1}).

When hourly temperature is provided, all the values of the same day have to be set to the same Julian day of the year (e.g. \code{as.POSIXlt("2016-12-31 00:00:00")$yday + 1} and \code{as.POSIXlt("2016-12-31 00:01:00")$yday + 1}). Each single day must be provided 24 identical Julian day values (one for each hour).

Four cases are possible:
\itemize{
\item \code{TimeStepIn = "daily"} and \code{TimeStepOut = "daily"}: this is the classical application of the Oudin et al. (2005) formula
\item \code{TimeStepIn = "daily"} and \code{TimeStepOut = "hourly"}: the daily temperature is used inside the \code{PE_Oudin} function to calculate daily PE, which is then disaggregated at the hourly time step with use of a sinusoidal function (see Lobligeois, 2014, p. 78)
\item \code{TimeStepIn = "hourly"} and \code{TimeStepOut = "daily"}: the hourly temperature is aggregated at the daily time step and the daily PE is calculated normally within \code{PE_Oudin}
\item \code{TimeStepIn = "hourly"} and \code{TimeStepOut = "hourly"}: the hourly temperature is aggregated at the daily time step, the daily PE is then calculated normally within \code{PE_Oudin}, which is finally disaggregated at the hourly time step with use of a sinusoidal function (see Lobligeois, 2014, p. 78)
}

The use of the \code{PEdaily_Oudin} corresponds to the first case of the use of \code{PE_Oudin}.
}

\examples{
library(airGR)
data(L0123001)
PotEvap <- PE_Oudin(JD = as.POSIXlt(BasinObs$DatesR)$yday + 1,
                    Temp = BasinObs$T,
                    Lat = 0.8, LatUnit = "rad")
}


\author{
Laurent Coron, Ludovic Oudin, Olivier Delaigue, Guillaume Thirel, François Bourgin
}


\references{
Oudin, L., Hervieu, F., Michel, C., Perrin, C., Andréassian, V., Anctil, F. and Loumagne, C. (2005).
  Which potential evapotranspiration input for a lumped rainfall-runoff model?
  Part 2 - Towards a simple and efficient potential evapotranspiration model for rainfall-runoff modelling.
  Journal of Hydrology, 303(1-4), 290-306, \doi{10.1016/j.jhydrol.2004.08.026}.
\cr\cr
Lobligeois, F. (2014).
  Mieux connaitre la distribution spatiale des pluies améliore-t-il la modélisation des crues ? Diagnostic sur 181 bassins versants français.
  PhD thesis (in French), AgroParisTech - Irstea Antony, Paris, France.
}


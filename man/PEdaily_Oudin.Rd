\encoding{UTF-8}


\name{PEdaily_Oudin}
\alias{PEdaily_Oudin}


\title{Computation of daily series of potential evapotranspiration with Oudin's formula}


\usage{
PEdaily_Oudin(JD, Temp, LatRad)
}


\arguments{
\item{JD}{[numeric] time series of julian day [-]}

\item{Temp}{[numeric] time series of daily mean air temperature [°C]}

\item{LatRad}{[numeric] latitude of measurement for the temperature series [rad]}
}


\value{
[numeric] time series of daily potential evapotranspiration [mm/d]
}


\description{
Function which computes daily PE using the formula from Oudin et al. (2005).
}


\examples{
   library(airGR)
   data(L0123001)
   PotEvap <- PEdaily_Oudin(JD = as.POSIXlt(BasinObs$DatesR)$yday, Temp = BasinObs$T, LatRad = 0.8)
}


\author{
Laurent Coron, Ludovic Oudin
}


\references{
Oudin, L., F. Hervieu, C. Michel, C. Perrin, V. Andréassian, F. Anctil and C. Loumagne (2005),
      Which potential evapotranspiration input for a lumped rainfall-runoff model?: Part 2-Towards a 
      simple and efficient potential evapotranspiration model for rainfall-runoff modelling, Journal of Hydrology, 
      303(1-4), 290-306, doi:10.1016/j.jhydrol.2004.08.026.
}


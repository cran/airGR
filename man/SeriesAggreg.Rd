\encoding{UTF-8}
\name{SeriesAggreg}
\alias{SeriesAggreg}
\title{Conversion of time series to another time step (aggregation only)}
\usage{
SeriesAggreg(TabSeries, TimeFormat, NewTimeFormat, ConvertFun,
  YearFirstMonth = 1, TimeLag = 0, verbose = TRUE)
}
\arguments{
\item{TabSeries}{[POSIXt+numeric] data.frame containing the vector of dates (POSIXt) and the time series values numeric)}

\item{TimeFormat}{[character] desired format (i.e. "hourly", "daily", "monthly" or "yearly")}

\item{NewTimeFormat}{[character] desired format (i.e. "hourly", "daily", "monthly" or "yearly")}

\item{ConvertFun}{[character] names of aggregation functions (e.g. for P[mm], T[deg], Q[mm] : ConvertFun <- c("sum", "mean", "sum"))}

\item{YearFirstMonth}{(optional) [numeric] integer used when NewTimeFormat = "yearly" to set when the starting month of the year (e.g. 01 for calendar year or 09 for hydrological year starting in September)}

\item{TimeLag}{(optional) [numeric] numeric indicating a time lag (in seconds) for the time series aggregation (especially useful to aggregate hourly time series in daily time series)}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = FALSE}
}
\value{
[POSIXct+numeric] data.frame containing a vector of aggregated dates (POSIXct) and time series values numeric)
}
\description{
Conversion of time series to another time step (aggregation only). \cr
Warning : on the aggregated outputs, the dates correspond to the beginning of the time step \cr
(e.g. for daily time-series 01/03/2005 00:00 = value for period 01/03/2005 00:00 - 01/03/2005 23:59) \cr
(e.g. for monthly time-series 01/03/2005 00:00 = value for period 01/03/2005 00:00 - 31/03/2005 23:59) \cr
(e.g. for yearly time-series 01/03/2005 00:00 = value for period 01/03/2005 00:00 - 28/02/2006 23:59)
}
\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the initial time series data frame at the daily time step
TabSeries  <- data.frame(BasinObs$DatesR, BasinObs$P, BasinObs$E, BasinObs$T, BasinObs$Qmm)
TimeFormat <- "daily"

## conversion at the monthly time step
NewTimeFormat <- "monthly"
ConvertFun    <- c("sum", "sum", "mean", "sum")
NewTabSeries  <- SeriesAggreg(TabSeries = TabSeries, TimeFormat, NewTimeFormat, ConvertFun)

## conversion at the yearly time step
NewTimeFormat <- "yearly"
ConvertFun <- c("sum", "sum", "mean", "sum")
NewTabSeries <- SeriesAggreg(TabSeries = TabSeries, TimeFormat, NewTimeFormat, ConvertFun)

}
\author{
Laurent Coron (March 2015)
}


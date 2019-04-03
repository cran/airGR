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

\item{TimeFormat}{[character] desired format (i.e. \code{"hourly"}, \code{"daily"}, \code{"monthly"} or \code{"yearly"})}

\item{NewTimeFormat}{[character] desired format (i.e. \code{"hourly"}, \code{"daily"}, \code{"monthly"} or \code{"yearly"})}

\item{ConvertFun}{[character] names of aggregation functions (e.g. for P[mm], T[degC], Q[mm] : \code{ConvertFun = c("sum", "mean", "sum"}))}

\item{YearFirstMonth}{(optional) [numeric] integer used when \code{NewTimeFormat = "yearly"} to set when the starting month of the year (e.g. 01 for calendar year or 09 for hydrological year starting in September)}

\item{TimeLag}{(optional) [numeric] numeric indicating a time lag (in seconds) for the time series aggregation (especially useful to aggregate hourly time series in daily time series)}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{FALSE}}
}
\value{
[POSIXct+numeric] data.frame containing a vector of aggregated dates (POSIXct) and time series values numeric)
}


\description{
Conversion of time series to another time step (aggregation only). \cr
Warning : on the aggregated outputs, the dates correspond to the beginning of the time step \cr
(e.g. for daily time-series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2005-03-01 23:59) \cr
(e.g. for monthly time-series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2005-03-31 23:59) \cr
(e.g. for yearly time-series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2006-02-28 23:59)
}


\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the initial time series data frame at the daily time step
TabSeries <- BasinObs[, c("DatesR", "P", "E", "T", "Qmm")]

## conversion at the monthly time step
NewTabSeries <- SeriesAggreg(TabSeries = TabSeries,
                             TimeFormat = "daily", NewTimeFormat = "monthly",
                             ConvertFun = c("sum", "sum", "mean", "sum"))

## conversion at the yearly time step
NewTabSeries <- SeriesAggreg(TabSeries = TabSeries,
                             TimeFormat = "daily", NewTimeFormat = "yearly",
                             ConvertFun = c("sum", "sum", "mean", "sum"))

}


\author{
Laurent Coron
}


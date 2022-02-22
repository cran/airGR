\encoding{UTF-8}


\name{SeriesAggreg}
\alias{SeriesAggreg}
\alias{SeriesAggreg.list}
\alias{SeriesAggreg.data.frame}
\alias{SeriesAggreg.InputsModel}
\alias{SeriesAggreg.OutputsModel}


\title{Conversion of time series to another time step (aggregation only) and regime computation}


\description{
Conversion of time series to another time step (aggregation only) and regime computation. \cr
Warning: on the aggregated outputs, the dates correspond to the beginning of the time step \cr
(e.g. for daily time series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2005-03-01 23:59) \cr
(e.g. for monthly time series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2005-03-31 23:59) \cr
(e.g. for yearly time series 2005-03-01 00:00 = value for period 2005-03-01 00:00 - 2006-02-28 23:59)
}


\details{
  \code{\link{SeriesAggreg.InputsModel}} and \code{\link{SeriesAggreg.OutputsModel}}
  call \code{\link{SeriesAggreg.list}} which itself calls \code{\link{SeriesAggreg.data.frame}}.
  So, all arguments passed to any \code{\link{SeriesAggreg}} method will be passed to \code{\link{SeriesAggreg.data.frame}}.

  Argument \code{ConvertFun} also supports quantile calculation by using the syntax "Q[nn]" with [nn] the requested percentile.
  E.g. use "Q90" for calculating 90th percentile in the aggregation.
  The formula used is: \code{quantile(x, probs = perc / 100, type = 8, na.rm = TRUE)}.

  As there are multiple ways to take into account missing values in aggregation functions, \code{NA}s are not supported by \code{SeriesAggreg} and it provides \code{NA} values when \code{NA}s are present in the \code{x} input.

}


\usage{
\method{SeriesAggreg}{data.frame}(x,
             Format,
             ConvertFun,
             TimeFormat = NULL,
             NewTimeFormat = NULL,
             YearFirstMonth = 1,
             TimeLag = 0,
             \dots)

\method{SeriesAggreg}{list}(x,
             Format,
             ConvertFun,
             NewTimeFormat = NULL,
             simplify = FALSE,
             except = NULL,
             recursive = TRUE,
             \dots)

\method{SeriesAggreg}{InputsModel}(x, Format, \dots)

\method{SeriesAggreg}{OutputsModel}(x, Format, \dots)
}


\arguments{
\item{x}{[InputsModel], [OutputsModel], [list] or [data.frame] containing the vector of dates (\emph{POSIXt}) and the time series of numeric values}

\item{Format}{[character] output time step format (i.e. yearly times series: \code{"\%Y"}, monthly time series: \code{"\%Y\%m"}, daily time series: \code{"\%Y\%m\%d"}, monthly regimes: \code{"\%m"}, daily regimes: \code{"\%d"})}

\item{TimeFormat}{(deprecated) [character] input time step format (i.e. \code{"hourly"}, \code{"daily"}, \code{"monthly"} or \code{"yearly"}). Use the \code{x} argument instead}

\item{NewTimeFormat}{(deprecated) [character] output time step format (i.e. \code{"hourly"}, \code{"daily"}, \code{"monthly"} or \code{"yearly"}). Use the \code{Format} argument instead}

\item{ConvertFun}{[character] names of aggregation functions (e.g. for P[mm], T[degC], Q[mm]: \code{ConvertFun = c("sum", "mean", "sum"})) or name of aggregation function to apply to all elements if the parameter 'x' is a [list] . See details}

\item{YearFirstMonth}{(optional) [numeric] integer used when \code{Format = "\%Y"} to set when the starting month of the year (e.g. 01 for calendar year or 09 for hydrological year starting in September)}

\item{TimeLag}{(optional) [numeric] numeric indicating a time lag (in seconds) for the time series aggregation (especially useful to aggregate hourly time series into daily time series)}

\item{simplify}{(optional) [boolean] if set to \code{TRUE}, a \code{\link{data.frame}} is returned instead of a \code{\link{list}}. Embedded lists are then ignored. (default = \code{FALSE})}

\item{except}{(optional) [character] the name of the items to skip in the aggregation (default = \code{NULL})}

\item{recursive}{(optional) [boolean] if set to \code{FALSE}, embedded lists and dataframes are not aggregated (default = \code{TRUE})}

\item{\dots}{Arguments passed to \code{\link{SeriesAggreg.list}} and then to \code{\link{SeriesAggreg.data.frame}}}
}

\value{
[POSIXct+numeric] data.frame containing a vector of aggregated dates (POSIXct) and time series values numeric)
}


\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the initial time series data frame at the daily time step
TabSeries <- BasinObs[, c("DatesR", "P", "E", "T", "Qmm")]

## monthly time series
NewTabSeries <- SeriesAggreg(TabSeries,
                             Format = "\%Y\%m",
                             ConvertFun = c("sum", "sum", "mean", "sum"))
str(NewTabSeries)

## monthly regimes
NewTabSeries <- SeriesAggreg(TabSeries,
                             Format = "\%m",
                             ConvertFun = c("sum", "sum", "mean", "sum"))
str(NewTabSeries)

## conversion of InputsModel
example("RunModel_GR2M")

## monthly regimes on OutputsModel object
SimulatedMonthlyRegime <- SeriesAggreg(OutputsModel, Format = "\%m")
str(SimulatedMonthlyRegime)

}


\author{
Olivier Delaigue, David Dorchies
}


\encoding{UTF-8}
\name{plot.OutputsModel}
\alias{plot.OutputsModel}
\alias{plot_OutputsModel}
\title{Default preview of model outputs}
\usage{
\method{plot}{OutputsModel}(x, Qobs = NULL, IndPeriod_Plot = NULL,
  BasinArea = NULL, which = "all", log_scale = FALSE, verbose = TRUE, ...)
}
\arguments{
\item{x}{[object of class \emph{OutputsModel}] list of model outputs (which must at least include DatesR, Precip and Qsim) [POSIXlt, mm, mm]}

\item{Qobs}{(optional) [numeric] time series of observed flow (for the same time steps than simulated) [mm/time step]}

\item{IndPeriod_Plot}{(optional) [numeric] indices of the time steps to be plotted (among the OutputsModel series)}

\item{BasinArea}{(optional) [numeric] basin area [km2], used to plot flow axes in m3/s}

\item{which}{(optional) [character] choice of plots \cr (e.g. c(\code{"Precip"}, \code{"Temp"}, \code{"SnowPack"}, \code{"Flows"}, \code{"Regime"}, \code{"CumFreq"}, \code{"CorQQ"})), default = \code{"all"}}

\item{log_scale}{(optional) [boolean] boolean indicating if the flow axis is to be logarithmic, default = \code{FALSE}}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}

\item{...}{other parameters to be passed through to plotting functions}
}
\value{
screen plot window
}
\description{
Function which creates a screen plot giving an overview of the model outputs
}
\details{
Dashboard of results including various graphs (depending on the model):\cr
  (1) time series of total precipitation\cr
  (2) time series of temperature (plotted only if CemaNeige is used)\cr
  (3) time series of snow pack (plotted only if CemaNeige is used)\cr
  (4) time series of simulated flows (and observed flows if provided)\cr
  (5) interannual median monthly simulated flow (and observed flows if provided)\cr
  (6) correlation plot between simulated and observed flows (if observed flows provided)\cr
  (7) cumulative frequency plot for simulated flows (and observed flows if provided)
}
\author{
Laurent Coron (June 2014),
Olivier Delaigue,
Guillaume Thirel
}


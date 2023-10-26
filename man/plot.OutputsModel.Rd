\encoding{UTF-8}


\name{plot}
\alias{plot.OutputsModel}
\alias{plot}
\alias{exampleSimPlot}
\alias{simGR4J}
\alias{simCNGR4J}


\title{Default preview of model outputs}


\description{
Function which creates a screen plot giving an overview of the model outputs.
}


\usage{
\method{plot}{OutputsModel}(x, Qobs = NULL, IndPeriod_Plot = NULL,
     BasinArea = NULL, which = "synth", log_scale = FALSE,
     cex.axis = 1, cex.lab = 0.9, cex.leg = 0.9, lwd = 1,
     AxisTS = function(x) axis.POSIXct(side = 1, x = x$DatesR, ...),
     LayoutMat = NULL,
     LayoutWidths = rep.int(1, ncol(LayoutMat)),
     LayoutHeights = rep.int(1, nrow(LayoutMat)),
     verbose = TRUE, ...)
}


\arguments{
\item{x}{[object of class \emph{OutputsModel}] list of model outputs (which must at least include DatesR, Precip and Qsim) [POSIXlt, mm/time step, mm/time step]}

\item{Qobs}{(optional) [numeric] time series of observed flow (for the same time steps than simulated) [mm/time step]}

\item{IndPeriod_Plot}{(optional) [numeric] indices of the time steps to be plotted (among the OutputsModel series)}

\item{BasinArea}{(optional) [numeric] basin area [km2], used to plot flow axes in m3/s}

\item{which}{(optional) [character] choice of plots \cr (e.g. c(\code{"Precip"}, \code{"Temp"}, \code{"SnowPack"}, \code{"Flows"}, \code{"Error"}, \code{"Regime"}, \code{"CumFreq"}, \code{"CorQQ"})), default = \code{"synth"}, see details below}

\item{log_scale}{(optional) [boolean] indicating if the flow and the error time series axis and the flow error time series axis are to be logarithmic, default = \code{FALSE}}

\item{cex.axis}{(optional) [numeric] the magnification to be used for axis annotation relative to the current setting of \code{cex}}

\item{cex.lab}{(optional) [numeric] the magnification to be used for x and y labels relative to the current setting of \code{cex}}

\item{cex.leg}{(optional) [numeric] the magnification to be used for the legend labels relative to the current setting of \code{cex}}

\item{lwd}{(optional) [numeric] the line width (a positive number)}

\item{AxisTS}{(optional) [function] to manage x-axis representing calendar dates and times on time series plots (see \code{\link{axis}} and \code{\link{axis.POSIXct}})}

\item{LayoutMat}{(optional) [numeric] a matrix object specifying the location of the next N figures on the output device. Each value in the matrix must be 0 or a positive integer. If N is the largest positive integer in the matrix, then the integers \eqn{1, \dots, N-1} must also appear at least once in the matrix (see \code{\link{layout}})}

\item{LayoutWidths}{(optional) [numeric] a vector of values for the widths of columns on the device (see \code{\link{layout}})}

\item{LayoutHeights}{(optional) [numeric] a vector of values for the heights of rows on the device (see \code{\link{layout}})}

\item{verbose}{(optional) [boolean] indicating if the function is run in verbose mode or not, default = \code{TRUE}}

\item{...}{(optional) other parameters to be passed through to plotting functions}
}


\value{
Screen plot window.
}


\details{
Different types of independent graphs are available (depending on the model, but always drawn in this order):
\itemize{
  \item \code{"Precip"}: time series of total precipitation
  \item \code{"PotEvap"}: time series of potential evapotranspiration
  \item \code{"ActuEvap"}: time series of simulated actual evapotranspiration (overlaid to \code{"PotEvap"} if already drawn)
  \item \code{"Temp"}: time series of temperature (plotted only if CemaNeige is used)
  \item \code{"SnowPack"}: time series of snow water equivalent (plotted only if CemaNeige is used)
  \item \code{"Flows"}: time series of simulated flows (and observed flows if provided)
  \item \code{"Error"}: time series of simulated flows minus observed flows (and observed flows if provided)
  \item \code{"Regime"}: centred 30-day rolling mean applied on interannual average of daily simulated flows (and observed flows if provided)
  \item \code{"CorQQ"}: correlation plot between simulated and observed flows (only if observed flows provided)
  \item \code{"CumFreq"}: cumulative frequency plot for simulated flows (and observed flows if provided)
}

Different dashboards of results including various graphs are available:
\itemize{
  \item \code{"perf"}: corresponds to \code{"Error"}, \code{"Regime"}, \code{"CumFreq"} and \code{"CorQQ"}
  \item \code{"ts"}: corresponds to \code{"Precip"}, \code{"PotEvap"}, \code{"Temp"}, \code{"SnowPack"} and \code{"Flows"}
  \item \code{"synth"}: corresponds to \code{"Precip"}, \code{"Temp"}, \code{"SnowPack"}, \code{"Flows"}, \code{"Regime"}, \code{"CumFreq"} and \code{"CorQQ"}
  \item \code{"all"}: corresponds to \code{"Precip"}, \code{"PotEvap"}, \code{"ActuEvap"}, \code{"Temp"}, \code{"SnowPack"}, \code{"Flows"}, \code{"Error"}, \code{"Regime"}, \code{"CumFreq"} and \code{"CorQQ"}
  }

If several dashboards are selected, or if an independent graph is called with a dashboard, the graphical device will include all the requested graphs without redundancy.
}

\author{
Laurent Coron, Olivier Delaigue, Guillaume Thirel
}


\examples{
### see examples of RunModel_GR4J or RunModel_CemaNeigeGR4J functions
### to understand how the example datasets have been prepared


## loading examples dataset for GR4J and GR4J + CemaNeige
data(exampleSimPlot)


### Qobs and outputs from GR4J and GR4J + CemaNeige models
str(simGR4J, max.level = 1)
str(simCNGR4J, max.level = 1)


### default dashboard (which = "synth")

## GR models whithout CemaNeige
plot(simGR4J$OutputsModel, Qobs = simGR4J$Qobs)

## GR models whith CemaNeige ("Temp" and "SnowPack" added)
plot(simCNGR4J$OutputsModel, Qobs = simCNGR4J$Qobs)


### "Error" and "CorQQ" plots cannot be display whithout Qobs
plot(simGR4J$OutputsModel, which = "all", Qobs = simGR4J$Qobs)
plot(simGR4J$OutputsModel, which = "all", Qobs = NULL)


### complex plot arrangements
plot(simGR4J$OutputsModel, Qobs = simGR4J$Qobs,
     which = c("Flows", "Regime", "CumFreq", "CorQQ"),
     LayoutMat = matrix(c(1, 2, 3, 1, 4, 4), ncol = 2),
     LayoutWidths  = c(1.5, 1),
     LayoutHeights = c(0.5, 1, 1))


### customize x-axis on time series plot
FunAxisTS <- function(x) {
  axis.POSIXct(side = 1, x = x$DatesR,
  at = pretty(x$DatesR, n = 10),
  format = "\%Y-\%m-\%d")
}
plot(simGR4J$OutputsModel, Qobs = simGR4J$Qobs, AxisTS = FunAxisTS)


### add a main title

## the whole list of settable par's
opar <- par(no.readonly = TRUE)

## define outer margins and a title inside it
par(oma = c(0, 0, 3, 0))
plot(simGR4J$OutputsModel, Qobs = simGR4J$Qobs)
title(main = "GR4J outputs", outer = TRUE, line = 1.2, cex.main = 1.4)

## reset original par
par(opar)
}

\encoding{UTF-8}


\name{RunModel_GR2M}
\alias{RunModel_GR2M}


\title{Run with the GR2M hydrological model}


\description{
Function which performs a single run for the GR2M monthly lumped model over the test period.
}


\usage{
RunModel_GR2M(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 2 parameters
\tabular{ll}{                                                                      
GR2M X1 \tab production store capacity [mm]       \cr
GR2M X2 \tab groundwater exchange coefficient [-] \cr
}}
}


\value{
[list] list containing the function outputs organised as follows:                                         
  \tabular{ll}{                                                                                         
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                            \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/month]    \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation [mm/month]             \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/month]             \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (P1) [mm/month]                     \cr
    \emph{$Perc    } \tab [numeric] series of percolation (P2) [mm/month]                      \cr
    \emph{$PR      } \tab [numeric] series of PR=Pn+Perc (P3) [mm/month]                       \cr
    \emph{$Exch    } \tab [numeric] series of potential exchange between catchments [mm/month] \cr
    \emph{$Prod    } \tab [numeric] series of production store level [mm]                      \cr
    \emph{$Rout    } \tab [numeric] series of routing store level [mm]                         \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge [mm/month]                   \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (production store level and routing store level) [mm], \cr\tab see \code{\link{CreateIniStates}} for more details \cr
  }                                                                                                     
  (refer to the provided references or to the package source code for further details on these model outputs)
}


\details{
For further details on the model, see the references section.
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
\cr
\cr
\if{html}{\figure{diagramGR2M-EN.png}{options: width="60\%" alt="Figure: diagramGR4J-EN.png"}}
\if{latex}{\figure{diagramGR2M-EN.pdf}{options: width=6cm}}
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## conversion of example data from daily to monthly time step
TabSeries       <- data.frame(BasinObs$DatesR, BasinObs$P, BasinObs$E, BasinObs$T, BasinObs$Qmm)
TimeFormat      <- "daily"
NewTimeFormat   <- "monthly"
ConvertFun      <- c("sum", "sum", "mean", "sum")
NewTabSeries    <- SeriesAggreg(TabSeries = TabSeries, TimeFormat = TimeFormat, 
                                NewTimeFormat = NewTimeFormat, ConvertFun = ConvertFun)
BasinObs        <- NewTabSeries
names(BasinObs) <- c("DatesR", "P", "E", "T", "Qmm")

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR2M, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m")=="1990-01"), 
               which(format(BasinObs$DatesR, format = "\%Y-\%m")=="1999-12"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 265.072, X2 = 1.040)
OutputsModel <- RunModel_GR2M(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Safouane Mouelhi, Olivier Delaigue, Guillaume Thirel
}


\references{
Mouelhi S. (2003). 
  Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier. 
  PhD thesis (in French), ENGREF, Cemagref Antony, France.
\cr\cr
Mouelhi, S., C. Michel, C. Perrin and V. Andréassian (2006). 
  Stepwise development of a two-parameter monthly water balance model. 
  Journal of Hydrology, 318(1-4), 200-214. doi: 10.1016/j.jhydrol.2005.06.014.
}


\seealso{
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}

\encoding{UTF-8}


\name{RunModel_GR1A}
\alias{RunModel_GR1A}


\title{Run with the GR1A hydrological model}


\description{
Function which performs a single run for the GR1A annual lumped model over the test period.
}


\usage{
RunModel_GR1A(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 1 parameter
  \tabular{ll}{
    GR1A X1 \tab model parameter [-] \cr
 }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                     \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/y] \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation [mm/y]          \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge [mm/y]                \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/y] \cr
    \emph{RunOptions$Param     } \tab [numeric] parameter set parameter set used by the model                  \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (NULL) [-]             \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## conversion of example data from daily to yearly time step
TabSeries <- data.frame(DatesR = BasinObs$DatesR,
                        P = BasinObs$P,
                        E = BasinObs$E,
                        Qmm = BasinObs$Qmm)
TabSeries <- TabSeries[TabSeries$DatesR < as.POSIXct("2012-09-01", tz = "UTC"), ]
BasinObs <- SeriesAggreg(TabSeries, Format = "\%Y",
                         YearFirstMonth = 09,
                         ConvertFun = c("sum", "sum", "sum"))

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR1A, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y")=="1990"),
               which(format(BasinObs$DatesR, format = "\%Y")=="1999"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR1A,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 0.840)
OutputsModel <- RunModel_GR1A(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Olivier Delaigue, Guillaume Thirel
}


\references{
Mouelhi S. (2003).
  Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier.
  PhD thesis (in French), ENGREF - Cemagref Antony, France.
}


\seealso{
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


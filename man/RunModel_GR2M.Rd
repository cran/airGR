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
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                             \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/month] (E) \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation [mm/month] (P)          \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/month]              \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (P1) [mm/month]                      \cr
    \emph{$Ps      } \tab [numeric] series of part of P filling the production store [mm/month] \cr
    \emph{$Perc    } \tab [numeric] series of percolation (P2) [mm/month]                       \cr
    \emph{$PR      } \tab [numeric] series of PR=Pn+Perc (P3) [mm/month]                        \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments [mm/month]     \cr
    \emph{$Prod    } \tab [numeric] series of production store level (S) [mm]                   \cr
    \emph{$Rout    } \tab [numeric] series of routing store level (R1) [mm]                     \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge [mm/month] (Q)                \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/month] \cr
    \emph{RunOptions$Param     } \tab [numeric] parameter set parameter set used by the model                      \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (production store level and routing store level) [mm]. See \code{\link{CreateIniStates}} for more details \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
For further details on the model, see the references section. \cr
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

## preparation of the InputsModel object with daily time step data
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## conversion of InputsModel to monthly time step
InputsModel <- SeriesAggreg(InputsModel, Format = "\%Y\%m")

## run period selection
Ind_Run <- seq(which(format(InputsModel$DatesR, format = "\%Y-\%m")=="1990-01"),
               which(format(InputsModel$DatesR, format = "\%Y-\%m")=="1999-12"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 265.072, X2 = 1.040)
OutputsModel <- RunModel_GR2M(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## conversion of observed discharge to monthly time step
Qobs <- SeriesAggreg(data.frame(BasinObs$DatesR, BasinObs$Qmm),
                    Format = "\%Y\%m",
                    ConvertFun = "sum")
Qobs <- Qobs[Ind_Run, 2]

## results preview
plot(OutputsModel, Qobs = Qobs)

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = Qobs)
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Safouane Mouelhi, Olivier Delaigue, Guillaume Thirel
}


\references{
Mouelhi S. (2003).
  Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier.
  PhD thesis (in French), ENGREF - Cemagref Antony, France.
\cr\cr
Mouelhi, S., Michel, C., Perrin, C. and Andréassian, V. (2006).
  Stepwise development of a two-parameter monthly water balance model.
  Journal of Hydrology, 318(1-4), 200-214, \doi{10.1016/j.jhydrol.2005.06.014}.
}


\seealso{
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}

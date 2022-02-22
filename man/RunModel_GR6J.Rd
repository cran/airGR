\encoding{UTF-8}


\name{RunModel_GR6J}
\alias{RunModel_GR6J}


\title{Run with the GR6J hydrological model}


\description{
Function which performs a single run for the GR6J daily lumped model over the test period.
}


\usage{
RunModel_GR6J(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 6 parameters
  \tabular{ll}{
    GR6J X1 \tab production store capacity [mm]               \cr
    GR6J X2 \tab intercatchment exchange coefficient [mm/d]   \cr
    GR6J X3 \tab routing store capacity [mm]                  \cr
    GR6J X4 \tab unit hydrograph time constant [d]            \cr
    GR6J X5 \tab intercatchment exchange threshold [-]        \cr
    GR6J X6 \tab exponential store depletion coefficient [mm] \cr
  }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                                     \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration (E) [mm/d]             \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation (P) [mm/d]                      \cr
    \emph{$Prod    } \tab [numeric] series of production store level (S) [mm]                           \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (Pn) [mm/d]                                  \cr
    \emph{$Ps      } \tab [numeric] series of the part of Pn filling the production store (Ps) [mm/d]   \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/d]                          \cr
    \emph{$Perc    } \tab [numeric] series of percolation (Perc) [mm/d]                                 \cr
    \emph{$PR      } \tab [numeric] series of Pr=Pn-Ps+Perc (Pr) [mm/d]                                 \cr
    \emph{$Q9      } \tab [numeric] series of UH1 outflow (Q9) [mm/d]                                   \cr
    \emph{$Q1      } \tab [numeric] series of UH2 outflow (Q1) [mm/d]                                   \cr
    \emph{$Rout    } \tab [numeric] series of routing store level (R1) [mm]                             \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/d]         \cr
    \emph{$AExch1  } \tab [numeric] series of actual exchange between catchments for branch 1 [mm/d]    \cr
    \emph{$AExch2  } \tab [numeric] series of actual exchange between catchments for branch 2 [mm/d]    \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (AExch1+AExch2) [mm/d] \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (Qr) [mm/d]                         \cr
    \emph{$QRExp   } \tab [numeric] series of exponential store outflow (QrExp) [mm/d]                  \cr
    \emph{$Exp     } \tab [numeric] series of exponential store level (negative) (R2) [mm]              \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH2 after exchange (Qd) [mm/d]           \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge (Q) [mm/d]                            \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/d] \cr
    \emph{RunOptions$Param     } \tab [numeric] parameter set parameter set used by the model                  \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (res. levels, UH1 levels, UH2 levels) [mm]. See \code{\link{CreateIniStates}} for more details \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
\cr
\cr
\if{html}{\figure{diagramGR6J-EN.png}{options: width="60\%" alt="Figure: diagramGR6J-EN.png"}}
\if{latex}{\figure{diagramGR6J-EN.pdf}{options: width=6cm}}
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR6J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR6J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 242.257, X2 = 0.637, X3 = 53.517, X4 = 2.218, X5 = 0.424, X6 = 4.759)
OutputsModel <- RunModel_GR6J(InputsModel = InputsModel,
                             RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Charles Perrin, Raji Pushpalatha, Nicolas Le Moine, Olivier Delaigue, Guillaume Thirel
}


\references{
Pushpalatha, R., Perrin, C., Le Moine, N., Mathevet, T. and Andréassian, V. (2011).
  A downward structural sensitivity analysis of hydrological models to improve low-flow simulation.
  Journal of Hydrology, 411(1-2), 66-76, \doi{10.1016/j.jhydrol.2011.09.034}.
}


\seealso{
\code{\link{RunModel_GR4J}}, \code{\link{RunModel_GR5J}}, \code{\link{RunModel_CemaNeigeGR6J}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


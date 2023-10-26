\encoding{UTF-8}


\name{RunModel_GR5H}
\alias{RunModel_GR5H}


\title{Run with the GR5H hydrological model}


\description{
Function which performs a single run for the GR5H hourly lumped model.
}


\usage{
RunModel_GR5H(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 5 parameters
  \tabular{ll}{
    GR5H X1 \tab production store capacity [mm]             \cr
    GR5H X2 \tab intercatchment exchange coefficient [mm/h] \cr
    GR5H X3 \tab routing store capacity [mm]                \cr
    GR5H X4 \tab unit hydrograph time constant [h]          \cr
    GR5H X5 \tab intercatchment exchange threshold [-]      \cr
  }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                                                             \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration (E) [mm/h]                                     \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation (P) [mm/h]                                              \cr
    \emph{$Interc  } \tab [numeric] series of interception store level (I) [mm]                                                 \cr
    \emph{$Prod    } \tab [numeric] series of production store level (S) [mm]                                                   \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (Pn) [mm/h]                                                          \cr
    \emph{$Ps      } \tab [numeric] series of the part of Pn filling the production store (Ps) [mm/h]                           \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration (Ei+Es) [mm/h]                                          \cr
    \emph{$EI      } \tab [numeric] series of evapotranspiration from rainfall neutralisation or interception store (Ei) [mm/h] \cr
    \emph{$ES      } \tab [numeric] series of evapotranspiration from production store (Es) [mm/h]                              \cr
    \emph{$Perc    } \tab [numeric] series of percolation (Perc) [mm/h]                                                         \cr
    \emph{$PR      } \tab [numeric] series of Pr=Pn-Ps+Perc (Pr) [mm/h]                                                         \cr
    \emph{$Q9      } \tab [numeric] series of UH outflow going into branch 9 (Q9) [mm/h]                                        \cr
    \emph{$Q1      } \tab [numeric] series of UH outflow going into branch 1 (Q1) [mm/h]                                        \cr
    \emph{$Rout    } \tab [numeric] series of routing store level (R1) [mm]                                                     \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/h]                                 \cr
    \emph{$AExch1  } \tab [numeric] series of actual exchange between catchments for branch 1 [mm/h]                            \cr
    \emph{$AExch2  } \tab [numeric] series of actual exchange between catchments for branch 2 [mm/h]                            \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (AExch1+AExch2) [mm/h]                         \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (Qr) [mm/h]                                                 \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH after exchange (Qd) [mm/h]                                    \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge (Q) [mm/h]                                                    \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/h]                  \cr
    \emph{RunOptions$Param     } \tab [numeric] parameter set parameter set used by the model                                   \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (res. levels, UH levels) [mm]. See \code{\link{CreateIniStates}} for more details \cr
  }
Refer to the provided references or to the package source code for further details on these model outputs.
}


\details{
It is advised to run the GR5H model with an interception store (see Ficchi (2017) and Ficchi et al. (2019)) as it improves the consistency of the model fluxes and provides better performance. To do so, the \code{\link{Imax}} function allows to estimate the maximal capacity of the interception store, which can then be given to \code{\link{CreateRunOptions}}. \cr

For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
\cr
\cr
\if{html}{\figure{diagramGR5H-EN.png}{options: width="60\%" alt="Figure: diagramGR5H-EN.png"}}
\if{latex}{\figure{diagramGR5H-EN.pdf}{options: width=6cm}}
\cr
\cr
See \code{\link{RunModel_GR5J}} to look at the diagram of the hydrological model when no interception store is used.
}


\examples{
library(airGR)

## load of catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2006-01-01 00"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2006-12-31 23"))

## Imax computation
Imax <- Imax(InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
             TestedValues = seq(from = 0, to = 3, by = 0.2))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR5H, Imax = Imax,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 706.912, X2 = -0.163, X3 = 188.880, X4 = 2.575, X5 = 0.104)
OutputsModel <- RunModel_GR5H(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Guillaume Thirel, Olivier Delaigue
}


\references{
Ficchi, A. (2017).
  An adaptive hydrological model for multiple time-steps:
  Diagnostics and improvements based on fluxes consistency.
  PhD thesis, UPMC - Irstea Antony, Paris, France.
\cr\cr
Ficchi, A., Perrin, C. and Andréassian, V. (2019).
  Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching.
  Journal of Hydrology, 575, 1308-1327, \doi{10.1016/j.jhydrol.2019.05.084}.
}


\seealso{
\code{\link{RunModel_GR4H}}, \code{\link{RunModel_CemaNeigeGR5H}}, \code{\link{Imax}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


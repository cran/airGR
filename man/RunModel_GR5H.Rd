\encoding{UTF-8}


\name{RunModel_GR5H}
\alias{RunModel_GR5H}


\title{Run with the GR5H hydrological model}


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
[list] list containing the function outputs organised as follows:                                         
  \tabular{ll}{                                                                                         
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                                                        \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/h]                                    \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation [mm/h]                                             \cr
    \emph{$Interc  } \tab [numeric] series of interception store level [mm]                                                \cr
    \emph{$Prod    } \tab [numeric] series of production store level [mm]                                                  \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall [mm/h]                                                          \cr
    \emph{$Ps      } \tab [numeric] series of the part of Pn filling the production store [mm/h]                           \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/h]                                             \cr
    \emph{$EI      } \tab [numeric] series of evapotranspiration from rainfall neutralisation or interception store [mm/h] \cr
    \emph{$ES      } \tab [numeric] series of evapotranspiration from production store [mm/h]                              \cr
    \emph{$Perc    } \tab [numeric] series of percolation (PERC) [mm/h]                                                    \cr
    \emph{$PR      } \tab [numeric] series of PR=Pn-Ps+Perc [mm/h]                                                         \cr
    \emph{$Q9      } \tab [numeric] series of UH1 outflow (Q9) [mm/h]                                                      \cr
    \emph{$Q1      } \tab [numeric] series of UH2 outflow (Q1) [mm/h]                                                      \cr
    \emph{$Rout    } \tab [numeric] series of routing store level [mm]                                                     \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/h]                            \cr
    \emph{$AExch1  } \tab [numeric] series of actual exchange between catchments for branch 1 [mm/h]                       \cr
    \emph{$AExch2  } \tab [numeric] series of actual exchange between catchments for branch 2 [mm/h]                       \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (1+2) [mm/h]                              \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (QR) [mm/h]                                            \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/h]                              \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge [mm/h]                                                   \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (res. levels, UH1 levels, UH2 levels) [mm], see \code{\link{CreateIniStates}} for more details \cr
  }                                                                                                     
  (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the GR5H hourly lumped model.
}


\details{
It is advised to run the GR5H model with an interception store (see Ficchi (2017) and Ficchi et al. (2019)) as it improves the consistency of the model fluxes and provides better performance. To do so, the \code{\link{Imax}} function allows to estimate the maximal capacity of the interception store, which can then be given to \code{\link{CreateRunOptions}}. \cr

For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## load of catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2005-01-01 00:00"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2006-12-31 23:00"))

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
  PhD thesis, Irstea (Antony), GRNE (Paris), France.
\cr\cr
Ficchi, A., C. Perrin and V. Andréassian (2019).
  Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching.
  Journal of Hydrology, 575, 1308-1327. doi: 10.1016/j.jhydrol.2019.05.084.
}


\seealso{
\code{\link{RunModel_GR4H}}, \code{\link{RunModel_CemaNeigeGR5H}}, \code{\link{Imax}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


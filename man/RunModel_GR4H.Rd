\encoding{UTF-8}


\name{RunModel_GR4H}
\alias{RunModel_GR4H}


\title{Run with the GR4H hydrological model}


\description{
Function which performs a single run for the GR4H hourly lumped model.
}


\usage{
RunModel_GR4H(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 4 parameters
  \tabular{ll}{
    GR4H X1 \tab production store capacity [mm]          \cr
    GR4H X2 \tab groundwater exchange coefficient [mm/h] \cr
    GR4H X3 \tab routing store capacity [mm]             \cr
    GR4H X4 \tab unit hydrograph time constant [h]       \cr
  }}
}


\value{
[list] containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                                     \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration (E) [mm/h]             \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation (P) [mm/h]                      \cr
    \emph{$Prod    } \tab [numeric] series of production store level (S) [mm]                           \cr
    \emph{$Pn      } \tab [numeric] series of net rainfall (Pn) [mm/h]                                  \cr
    \emph{$Ps      } \tab [numeric] series of the part of Pn filling the production store (Ps) [mm/h    \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/h]                          \cr
    \emph{$Perc    } \tab [numeric] series of percolation (Perc) [mm/h]                                 \cr
    \emph{$PR      } \tab [numeric] series of Pr=Pn-Ps+Perc (Pr) [mm/h]                                 \cr
    \emph{$Q9      } \tab [numeric] series of UH1 outflow (Q9) [mm/h]                                   \cr
    \emph{$Q1      } \tab [numeric] series of UH2 outflow (Q1) [mm/h]                                   \cr
    \emph{$Rout    } \tab [numeric] series of routing store level (R1) [mm]                             \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/h]         \cr
    \emph{$AExch1  } \tab [numeric] series of actual exchange between catchments for branch 1 [mm/h]    \cr
    \emph{$AExch2  } \tab [numeric] series of actual exchange between catchments for branch 2 [mm/h]    \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (AExch1+AExch2) [mm/h] \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (Qr) [mm/h]                         \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH2 after exchange (Qd) [mm/h]           \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge (Q) [mm/h]                            \cr
    \emph{RunOptions$WarmUpQsim} \tab [numeric] series of simulated discharge (Q) on the warm-up period [mm/h] \cr
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
See \code{\link{RunModel_GR4J}} to look at the diagram of the hydrological model.
}


\examples{
library(airGR)

## load of catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2005-01-01 00"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H")=="2008-12-31 23"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4H,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 756.930, X2 = -0.773, X3 = 138.638, X4 = 5.247)
OutputsModel <- RunModel_GR4H(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Charles Perrin, Thibaut Mathevet, Olivier Delaigue, Guillaume Thirel
}


\references{
Mathevet, T. (2005).
  Quels modèles pluie-débit globaux pour le pas de temps horaire ?
  Développement empirique et comparaison de modèles sur un large échantillon de bassins versants.
  PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France.
\cr\cr
Le Moine, N. (2008).
  Le bassin versant de surface vu par le souterrain :
  une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?
  PhD thesis (in French), UPMC - Cemagref Antony, Paris, France.
}


\seealso{
\code{\link{RunModel_GR4J}}, \code{\link{RunModel_CemaNeigeGR4H}},
         \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


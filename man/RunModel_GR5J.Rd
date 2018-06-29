\encoding{UTF-8}


\name{RunModel_GR5J}
\alias{RunModel_GR5J}


\title{Run with the GR5J hydrological model}


\usage{
RunModel_GR5J(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 5 parameters
\tabular{ll}{                                                                      
GR5J X1      \tab production store capacity [mm]                                \cr
GR5J X2      \tab intercatchment exchange coefficient [mm/d]                  \cr
GR5J X3      \tab routing store capacity [mm]                                   \cr
GR5J X4      \tab unit hydrograph time constant [d]                             \cr
GR5J X5      \tab intercatchment exchange threshold [-]                     \cr
}}
}


\value{
[list] list containing the function outputs organised as follows:                                         
         \tabular{ll}{                                                                                         
         \emph{$DatesR  }          \tab [POSIXlt] series of dates                                                     \cr
         \emph{$PotEvap }          \tab [numeric] series of input potential evapotranspiration [mm/d]                 \cr
         \emph{$Precip  }          \tab [numeric] series of input total precipitation [mm/d]                          \cr
         \emph{$Prod    }          \tab [numeric] series of production store level [mm]                        \cr
         \emph{$Pn      }          \tab [numeric] series of net rainfall [mm/d]                         			  \cr
         \emph{$Ps      }          \tab [numeric] series of the part of Pn filling the production store [mm/d]        \cr
         \emph{$AE      }          \tab [numeric] series of actual evapotranspiration [mm/d]                          \cr
         \emph{$Perc    }          \tab [numeric] series of percolation (PERC) [mm/d]                                 \cr
         \emph{$PR      }          \tab [numeric] series of PR=Pn-Ps+Perc [mm/d]                                      \cr
         \emph{$Q9      }          \tab [numeric] series of UH1 outflow (Q9) [mm/d]                                   \cr
         \emph{$Q1      }          \tab [numeric] series of UH2 outflow (Q1) [mm/d]                                   \cr
         \emph{$Rout    }          \tab [numeric] series of routing store level [mm]                           \cr
         \emph{$Exch    }          \tab [numeric] series of potential semi-exchange between catchments [mm/d]         \cr
         \emph{$AExch1  }          \tab [numeric] series of actual exchange between catchments for branch 1 [mm/d]    \cr
         \emph{$AExch2  }          \tab [numeric] series of actual exchange between catchments for branch 2 [mm/d]    \cr
         \emph{$AExch   }          \tab [numeric] series of actual exchange between catchments (1+2) [mm/d]           \cr
         \emph{$QR      }          \tab [numeric] series of routing store outflow (QR) [mm/d]                         \cr
         \emph{$QD      }          \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/d]           \cr
         \emph{$Qsim    }          \tab [numeric] series of simulated discharge [mm/d]                                               \cr
         \emph{$StateEnd}          \tab [numeric] states at the end of the run (res. levels, UH1 levels, UH2 levels) [mm], \cr\tab see \code{\link{CreateIniStates}} for more details \cr
         }                                                                                                     
         (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the GR5J daily lumped model over the test period.
}


\details{
For further details on the model, see the references section.
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y")=="01/01/1990"), 
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y")=="31/12/1999"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR5J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(245.918, 1.027, 90.017, 2.198, 0.434)
OutputsModel <- RunModel_GR5J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Nicolas Le Moine
}


\references{
Le Moine, N. (2008), Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances
      et du réalisme des modèles pluie-débit ?, PhD thesis (french), UPMC, Paris, France. \cr
Pushpalatha, R., C. Perrin, N. Le Moine, T. Mathevet, and V. Andréassian (2011), 
      A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, 
      Journal of Hydrology, 411(1-2), 66-76, doi:10.1016/j.jhydrol.2011.09.034. \cr
}


\seealso{
\code{\link{RunModel_GR4J}}, \code{\link{RunModel_GR6J}}, \code{\link{RunModel_CemaNeigeGR5J}},
         \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


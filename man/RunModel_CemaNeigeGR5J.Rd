\encoding{UTF-8}
\name{RunModel_CemaNeigeGR5J}
\alias{RunModel_CemaNeigeGR5J}
\title{Run with the CemaNeigeGR5J hydrological model}
\usage{
RunModel_CemaNeigeGR5J(InputsModel, RunOptions, Param)
}
\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 7 parameters
\tabular{ll}{                                                                      
GR5J X1      \tab production store capacity [mm]                                \cr
GR5J X2      \tab intercatchment exchange coefficient [mm/d]                  \cr
GR5J X3      \tab routing store capacity [mm]                                   \cr
GR5J X4      \tab unit hydrograph time constant [d]                             \cr
GR5J X5      \tab intercatchment exchange threshold [-]                     \cr
CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-]         \cr
CemaNeige X2 \tab degree-day melt coefficient [mm/°C/d]                       \cr
}}
}
\value{
[list] list containing the function outputs organised as follows:                                         
         \tabular{ll}{                                                                                         
         \emph{$DatesR  }          \tab [POSIXlt] series of dates                                                     \cr
         \emph{$PotEvap }          \tab [numeric] series of input potential evapotranspiration [mm/d]                 \cr
         \emph{$Precip  }          \tab [numeric] series of input total precipitation [mm/d]                          \cr
         \emph{$Prod    }          \tab [numeric] series of production store level [mm]                        \cr
         \emph{$AE      }          \tab [numeric] series of actual evapotranspiration [mm/d]                          \cr
         \emph{$Perc    }          \tab [numeric] series of percolation (PERC) [mm/d]                                 \cr
         \emph{$PR      }          \tab [numeric] series of PR=PN-PS+PERC [mm/d]                                      \cr
         \emph{$Q9      }          \tab [numeric] series of UH1 outflow (Q9) [mm/d]                                   \cr
         \emph{$Q1      }          \tab [numeric] series of UH2 outflow (Q1) [mm/d]                                   \cr
         \emph{$Rout    }          \tab [numeric] series of routing store level [mm]                           \cr
         \emph{$Exch    }          \tab [numeric] series of potential semi-exchange between catchments [mm/d]         \cr
         \emph{$AExch   }          \tab [numeric] series of actual exchange between catchments (1+2) [mm/d]           \cr
         \emph{$QR      }          \tab [numeric] series of routing store outflow (QR) [mm/d]                         \cr
         \emph{$QD      }          \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/d]           \cr
         \emph{$Qsim    }          \tab [numeric] series of Qsim [mm/d]                                               \cr
         \emph{$CemaNeigeLayers}   \tab [list] list of CemaNeige outputs (1 list per layer)                           \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Pliq         }   \tab [numeric] series of liquid precip. [mm/d]                          \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Psol         }   \tab [numeric] series of solid precip. [mm/d]                           \cr
         \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     }   \tab [numeric] series of snow pack [mm]                                 \cr
         \emph{$CemaNeigeLayers[[iLayer]]$ThermalState }   \tab [numeric] series of snow pack thermal state [°C]                 \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Gratio       }   \tab [numeric] series of Gratio [0-1]                                   \cr
         \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      }   \tab [numeric] series of potential snow melt [mm/d]                     \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Melt         }   \tab [numeric] series of actual snow melt [mm/d]                        \cr
         \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  }   \tab [numeric] series of liquid precip. + actual snow melt [mm/d]       \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Temp         }   \tab [numeric] series of air temperature [°C] \cr
         \emph{$StateEnd}                                  \tab [numeric] states at the end of the run: \cr\tab store & unit hydrographs levels [mm], CemaNeige states [mm & °C] \cr
         }                                                                                                     
         (refer to the provided references or to the package source code for further details on these model outputs)
}
\description{
Function which performs a single run for the CemaNeige-GR5J daily lumped model.
}
\details{
For further details on the model, see the references section.
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}
\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR5J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T, 
                                 ZInputs = median(BasinInfo$HypsoData),
                                 HypsoData = BasinInfo$HypsoData, NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="01/01/1990 00:00"), 
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="31/12/1999 00:00"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR5J, InputsModel = InputsModel, 
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(179.139, -0.100, 203.815, 1.174, 2.478, 0.977, 2.774)
OutputsModel <- RunModel_CemaNeigeGR5J(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}
\author{
Laurent Coron, Audrey Valéry, Claude Michel, Nicolas Le Moine, Charles Perrin, Vazken Andréassian (December 2013)
}
\references{
Le Moine, N. (2008), Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances 
      et du réalisme des modèles pluie-débit ?, PhD thesis (french), UPMC, Paris, France. \cr
  Pushpalatha, R., C. Perrin, N. Le Moine, T. Mathevet and V. Andréassian (2011), 
      A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, 
      Journal of Hydrology, 411(1-2), 66-76, doi:10.1016/j.jhydrol.2011.09.034. \cr
  Valéry, A., V. Andréassian and C. Perrin (2014), 
      "As simple as possible but not simpler": what is useful in a temperature-based snow-accounting routine? 
      Part 1 - Comparison of six snow accounting routines on 380 catchments, Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.059. \cr
  Valéry, A., V. Andréassian and C. Perrin (2014), 
      "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? 
      Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments, Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.058.
}
\seealso{
\code{\link{RunModel_CemaNeigeGR4J}}, \code{\link{RunModel_CemaNeigeGR6J}}, \code{\link{RunModel_GR5J}},
         \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}

\encoding{UTF-8}


\name{RunModel_CemaNeigeGR6J}
\alias{RunModel_CemaNeigeGR6J}


\title{Run with the CemaNeigeGR6J hydrological model}


\usage{
RunModel_CemaNeigeGR6J(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 8 (or 10 parameters if \code{IsHyst = TRUE})
\tabular{ll}{                                                                  
GR6J X1      \tab production store capacity [mm]                                          \cr
GR6J X2      \tab intercatchment exchange coefficient [mm/d]                              \cr
GR6J X3      \tab routing store capacity [mm]                                             \cr
GR6J X4      \tab unit hydrograph time constant [d]                                       \cr
GR6J X5      \tab intercatchment exchange threshold [-]                                   \cr
GR6J X6      \tab coefficient for emptying exponential store [mm]                         \cr
CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-]                   \cr
CemaNeige X2 \tab degree-day melt coefficient [mm/°C/d]                                   \cr
CemaNeige X3 \tab (optional) accumulation threshold [mm] (needed if \code{IsHyst = TRUE}) \cr
CemaNeige X4 \tab (optional) percentage (between 0 and 1) of annual snowfall defining the melt threshold [-] (needed if \code{IsHyst = TRUE})\cr
}}
}


\value{
[list] list containing the function outputs organised as follows:                                         
  \tabular{ll}{                                                                                         
  \emph{$DatesR }          \tab [POSIXlt] series of dates                                                             \cr
  \emph{$PotEvap}          \tab [numeric] series of input potential evapotranspiration [mm/d]                         \cr
  \emph{$Precip }          \tab [numeric] series of input total precipitation [mm/d]                                  \cr
  \emph{$Prod   }          \tab [numeric] series of production store level [mm]                                       \cr
  \emph{$Pn     }          \tab [numeric] series of net rainfall [mm/d]                         			                \cr
  \emph{$Ps     }          \tab [numeric] series of the part of Ps filling the production store [mm/d]                \cr
  \emph{$AE     }          \tab [numeric] series of actual evapotranspiration [mm/d]                                  \cr
  \emph{$Perc   }          \tab [numeric] series of percolation (PERC) [mm/d]                                         \cr
  \emph{$PR     }          \tab [numeric] series of PR=PN-PS+PERC [mm/d]                                              \cr
  \emph{$Q9     }          \tab [numeric] series of UH1 outflow (Q9) [mm/d]                                           \cr
  \emph{$Q1     }          \tab [numeric] series of UH2 outflow (Q1) [mm/d]                                           \cr
  \emph{$Rout   }          \tab [numeric] series of routing store level [mm]                                          \cr
  \emph{$Exch   }          \tab [numeric] series of potential semi-exchange between catchments [mm/d]                 \cr
  \emph{$AExch1 }          \tab [numeric] series of actual exchange between catchments for branch 1 [mm/d]            \cr
  \emph{$AExch2 }          \tab [numeric] series of actual exchange between catchments for branch 2 [mm/d]            \cr
  \emph{$AExch  }          \tab [numeric] series of actual exchange between catchments (1+2) [mm/d]                   \cr
  \emph{$QR     }          \tab [numeric] series of routing store outflow (QR) [mm/d]                                 \cr
  \emph{$QRExp  }          \tab [numeric] series of exponential store outflow (QRExp) [mm/d]                          \cr
  \emph{$Exp    }          \tab [numeric] series of exponential store level (negative) [mm]                           \cr
  \emph{$QD     }          \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/d]                   \cr
  \emph{$Qsim   }          \tab [numeric] series of Qsim [mm/d]                                                       \cr
  \emph{$CemaNeigeLayers}   \tab [list] list of CemaNeige outputs (1 list per layer)                                  \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Pliq         }   \tab [numeric] series of liquid precip. [mm/d]                    \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Psol         }   \tab [numeric] series of solid precip. [mm/d]                     \cr
  \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     }   \tab [numeric] series of snow pack [mm]                           \cr
  \emph{$CemaNeigeLayers[[iLayer]]$ThermalState }   \tab [numeric] series of snow pack thermal state [°C]             \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gratio       }   \tab [numeric] series of Gratio [0-1]                             \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      }   \tab [numeric] series of potential snow melt [mm/d]               \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Melt         }   \tab [numeric] series of actual snow melt [mm/d]                  \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  }   \tab [numeric] series of liquid precip. + actual snow melt [mm/d] \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Temp         }   \tab [numeric] series of air temperature [°C]                     \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gthreshold   }   \tab [numeric] series of melt threshold [mm]                      \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Glocalmax    }   \tab [numeric] series of local melt threshold for hysteresis [mm] \cr
  \emph{$StateEnd}                                  \tab [numeric] states at the end of the run: \cr\tab store & unit hydrographs levels [mm], CemaNeige states [mm & °C], \cr\tab see \code{\link{CreateIniStates}} for more details \cr
         }                                                                                                     
         (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the CemaNeige-GR6J daily lumped model.
}


\details{
The choice of the CemaNeige version is explained in \code{\link{CreateRunOptions}}. \cr
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## loading catchment data
data(L0123002)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR6J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T, 
                                 ZInputs = median(BasinInfo$HypsoData),
                                 HypsoData = BasinInfo$HypsoData, NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"), 
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))


## ---- original version of CemaNeige

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR6J, InputsModel = InputsModel, 
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 116.482, X2 = 0.500, X3 = 72.733, X4 = 1.224, X5 = 0.278, X6 = 30.333,
           CNX1 = 0.977, CNX2 = 2.776)
OutputsModel <- RunModel_CemaNeigeGR6J(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)


## ---- version of CemaNeige with the Linear Hysteresis

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR6J, InputsModel = InputsModel, 
                               IndPeriod_Run = Ind_Run, IsHyst = TRUE)
                               
## simulation
Param <- c(X1 = 116.482, X2 = 0.500, X3 = 72.733, X4 = 1.224, X5 = 0.278, X6 = 30.333,
           CNX1 = 0.977, CNX2 = 2.774, CNX3 = 100, CNX4 = 0.4)
OutputsModel <- RunModel_CemaNeigeGR6J(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Audrey Valéry, Claude Michel, Charles Perrin, Raji Pushpalatha, Nicolas Le Moine, Vazken Andréassian, Olivier Delaigue
}


\references{
Pushpalatha, R., C. Perrin, N. Le Moine, T. Mathevet and V. Andréassian (2011). 
A downward structural sensitivity analysis of hydrological models to improve low-flow simulation.
Journal of Hydrology, 411(1-2), 66-76. doi:10.1016/j.jhydrol.2011.09.034.
\cr\cr
Riboust, P., G. Thirel, N. Le Moine and P. Ribstein (2019). 
  Revisiting a simple degree-day model for integrating satellite data: implementation of SWE-SCA hystereses. 
  Journal of Hydrology and Hydromechanics. doi:10.2478/johh-2018-0004, 67, 1, 70–81.
\cr\cr
Valéry, A., V. Andréassian and C. Perrin (2014). 
  "As simple as possible but not simpler": what is useful in a temperature-based snow-accounting routine? 
  Part 1 - Comparison of six snow accounting routines on 380 catchments. 
  Journal of Hydrology.  doi:10.1016/j.jhydrol.2014.04.059.
\cr\cr
Valéry, A., V. Andréassian and C. Perrin (2014). *
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? 
  Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments. 
  Journal of Hydrology. doi:10.1016/j.jhydrol.2014.04.058.
}


\seealso{
\code{\link{RunModel_CemaNeige}}, \code{\link{RunModel_CemaNeigeGR4J}}, 
\code{\link{RunModel_CemaNeigeGR5J}}, \code{\link{RunModel_GR6J}}, 
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}.
}


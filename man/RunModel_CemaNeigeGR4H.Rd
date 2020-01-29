\encoding{UTF-8}


\name{RunModel_CemaNeigeGR4H}
\alias{RunModel_CemaNeigeGR4H}


\title{Run with the CemaNeigeGR4H hydrological model}


\description{
Function which performs a single run for the CemaNeige-GR4H hourly lumped model over the test period.
}


\usage{
RunModel_CemaNeigeGR4H(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 6 (or 8 parameters if \code{IsHyst = TRUE}, see \code{\link{CreateRunOptions}} for details)
\tabular{ll}{                                                                      
GR4H X1      \tab production store capacity [mm]                                          \cr
GR4H X2      \tab intercatchment exchange coefficient [mm/h]                              \cr
GR4H X3      \tab routing store capacity [mm]                                             \cr
GR4H X4      \tab unit hydrograph time constant [h]                                       \cr
CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-]                   \cr
CemaNeige X2 \tab degree-hour melt coefficient [mm/°C/h]                                  \cr
CemaNeige X3 \tab (optional) accumulation threshold [mm] (needed if \code{IsHyst = TRUE}) \cr
CemaNeige X4 \tab (optional) percentage (between 0 and 1) of annual snowfall defining the melt threshold [-] (needed if \code{IsHyst = TRUE}) \cr
}}
}


\value{
[list] list containing the function outputs organised as follows:                                         
  \tabular{ll}{                                                                                         
  \emph{$DatesR  }          \tab [POSIXlt] series of dates                                                  \cr
  \emph{$PotEvap }          \tab [numeric] series of input potential evapotranspiration [mm/h]              \cr
  \emph{$Precip  }          \tab [numeric] series of input total precipitation [mm/h]                       \cr
  \emph{$Prod    }          \tab [numeric] series of production store level [mm]                            \cr
  \emph{$Pn      }          \tab [numeric] series of net rainfall [mm/h]                                    \cr
  \emph{$Ps      }          \tab [numeric] series of the part of Pn filling the production store [mm/h]     \cr
  \emph{$AE      }          \tab [numeric] series of actual evapotranspiration [mm/h]                       \cr
  \emph{$Perc    }          \tab [numeric] series of percolation (PERC) [mm/h]                              \cr
  \emph{$PR      }          \tab [numeric] series of PR=Pn-Ps+Perc [mm/h]                                   \cr
  \emph{$Q9      }          \tab [numeric] series of UH1 outflow (Q9) [mm/h]                                \cr
  \emph{$Q1      }          \tab [numeric] series of UH2 outflow (Q1) [mm/h]                                \cr
  \emph{$Rout    }          \tab [numeric] series of routing store level [mm]                               \cr
  \emph{$Exch    }          \tab [numeric] series of potential semi-exchange between catchments [mm/h]      \cr
  \emph{$AExch1  }          \tab [numeric] series of actual exchange between catchments for branch 1 [mm/h] \cr
  \emph{$AExch2  }          \tab [numeric] series of actual exchange between catchments for branch 2 [mm/h] \cr
  \emph{$AExch   }          \tab [numeric] series of actual exchange between catchments (1+2) [mm/h]        \cr
  \emph{$QR      }          \tab [numeric] series of routing store outflow (QR) [mm/h]                      \cr
  \emph{$QD      }          \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/h]        \cr
  \emph{$Qsim    }          \tab [numeric] series of simulated discharge [mm/h]                             \cr
  \emph{$CemaNeigeLayers}   \tab [list] list of CemaNeige outputs (1 list per layer)                        \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Pliq         } \tab [numeric] series of liquid precip. [mm/h]                    \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Psol         } \tab [numeric] series of solid precip. [mm/h]                     \cr
  \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     } \tab [numeric] series of snow pack [mm]                           \cr
  \emph{$CemaNeigeLayers[[iLayer]]$ThermalState } \tab [numeric] series of snow pack thermal state [°C]             \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gratio       } \tab [numeric] series of Gratio [0-1]                             \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      } \tab [numeric] series of potential snow melt [mm/h]               \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Melt         } \tab [numeric] series of actual snow melt [mm/h]                  \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  } \tab [numeric] series of liquid precip. + actual snow melt [mm/h] \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Temp         } \tab [numeric] series of air temperature [°C]                     \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gthreshold   } \tab [numeric] series of melt threshold [mm]                      \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Glocalmax    } \tab [numeric] series of local melt threshold for hysteresis [mm] \cr
  \emph{$StateEnd}                                \tab [numeric] states at the end of the run: \cr\tab store & unit hydrographs levels [mm], CemaNeige states [mm & °C], \cr\tab see \code{\link{CreateIniStates}} for more details                               \cr
  }                                                                                                     
  (refer to the provided references or to the package source code for further details on these model outputs)
}


\details{
It is advised to run the GR5H model with an interception store (see Ficchi et al. (2019)) as it improves the consistency of the model fluxes and provides better performance. To do so, the \code{\link{Imax}} functions allows to estimates the maximal capacity of the interception store, which can then be given to \code{\link{CreateRunOptions}}. \cr

The choice of the CemaNeige version is explained in \code{\link{CreateRunOptions}}. \cr
For further details on the model, see the references section. \cr
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
\dontrun{
library(airGR)

## loading catchment data
data(U2345030)


## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4H, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T, 
                                 ZInputs = BasinInfo$ZInputs,
                                 HypsoData = BasinInfo$HypsoData, NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2004-03-01 00:00"), 
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2008-12-31 23:00"))


## ---- original version of CemaNeige

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4H, InputsModel = InputsModel, 
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 149.905, X2 = -0.487, X3 = 391.506, X4 = 9.620,
           CNX1 = 0.520, CNX2 = 0.133)
OutputsModel <- RunModel_CemaNeigeGR4H(InputsModel = InputsModel,
                                       RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}
}


\author{
Laurent Coron, Claude Michel, Charles Perrin, Thibault Mathevet, Audrey Valéry, Vazken Andréassian, Olivier Delaigue, Guillaume Thirel
}


\references{
Perrin, C., C. Michel and V. Andréassian (2003). 
  Improvement of a parsimonious model for streamflow simulation. 
  Journal of Hydrology, 279(1-4), 275-289, doi: 10.1016/S0022-1694(03)00225-7.
\cr\cr
Riboust, P., G. Thirel, N. Le Moine and P. Ribstein (2019). 
  Revisiting a simple degree-day model for integrating satellite data: implementation of SWE-SCA hystereses. 
  Journal of Hydrology and Hydromechanics. doi: 10.2478/johh-2018-0004, 67, 1, 70–81.
\cr\cr
Valéry, A., V. Andréassian and C. Perrin (2014). 
  "As simple as possible but not simpler": what is useful in a temperature-based snow-accounting routine? 
  Part 1 - Comparison of six snow accounting routines on 380 catchments. 
  Journal of Hydrology. doi: 10.1016/j.jhydrol.2014.04.059.
\cr\cr
Valéry, A., V. Andréassian and C. Perrin (2014). 
  "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? 
   Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments. 
   Journal of Hydrology. doi: 10.1016/j.jhydrol.2014.04.058.
}


\seealso{
\code{\link{RunModel_CemaNeige}}, \code{\link{RunModel_CemaNeigeGR4J}}, \code{\link{RunModel_CemaNeigeGR5J}},
\code{\link{RunModel_CemaNeigeGR6J}}, \code{\link{RunModel_GR4H}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


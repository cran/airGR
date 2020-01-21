\encoding{UTF-8}


\name{RunModel_CemaNeigeGR5H}
\alias{RunModel_CemaNeigeGR5H}


\title{Run with the CemaNeigeGR5H hydrological model}


\usage{
RunModel_CemaNeigeGR5H(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 7 (or 9 parameters if \code{IsHyst = TRUE}, see \code{\link{CreateRunOptions}} for details)
\tabular{ll}{                                                                      
GR5H X1      \tab production store capacity [mm]                                          \cr
GR5H X2      \tab intercatchment exchange coefficient [mm/h]                              \cr
GR5H X3      \tab routing store capacity [mm]                                             \cr
GR5H X4      \tab unit hydrograph time constant [h]                                       \cr
GR5H X5      \tab intercatchment exchange threshold [-]                                   \cr
CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-]                   \cr
CemaNeige X2 \tab degree-hour melt coefficient [mm/°C/h]                                  \cr
CemaNeige X3 \tab (optional) accumulation threshold [mm] (needed if \code{IsHyst = TRUE}) \cr
CemaNeige X4 \tab (optional) percentage (between 0 and 1) of annual snowfall defining the melt threshold [-] (needed if \code{IsHyst = TRUE}) \cr
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
  \emph{$CemaNeigeLayers[[iLayer]]$Pliq         } \tab [numeric] series of liquid precip. [mm/h]                         \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Psol         } \tab [numeric] series of solid precip. [mm/h]                          \cr
  \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     } \tab [numeric] series of snow pack [mm]                                \cr
  \emph{$CemaNeigeLayers[[iLayer]]$ThermalState } \tab [numeric] series of snow pack thermal state [°C]                  \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gratio       } \tab [numeric] series of Gratio [0-1]                                  \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      } \tab [numeric] series of potential snow melt [mm/h]                    \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Melt         } \tab [numeric] series of actual snow melt [mm/h]                       \cr
  \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  } \tab [numeric] series of liquid precip. + actual snow melt [mm/h]      \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Temp         } \tab [numeric] series of air temperature [°C]                          \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Gthreshold   } \tab [numeric] series of melt threshold [mm]                           \cr
  \emph{$CemaNeigeLayers[[iLayer]]$Glocalmax    } \tab [numeric] series of local melt threshold for hysteresis [mm]      \cr
  \emph{$StateEnd}                                \tab [numeric] states at the end of the run: \cr\tab store & unit hydrographs levels [mm], CemaNeige states [mm & °C], \cr\tab see \code{\link{CreateIniStates}} for more details                               \cr
  }                                                                                                     
  (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the CemaNeige-GR5H hourly lumped model over the test period.
}


\details{
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
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR5H, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T, 
                                 ZInputs = BasinInfo$ZInputs,
                                 HypsoData = BasinInfo$HypsoData, NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2004-03-01 00:00"), 
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2008-12-31 23:00"))


## ---- original version of CemaNeige

## Imax computation
Imax <- Imax(InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
             TestedValues = seq(from = 0, to = 3, by = 0.2))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR5H, InputsModel = InputsModel, 
                               Imax = Imax, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 218.537, X2 = -0.009, X3 = 174.862, X4 = 6.674, X5 = 0.000,
           CNX1 = 0.002, CNX2 = 3.787)
OutputsModel <- RunModel_CemaNeigeGR5H(InputsModel = InputsModel,
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
Laurent Coron, Guillaume Thirel, Olivier Delaigue, Audrey Valéry, Vazken Andréassian
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
\cr\cr
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
\code{\link{RunModel_CemaNeige}}, \code{\link{RunModel_CemaNeigeGR4H}}, \code{\link{RunModel_GR5H}}, \code{\link{Imax}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


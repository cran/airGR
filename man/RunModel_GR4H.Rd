\encoding{UTF-8}


\name{RunModel_GR4H}
\alias{RunModel_GR4H}


\title{Run with the GR4H hydrological model}


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
[list] list containing the function outputs organised as follows:                                         
  \tabular{ll}{                                                                                         
    \emph{$DatesR  } \tab [POSIXlt] series of dates                                             \cr
    \emph{$PotEvap } \tab [numeric] series of input potential evapotranspiration [mm/h]         \cr
    \emph{$Precip  } \tab [numeric] series of input total precipitation [mm/h]                  \cr
    \emph{$Prod    } \tab [numeric] series of production store level [mm]                       \cr
    \emph{$AE      } \tab [numeric] series of actual evapotranspiration [mm/h]                  \cr
    \emph{$Perc    } \tab [numeric] series of percolation (PERC) [mm/h]                         \cr
    \emph{$PR      } \tab [numeric] series of PR=Pn-Ps+Perc [mm/h]                              \cr
    \emph{$Q9      } \tab [numeric] series of UH1 outflow (Q9) [mm/h]                           \cr
    \emph{$Q1      } \tab [numeric] series of UH2 outflow (Q1) [mm/h]                           \cr
    \emph{$Rout    } \tab [numeric] series of routing store level [mm]                          \cr
    \emph{$Exch    } \tab [numeric] series of potential semi-exchange between catchments [mm/h] \cr
    \emph{$AExch   } \tab [numeric] series of actual exchange between catchments (1+2) [mm/h]   \cr
    \emph{$QR      } \tab [numeric] series of routing store outflow (QR) [mm/h]                 \cr
    \emph{$QD      } \tab [numeric] series of direct flow from UH2 after exchange (QD) [mm/h]   \cr
    \emph{$Qsim    } \tab [numeric] series of simulated discharge [mm/h]                        \cr
    \emph{$StateEnd} \tab [numeric] states at the end of the run (res. levels, UH1 levels, UH2 levels) [mm], see \code{\link{CreateIniStates}} for more details \cr
  }                                                                                                     
  (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the GR4H hourly lumped model.
}


\details{
For further details on the model, see the references section.
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## load of catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2004-03-01 00:00"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d \%H:\%M")=="2008-12-31 23:00"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4H,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 521.113, X2 = -2.918, X3 = 218.009, X4 = 4.124)
OutputsModel <- RunModel_GR4H(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Charles Perrin, Thibaut Mathevet
}


\references{
Mathevet, T. (2005). 
  Quels modèles pluie-débit globaux pour le pas de temps horaire ? 
  Développement empirique et comparaison de modèles sur un large échantillon de bassins versants. 
  PhD thesis (in French), ENGREF - Cemagref (Antony), Paris, France.
\cr\cr
Le Moine, N. (2008). 
  Le bassin versant de surface vu par le souterrain : 
  une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?
  PhD thesis (french), UPMC, Paris, France.
}


\seealso{
\code{\link{RunModel_GR4J}},
         \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


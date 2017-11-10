\encoding{UTF-8}


\name{RunModel_CemaNeige}
\alias{RunModel_CemaNeige}


\title{Run with the CemaNeige snow module}


\usage{
RunModel_CemaNeige(InputsModel, RunOptions, Param)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 2 parameters
\tabular{ll}{                                                                      
CemaNeige X1 \tab weighting coefficient for snow pack thermal state [-] \cr
CemaNeige X2 \tab degree-day melt coefficient [mm/°C/d]                 \cr
}}
}


\value{
[list] list containing the function outputs organised as follows:                                         
         \tabular{ll}{
         \emph{$DatesR}                                    \tab [POSIXlt] series of dates                                    \cr
         \emph{$CemaNeigeLayers}                           \tab [list] list of CemaNeige outputs (1 list per layer)          \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Pliq         }   \tab [numeric] series of liquid precip. [mm/d]                    \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Psol         }   \tab [numeric] series of solid precip. [mm/d]                     \cr
         \emph{$CemaNeigeLayers[[iLayer]]$SnowPack     }   \tab [numeric] series of snow pack [mm]                           \cr
         \emph{$CemaNeigeLayers[[iLayer]]$ThermalState }   \tab [numeric] series of snow pack thermal state [°C]           \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Gratio       }   \tab [numeric] series of Gratio [0-1]                             \cr
         \emph{$CemaNeigeLayers[[iLayer]]$PotMelt      }   \tab [numeric] series of potential snow melt [mm]                 \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Melt         }   \tab [numeric] series of actual snow melt [mm]                    \cr
         \emph{$CemaNeigeLayers[[iLayer]]$PliqAndMelt  }   \tab [numeric] series of liquid precip. + actual snow melt [mm]   \cr
         \emph{$CemaNeigeLayers[[iLayer]]$Temp         }   \tab [numeric] series of air temperature [°C] \cr
         \emph{$StateEnd}                                  \tab [numeric] states at the end of the run: CemaNeige states [mm & °C], \cr\tab see \code{\link{CreateIniStates}} for more details \cr
         }                                                                                                     
         (refer to the provided references or to the package source code for further details on these model outputs)
}


\description{
Function which performs a single run for the CemaNeige daily snow module.
}


\details{
For further details on the model, see the references section.
For further details on the argument structures and initialisation options, see \code{\link{CreateRunOptions}}.
}


\examples{
library(airGR)

## load of catchment data
data(L0123002)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeige, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P,TempMean = BasinObs$T,
                                 ZInputs = BasinInfo$HypsoData[51], HypsoData=BasinInfo$HypsoData,
                                 NLayers = 5)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="01/01/1990 00:00"),
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="31/12/1999 00:00"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeige, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(0.962, 2.249)
OutputsModel <- RunModel_CemaNeige(InputsModel = InputsModel,
                                   RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel)

}


\author{
Laurent Coron
}


\references{
Valéry, A., V. Andréassian and C. Perrin (2014), 
      "As simple as possible but not simpler": what is useful in a temperature-based snow-accounting routine? 
      Part 1 - Comparison of six snow accounting routines on 380 catchments, Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.059. \cr
  Valéry, A., V. Andréassian and C. Perrin (2014), 
      "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? 
      Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments, Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.058.
}


\seealso{
\code{\link{RunModel_CemaNeigeGR4J}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateIniStates}}.
}


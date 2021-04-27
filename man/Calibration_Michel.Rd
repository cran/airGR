\encoding{UTF-8}


\name{Calibration_Michel}
\alias{Calibration_Michel}


\title{Calibration algorithm that optimises the error criterion selected as objective function using the Irstea procedure described by C. Michel}


\description{
Calibration algorithm that optimises the error criterion selected as objective function. \cr
\cr
The algorithm combines a global and a local approach.
First, a screening is performed using either a rough predefined grid or a list of parameter sets.
Then a steepest descent local search algorithm is performed, starting from the result of the screening procedure.
}


\usage{
Calibration_Michel(InputsModel, RunOptions, InputsCrit, CalibOptions,
                   FUN_MOD, FUN_CRIT, FUN_TRANSFO = NULL, verbose = TRUE, ...)
}


\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{InputsCrit}{[object of class \emph{InputsCrit}] see \code{\link{CreateInputsCrit}} for details}

\item{CalibOptions}{[object of class \emph{CalibOptions}] see \code{\link{CreateCalibOptions}} for details}

\item{FUN_MOD}{[function] hydrological model function (e.g. \code{\link{RunModel_GR4J}}, \code{\link{RunModel_CemaNeigeGR4J}})}

\item{FUN_CRIT}{(deprecated) [function] error criterion function (e.g. \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}})}

\item{FUN_TRANSFO}{(optional) [function] model parameters transformation function, if the \code{FUN_MOD} used is native in the package \code{FUN_TRANSFO} is automatically defined}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}

\item{...}{(optional) arguments to pass to \code{\link{RunModel}}}
}


\value{
[list] list containing the function outputs organised as follows:
  \tabular{ll}{
    \emph{$ParamFinalR  } \tab [numeric] parameter set obtained at the end of the calibration                                  \cr
    \emph{$CritFinal    } \tab [numeric] error criterion selected as objective function obtained at the end of the calibration \cr
    \emph{$NIter        } \tab [numeric] number of iterations during the calibration                                           \cr
    \emph{$NRuns        } \tab [numeric] number of model runs done during the calibration                                      \cr
    \emph{$HistParamR   } \tab [numeric] table showing the progression steps in the search for optimal set: parameter values   \cr
    \emph{$HistCrit     } \tab [numeric] table showing the progression steps in the search for optimal set: criterion values   \cr
    \emph{$MatBoolCrit  } \tab [boolean] table giving the requested and actual time steps over which the model is calibrated   \cr
    \emph{$CritName     } \tab [character] name of the calibration criterion used as objective function                        \cr
    \emph{$CritBestValue} \tab [numeric] theoretical best criterion value                                                      \cr
  }
}


\details{
A screening is first performed either based on a rough predefined grid (considering various initial
values for each parameter) or from a list of initial parameter sets. \cr
The best set identified in this screening is then used as a starting point for the steepest
descent local search algorithm. \cr
For this search, since the ranges of parameter values can be quite different,
simple mathematical transformations are applied to parameters to make them vary
in a similar range and get a similar sensitivity to a predefined search step.  This is done using the TransfoParam functions. \cr
During the steepest descent method, at each iteration, we start from a parameter set of NParam values (NParam being the number of
free parameters of the chosen hydrological model) and we determine the 2*NParam-1 new candidates
by changing one by one the different parameters (+/- search step). \cr
All these candidates are tested and the best one kept to be the starting point for the next
iteration. At the end of each iteration, the the search step is either increased or decreased to adapt
the progression speed. A composite step can occasionally be done. \cr
The calibration algorithm stops when the search step becomes smaller than a predefined threshold.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## calibration period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run)

## calibration criterion: preparation of the InputsCrit object
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])

## preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)

## calibration
OutputsCalib <- Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                 InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                 FUN_MOD = RunModel_GR4J)

## simulation
Param <- OutputsCalib$ParamFinalR
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Claude Michel, Charles Perrin, Thibault Mathevet, Olivier Delaigue, Guillaume Thirel, David Dorchies
}


\references{
Michel, C. (1991),
  Hydrologie appliquée aux petits bassins ruraux.
  Hydrology handbook (in French), Cemagref, Antony, France.
}


\seealso{
\code{\link{Calibration}},
\code{\link{RunModel_GR4J}}, \code{\link{TransfoParam}}, \code{\link{ErrorCrit_RMSE}},
\code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}},
\code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}.
}


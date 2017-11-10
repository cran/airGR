\encoding{UTF-8}


\name{CreateCalibOptions}
\alias{CreateCalibOptions}


\title{Creation of the CalibOptions object required but the Calibration functions}


\usage{
CreateCalibOptions(FUN_MOD, FUN_CALIB = Calibration_Michel,
  FUN_TRANSFO = NULL, FixedParam = NULL,
  SearchRanges = NULL, StartParamList = NULL,
  StartParamDistrib = NULL)
}


\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. \code{\link{RunModel_GR4J}}, \code{\link{RunModel_CemaNeigeGR4J}})}

\item{FUN_CALIB}{(optional) [function] calibration algorithm function (e.g. Calibration_Michel), default = \code{Calibration_Michel}}

\item{FUN_TRANSFO}{(optional) [function] model parameters transformation function, if the \code{FUN_MOD} used is native in the package, \code{FUN_TRANSFO} is automatically defined}

\item{FixedParam}{(optional) [numeric] vector giving the values set for the non-optimised parameter values (NParam columns, 1 line)
\cr Example:
\tabular{llllll}{
              \tab   NA   \tab   NA   \tab 3.34   \tab  ...    \tab   NA \cr
}}

\item{SearchRanges}{(optional) [numeric] matrix giving the ranges of real parameters (NParam columns, 2 lines)
\cr Example:
\tabular{llllll}{
              \tab [X1]   \tab [X2]   \tab [X3]   \tab [...]   \tab [Xi] \cr
     [1,]     \tab    0   \tab   -1   \tab    0   \tab  ...    \tab  0.0 \cr
     [2,]     \tab 3000   \tab   +1   \tab  100   \tab  ...    \tab  3.0 \cr
}}

\item{StartParamList}{(optional) [numeric] matrix of parameter sets used for grid-screening calibration procedure (values in columns, sets in line)
\cr Example:
\tabular{llllll}{
              \tab [X1]   \tab [X2]   \tab [X3]   \tab [...]   \tab [Xi] \cr
     [set1]   \tab  800   \tab -0.7   \tab   25   \tab  ...    \tab  1.0 \cr
     [set2]   \tab 1000   \tab -0.5   \tab   22   \tab  ...    \tab  1.1 \cr
     [...]    \tab  ...   \tab  ...   \tab  ...   \tab  ...    \tab  ... \cr
     [set n]  \tab  200   \tab -0.3   \tab   17   \tab  ...    \tab  1.0 \cr
}}

\item{StartParamDistrib}{(optional) [numeric] matrix of parameter values used for grid-screening calibration procedure (values in columns, percentiles in line)
\cr Example:
\tabular{llllll}{
              \tab [X1]   \tab [X2]   \tab [X3]   \tab [...]   \tab [Xi] \cr
     [value1] \tab  800   \tab -0.7   \tab   25   \tab  ...    \tab  1.0 \cr
     [value2] \tab 1000   \tab   NA   \tab   50   \tab  ...    \tab  1.2 \cr
     [value3] \tab 1200   \tab   NA   \tab   NA   \tab  ...    \tab  1.6 \cr
}}
}
\value{
[list] object of class \emph{CalibOptions} containing the data required to evaluate the model outputs; it can include the following:
         \tabular{ll}{
         \emph{$FixedParam       }  \tab   [numeric] vector giving the values to allocate to non-optimised parameter values \cr
         \emph{$SearchRanges     }  \tab   [numeric] matrix giving the ranges of raw parameters \cr
         \emph{$StartParamList   }  \tab   [numeric] matrix of parameter sets used for grid-screening calibration procedure \cr
         \emph{$StartParamDistrib}  \tab   [numeric] matrix of parameter values used for grid-screening calibration procedure \cr
         }
}


\description{
Creation of the \emph{CalibOptions} object required by the \code{Calibration*} functions.
}


\details{
Users wanting to use \code{FUN_MOD}, \code{FUN_CALIB} or \code{FUN_TRANSFO} functions that are not included in 
the package must create their own \code{CalibOptions} object accordingly.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## calibration period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="01/01/1990 00:00"), 
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="31/12/1999 00:00"))

## preparation of RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## calibration criterion: preparation of the InputsCrit object
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])

## preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)

## calibration
OutputsCalib <- Calibration(InputsModel = InputsModel, RunOptions = RunOptions,
                            InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                            FUN_MOD = RunModel_GR4J, FUN_CRIT = ErrorCrit_NSE, 
                            FUN_CALIB = Calibration_Michel)

## simulation
Param <- OutputsCalib$ParamFinalR
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, 
                         Param = Param, FUN = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

}


\author{
Laurent Coron
}


\seealso{
\code{\link{Calibration}}, \code{\link{RunModel}}
}


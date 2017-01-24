\encoding{UTF-8}
\name{Calibration}
\alias{Calibration}
\title{Calibration algorithm which optimises the error criterion selected as objective function using the provided functions}
\usage{
Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions, FUN_MOD,
  FUN_CRIT, FUN_CALIB = Calibration_Michel, FUN_TRANSFO = NULL,
  verbose = TRUE)
}
\arguments{
\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{InputsCrit}{[object of class \emph{InputsCrit}] see \code{\link{CreateInputsCrit}} for details}

\item{CalibOptions}{[object of class \emph{CalibOptions}] see \code{\link{CreateCalibOptions}} for details}

\item{FUN_MOD}{[function] hydrological model function (e.g. RunModel_GR4J, RunModel_CemaNeigeGR4J)}

\item{FUN_CRIT}{[function] error criterion function (e.g. ErrorCrit_RMSE, ErrorCrit_NSE)}

\item{FUN_CALIB}{(optional) [function] calibration algorithm function (e.g. Calibration_Michel), default=Calibration_Michel}

\item{FUN_TRANSFO}{(optional) [function] model parameters transformation function, if the FUN_MOD used is native in the package FUN_TRANSFO is automatically defined}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default=TRUE}
}
\value{
[list] see \code{\link{Calibration_Michel}}
}
\description{
Calibration algorithm which optimises the error criterion selected as objective function using the provided functions. \cr
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
                            InputsCrit = InputsCrit,  CalibOptions = CalibOptions,
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
Laurent Coron (June 2014)
}
\seealso{
\code{\link{Calibration_Michel}},
         \code{\link{ErrorCrit}}, \code{\link{TransfoParam}},
         \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, 
         \code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}.
}


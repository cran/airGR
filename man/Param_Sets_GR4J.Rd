\docType{data}
\encoding{UTF-8}


\name{Param_Sets_GR4J}
\alias{Param_Sets_GR4J}


\title{Generalist parameter sets for the GR4J model}


\description{
These parameter sets can be used as an alternative for the grid-screening calibration procedure (i.e. first step in \code{\link{Calibration_Michel}}).
Please note that the given GR4J X4u variable does not correspond to the actual GR4J X4 parameter. As explained in Andréassian et al. (2014; section 2.1), the given GR4J X4u value has to be adjusted (rescaled) using catchment area (S) [km2] as follows: \eqn{X4 = X4u / 5.995 \times S^{0.3}} (please note that the formula is erroneous in the publication). Please, see the example below. \cr
As shown in Andréassian et al. (2014; figure 4), only using these parameters sets as the tested values for calibration is more efficient than a classical calibration when the amount of data is low (6 months or less).
}


\format{Data frame of parameters containing four numeric vectors:
  \tabular{ll}{
    GR4J X1  \tab production store capacity [mm]             \cr
    GR4J X2  \tab intercatchment exchange coefficient [mm/d] \cr
    GR4J X3  \tab routing store capacity [mm]                \cr
    GR4J X4u \tab unajusted unit hydrograph time constant [d]\cr
  }
}


\seealso{
  \code{\link{RunModel_GR4J}}, \code{\link{Calibration_Michel}}, \code{\link{CreateCalibOptions}}.
}


\references{
Andréassian, V., Bourgin, F., Oudin, L., Mathevet, T., Perrin, C., Lerat, J., Coron, L. and Berthet, L. (2014).
  Seeking genericity in the selection of parameter sets: Impact on hydrological model efficiency.
  Water Resources Research, 50(10), 8356-8366, \doi{10.1002/2013WR014761}.
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## loading generalist parameter sets
data(Param_Sets_GR4J)
str(Param_Sets_GR4J)

## computation of the real GR4J X4
Param_Sets_GR4J$X4 <- Param_Sets_GR4J$X4u / 5.995 * BasinInfo$BasinArea^0.3
Param_Sets_GR4J$X4u <- NULL
Param_Sets_GR4J <- as.matrix(Param_Sets_GR4J)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## --- calibration step

## short calibration period selection (< 6 months)
Ind_Cal <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-02-28"))

## preparation of the RunOptions object for the calibration period
RunOptions_Cal <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModel, IndPeriod_Run = Ind_Cal)

## simulation and efficiency criterion (Nash-Sutcliffe Efficiency)
## with all generalist parameter sets on the calibration period
OutputsCrit_Loop <- apply(Param_Sets_GR4J, 1, function(Param) {
  OutputsModel_Cal <- RunModel_GR4J(InputsModel = InputsModel,
                                    RunOptions = RunOptions_Cal,
                                    Param = Param)
  InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                  RunOptions = RunOptions_Cal, Obs = BasinObs$Qmm[Ind_Cal])
  OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel_Cal)
  return(OutputsCrit$CritValue)
})

## best parameter set
Param_Best <- unlist(Param_Sets_GR4J[which.max(OutputsCrit_Loop), ])


## --- validation step

## validation period selection
Ind_Val <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-03-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of the RunOptions object for the validation period
RunOptions_Val <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModel, IndPeriod_Run = Ind_Val)

## simulation with the best parameter set on the validation period
OutputsModel_Val <- RunModel_GR4J(InputsModel = InputsModel,
                                  RunOptions = RunOptions_Val,
                                  Param = Param_Best)

## results preview of the simulation with the best parameter set on the validation period
plot(OutputsModel_Val, Qobs = BasinObs$Qmm[Ind_Val])

## efficiency criterion (Nash-Sutcliffe Efficiency) on the validation period
InputsCrit_Val  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions_Val, Obs = BasinObs$Qmm[Ind_Val])
OutputsCrit_Val <- ErrorCrit_NSE(InputsCrit = InputsCrit_Val, OutputsModel = OutputsModel_Val)
}

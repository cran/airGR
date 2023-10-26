\encoding{UTF-8}


\name{ErrorCrit}
\alias{ErrorCrit}


\title{Error criterion using the provided function}


\description{
Function which computes an error criterion with the provided function.
}


\usage{
ErrorCrit(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
}


\arguments{
\item{InputsCrit}{[object of class \emph{InputsCrit}] see \code{\link{CreateInputsCrit}} for details}

\item{OutputsModel}{[object of class \emph{OutputsModel}] see \code{\link{RunModel_GR4J}} or \code{\link{RunModel_CemaNeigeGR4J}} for details}

\item{warnings}{(optional) [boolean] boolean indicating if the warning messages are shown, default = \code{TRUE}}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}
}


\value{
If \code{InputsCrit} is of class \emph{Single}:
  \tabular{ll}{
    [list] containing the \code{ErrorCrit_*} functions outputs, see \code{\link{ErrorCrit_RMSE}} or \code{\link{ErrorCrit_NSE}} for details \cr
  }

If \code{InputsCrit} is of class \emph{Multi}:
  \tabular{ll}{
    [list] of list containing the \code{ErrorCrit_*} functions outputs, see \code{\link{ErrorCrit_RMSE}} or \code{\link{ErrorCrit_NSE}} for details \cr
  }

If \code{InputsCrit} is of class \emph{Compo}:
  \tabular{ll}{
    \emph{$CritValue    } \tab [numeric] value of the composite criterion \cr
    \emph{$CritName     } \tab [character] name of the composite criterion \cr
    \emph{$CritBestValue} \tab [numeric] theoretical best criterion value \cr
    \emph{$Multiplier   } \tab [numeric] integer indicating whether the criterion is indeed an error (+1) or an efficiency (-1) \cr
    \emph{$CritCompo$MultiCritValues} \tab [numeric] values of the sub-criteria \cr
    \emph{$CritCompo$MultiCritNames} \tab [numeric] names of the sub-criteria \cr
    \emph{$CritCompo$MultiCritWeights} \tab [character] weighted values of the sub-criteria \cr
    \emph{$MultiCrit    } \tab [list] of list containing the \code{ErrorCrit_*} functions outputs, see \code{\link{ErrorCrit_NSE}} or \code{\link{ErrorCrit_KGE}} for details \cr
  }

}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## calibration period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## single efficiency criterion: Nash-Sutcliffe Efficiency
InputsCritSingle <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                     InputsModel = InputsModel, RunOptions = RunOptions,
                                     Obs = list(BasinObs$Qmm[Ind_Run]),
                                     VarObs = "Q", transfo = "",
                                     Weights = NULL)
str(ErrorCrit(InputsCrit = InputsCritSingle, OutputsModel = OutputsModel))

## 2 efficiency critera:  RMSE and the Nash-Sutcliffe Efficiency
InputsCritMulti <- CreateInputsCrit(FUN_CRIT = list(ErrorCrit_RMSE, ErrorCrit_NSE),
                                    InputsModel = InputsModel, RunOptions = RunOptions,
                                    Obs = list(BasinObs$Qmm[Ind_Run],
                                               BasinObs$Qmm[Ind_Run]),
                                    VarObs = list("Q", "Q"), transfo = list("", "sqrt"),
                                    Weights = NULL)
str(ErrorCrit(InputsCrit = InputsCritMulti, OutputsModel = OutputsModel))

## efficiency composite criterion: Nash-Sutcliffe Efficiency mixing
##                                 both raw and log-transformed flows
InputsCritCompo <- CreateInputsCrit(FUN_CRIT = list(ErrorCrit_NSE, ErrorCrit_NSE),
                                    InputsModel = InputsModel, RunOptions = RunOptions,
                                    Obs = list(BasinObs$Qmm[Ind_Run],
                                               BasinObs$Qmm[Ind_Run]),
                                    VarObs = list("Q", "Q"), transfo = list("", "log"),
                                    Weights = list(0.4, 0.6))
str(ErrorCrit(InputsCrit = InputsCritCompo, OutputsModel = OutputsModel))
}


\author{
Olivier Delaigue
}


\seealso{
\code{\link{CreateInputsCrit}}, \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}},
\code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}
}


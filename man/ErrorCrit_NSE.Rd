\encoding{UTF-8}


\name{ErrorCrit_NSE}
\alias{ErrorCrit_NSE}


\title{Error criterion based on the NSE formula}


\usage{
ErrorCrit_NSE(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
}


\description{
Function which computes an error criterion based on the NSE formula proposed by Nash & Sutcliffe (1970).
}


\arguments{
\item{InputsCrit}{[object of class \emph{InputsCrit}] see \code{\link{CreateInputsCrit}} for details}

\item{OutputsModel}{[object of class \emph{OutputsModel}] see \code{\link{RunModel_GR4J}} or \code{\link{RunModel_CemaNeigeGR4J}} for details}

\item{warnings}{(optional) [boolean] boolean indicating if the warning messages are shown, default = \code{TRUE}}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}
}


\value{
[list] list containing the function outputs organised as follows:
         \tabular{ll}{
         \emph{$CritValue      }   \tab   [numeric] value of the criterion \cr
         \emph{$CritName       }   \tab   [character] name of the criterion \cr
         \emph{$CritBestValue  }   \tab   [numeric] theoretical best criterion value \cr
         \emph{$Multiplier     }   \tab   [numeric] integer indicating whether the criterion is indeed an error (+1) or an efficiency (-1) \cr
         \emph{$Ind_notcomputed}   \tab   [numeric] indices of the time steps where \emph{InputsCrit$BoolCrit} = \code{FALSE} or no data is available \cr
         }
}


\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) which allows
the use of the function for model calibration: the product \eqn{CritValue \times Multiplier} is the criterion to be minimised
(Multiplier = -1 for NSE).
}


\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="1999-12-31"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(X1 = 734.568, X2 = -0.840, X3 = 109.809, X4 = 1.971)
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions,
                         Param = Param, FUN = RunModel_GR4J)

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Nash-Sutcliffe Efficiency on log-transformed flows
transfo <- "log"
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run],
                               transfo = transfo)
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency above a threshold (quant. 75 \%)
BoolCrit <- BasinObs$Qmm[Ind_Run] >= quantile(BasinObs$Qmm[Ind_Run], 0.75, na.rm = TRUE)
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run],
                               BoolCrit = BoolCrit)
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Olivier Delaigue
}
\references{
Nash, J. E. and Sutcliffe, J. V. (1970).
  River flow forecasting through conceptual models.
  Part 1 - A discussion of principles.
  Journal of Hydrology, 10(3), 282-290, \doi{10.1016/0022-1694(70)90255-6}.
}


\seealso{
\code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}
}


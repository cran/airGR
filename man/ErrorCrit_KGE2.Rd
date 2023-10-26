\encoding{UTF-8}


\name{ErrorCrit_KGE2}
\alias{ErrorCrit_KGE2}


\title{Error criterion based on the KGE' formula}


\description{
Function which computes an error criterion based on the KGE' formula proposed by Kling et al. (2012).
}


\usage{
ErrorCrit_KGE2(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
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
         \emph{$SubCritValues  }   \tab   [numeric] values of the sub-criteria \cr
         \emph{$SubCritNames   }   \tab   [character] names of the components of the criterion \cr
         \emph{$CritBestValue  }   \tab   [numeric] theoretical best criterion value \cr
         \emph{$Multiplier     }   \tab   [numeric] integer indicating whether the criterion is indeed an error (+1) or an efficiency (-1) \cr
         \emph{$Ind_notcomputed}   \tab   [numeric] indices of the time steps where \emph{InputsCrit$BoolCrit} = \code{FALSE} or no data is available \cr
         }
}


\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) which allows
the use of the function for model calibration: the product \eqn{CritValue \times Multiplier} is the criterion to be minimised (Multiplier = -1 for KGE2).\cr\cr
The KGE' formula is
\deqn{KGE' = 1 - \sqrt{(r - 1)^2 + (\gamma - 1)^2 + (\beta - 1)^2}}{KGE' = 1 - sqrt((r - 1)² + (\gamma - 1)² + (\beta - 1)²)}
with the following sub-criteria:
\deqn{r = \mathrm{the\: linear\ correlation\: coefficient\: between\:} sim\: \mathrm{and\:} obs}{r = is the linear correlation coefficient between Q[sim] and Q[obs]}
\deqn{\gamma = \frac{CV_{sim}}{CV_{obs}}}{\gamma = CV[sim] / CV[obs]}
\deqn{\beta = \frac{\mu_{sim}}{\mu_{obs}}}{\beta = \mu[sim] / \mu[obs]}
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

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE2, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_KGE2(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency on square-root-transformed flows
transfo <- "sqrt"
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE2, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run],
                               transfo = transfo)
OutputsCrit <- ErrorCrit_KGE2(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency above a threshold (quant. 75 \%)
BoolCrit <- BasinObs$Qmm[Ind_Run] >= quantile(BasinObs$Qmm[Ind_Run], 0.75, na.rm = TRUE)
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE2, InputsModel = InputsModel,
                               RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run],
                               BoolCrit = BoolCrit)
OutputsCrit <- ErrorCrit_KGE2(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Olivier Delaigue
}


\references{
Gupta, H. V., Kling, H., Yilmaz, K. K. and Martinez, G. F. (2009).
  Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling.
  Journal of Hydrology, 377(1-2), 80-91, \doi{10.1016/j.jhydrol.2009.08.003}.
\cr\cr
Kling, H., Fuchs, M. and Paulin, M. (2012).
  Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios.
  Journal of Hydrology, 424-425, 264-277, \doi{10.1016/j.jhydrol.2012.01.011}.
}


\seealso{
\code{\link{ErrorCrit}}, \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}}, \code{\link{ErrorCrit_KGE}}
}


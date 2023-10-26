\encoding{UTF-8}


\name{CreateErrorCrit_GAPX}
\alias{CreateErrorCrit_GAPX}


\title{Creation of the ErrorCrit_GAPX function}


\description{
Function which creates the function \code{ErrorCrit_GAPX}.

The produced function \code{ErrorCrit_GAPX} allows computing an error criterion based on the GAPX formula proposed by Lavenne et al. (2019).
}


\usage{
CreateErrorCrit_GAPX(FUN_TRANSFO)
}


\arguments{
\item{FUN_TRANSFO}{[function] The parameter transformation function used with the model}
}


\value{
  [function] function \code{ErrorCrit_GAPX} embedding the parameter transformation function used with the model
}

\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) that allows
the use of the function for model calibration: the product \eqn{CritValue \times Multiplier} is the criterion to be minimised
(\code{Multiplier = -1} for NSE).
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

## Creation of the ErrorCrit GAPX function
ErrorCrit_GAPX <- CreateErrorCrit_GAPX(TransfoParam_GR4J)

## The "a priori" parameters for GAPX
AprParamR <- c(X1 = 157, X2 = 0.8, X3 = 100, X4 = 1.5)
AprParamT <- TransfoParam_GR4J(AprParamR, "RT")

## Single efficiency criterion: GAPX with a priori parameters
InputsCrit <- CreateInputsCrit(ErrorCrit_GAPX,
                               InputsModel,
                               RunOptions,
                               Obs = AprParamT,
                               VarObs = "ParamT")
ErrorCrit <- ErrorCrit_GAPX(InputsCrit, OutputsModel)
str(ErrorCrit)
}


\author{
David Dorchies
}

\references{
de Lavenne, A., Andréassian, V., Thirel, G., Ramos, M.-H. and Perrin, C. (2019).
  A Regularization Approach to Improve the Sequential Calibration of a Semidistributed Hydrological Model.
  Water Resources Research 55, 8821–8839. \doi{10.1029/2018WR024266}
}

\seealso{
\code{\link{CreateInputsCrit}}, \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}},
\code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}
}


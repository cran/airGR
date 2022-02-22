\encoding{UTF-8}


\name{CreateInputsCrit_Lavenne}
\alias{CreateInputsCrit_Lavenne}


\title{Creation of the InputsCrit object for Lavenne Criterion}


\description{
Creation of the \code{InputsCrit} object required to the \code{\link{ErrorCrit}} function. This function defines a composite criterion based on the formula proposed by Lavenne et al. (2019).
}


\usage{
CreateInputsCrit_Lavenne(FUN_CRIT = ErrorCrit_KGE,
                         InputsModel,
                         RunOptions,
                         Obs,
                         VarObs = "Q",
                         AprParamR,
                         AprCrit = 1,
                         k = 0.15,
                         BoolCrit = NULL,
                         transfo = "sqrt",
                         epsilon = NULL)
}


\arguments{
\item{FUN_CRIT}{[function] error criterion function (e.g. \code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_NSE}}). Default \code{\link{ErrorCrit_KGE}}}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Obs}{[numeric (atomic or list)] series of observed variable ([mm/time step] for discharge or SWE, [-] for SCA)}

\item{VarObs}{(optional) [character (atomic or list)] names of the observed variable (\code{"Q"} by default, or one of \code{"SCA"}, \code{"SWE"})}

\item{AprParamR}{[numeric] a priori parameter set from a donor catchment}

\item{AprCrit}{(optional) [numeric] performance criterion of the donor catchment (1 by default)}

\item{k}{(optional) [numeric] coefficient used for the weighted average between \code{FUN_CRIT} and the gap between the optimised parameter set and an a priori parameter set calculated with the function produced by \code{\link{CreateErrorCrit_GAPX}}}

\item{BoolCrit}{(optional) [boolean] boolean (the same length as \code{Obs}) giving the time steps to consider in the computation (all time steps are considered by default. See details)}

\item{transfo}{(optional) [character] name of the transformation applied to the variables (e.g. \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"}, \code{"sort"}, \code{"boxcox"} or a numeric value for power transformation for \code{FUN_CRIT}. Default value is \code{"sqrt"}. See details of \code{\link{CreateInputsCrit}}}

\item{epsilon}{(optional) [numeric] small value to add to all observations and simulations for \code{FUN_CRIT} when \code{"log"} or \code{"inv"} transformations are used [same unit as \code{Obs}]. See details of \code{\link{CreateInputsCrit}}}
}


\value{

[list] object of class InputsCrit containing the data required to evaluate the model outputs (see \code{\link{CreateInputsCrit}} for more details).

\code{CreateInputsCrit_Lavenne} returns an object of class \emph{Compo}.

Items \code{Weights} of the criteria are respectively equal to \code{k} and \code{k * max(0, AprCrit)}.

To calculate the Lavenne criterion, it is necessary to use the \code{ErrorCrit} function as for any other composite criterion.
}


\details{

The parameters \code{FUN_CRIT}, \code{Obs}, \code{VarObs}, \code{BoolCrit}, \code{transfo}, and \code{epsilon} must be used as they would be used for \code{\link{CreateInputsCrit}} in the case of a single criterion.

\code{\link{ErrorCrit_RMSE}} cannot be used in a composite criterion since it is not a unitless value.


\code{CreateInputsCrit_Lavenne} creates a composite criterion in respect with Equations 1 and 2 of de Lavenne et al. (2019).
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

## The "a priori" parameters for the Lavenne formula
AprParamR <- c(X1 = 157, X2 = 0.8, X3 = 100, X4 = 1.5)

## Single efficiency criterion: GAPX with a priori parameters
IC_DL <- CreateInputsCrit_Lavenne(InputsModel = InputsModel,
                                    RunOptions = RunOptions,
                                    Obs = BasinObs$Qmm[Ind_Run],
                                    AprParamR = AprParamR)
str(ErrorCrit(InputsCrit = IC_DL, OutputsModel = OutputsModel))
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
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateCalibOptions}}, \code{\link{ErrorCrit}}
}


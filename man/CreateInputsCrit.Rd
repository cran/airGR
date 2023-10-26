\encoding{UTF-8}


\name{CreateInputsCrit}
\alias{CreateInputsCrit}


\title{Creation of the InputsCrit object required to the ErrorCrit functions}


\description{
Creation of the \code{InputsCrit} object required to the \code{ErrorCrit_*} functions. This function is used to define whether the user wants to calculate a single criterion, multiple criteria in the same time, or a composite criterion, which averages several criteria.
}


\usage{
CreateInputsCrit(FUN_CRIT, InputsModel, RunOptions,
                 Obs, VarObs = "Q", BoolCrit = NULL,
                 transfo = "", Weights = NULL,
                 epsilon = NULL,
                 warnings = TRUE)
}


\arguments{
\item{FUN_CRIT}{[function (atomic or list)] error criterion function (e.g. \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}})}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Obs}{[numeric (atomic or list)] series of observed variable ([mm/time step] for discharge or SWE, [-] for SCA)}

\item{VarObs}{(optional) [character (atomic or list)] names of the observed variable (\code{"Q"} by default, or one of \code{"SCA"}, \code{"SWE"}])}

\item{BoolCrit}{(optional) [boolean (atomic or list)] boolean (the same length as \code{Obs}) giving the time steps to consider in the computation (all time steps are considered by default. See details)}

\item{transfo}{(optional) [character (atomic or list)] name of the transformation applied to the variables (e.g. \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"}, \code{"sort"}, \code{"boxcox"} or a numeric value for power transformation . See details)}

\item{Weights}{(optional) [numeric (atomic or list)] vector of weights necessary to calculate a composite criterion (the same length as \code{FUN_CRIT}) giving the weights to use for elements of \code{FUN_CRIT} [-]. See details}

\item{epsilon}{(optional) [numeric (atomic or list)] small value to add to all observations and simulations when \code{"log"} or \code{"inv"} transformations are used [same unit as \code{Obs}]. See details}

\item{warnings}{(optional) [boolean] boolean indicating if the warning messages are shown, default = \code{TRUE}}
}


\value{
[list] object of class \emph{InputsCrit} containing the data required to evaluate the model outputs; it can include the following:
  \tabular{ll}{
    \emph{$FUN_CRIT  } \tab [function] error criterion function (e.g. \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}}) \cr
    \emph{$Obs       } \tab [numeric] series of observed variable(s) ([mm/time step] for discharge or SWE, [-] for SCA) \cr
    \emph{$VarObs    } \tab [character] names of the observed variable(s) \cr
    \emph{$BoolCrit  } \tab [boolean] boolean giving the time steps considered in the computation \cr
    \emph{$transfo   } \tab [character] name of the transformation (e.g. \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"}, \code{"sort"}, \code{"boxcox"} or a number for power transformation) \cr
    \emph{$epsilon   } \tab [numeric] small value to add to all observations and simulations when \code{"log"} or \code{"inv"} transformations are used [same unit as \code{Obs}] \cr
    \emph{$Weights   } \tab [numeric] vector (same length as \code{VarObs}) giving the weights to use for elements of \code{FUN_CRIT} [-] \cr
  }

When \code{Weights = NULL}, \code{CreateInputsCrit} returns an object of class \emph{Single} that is  a list such as the one described above. \cr
When \code{Weights} contains at least one \code{NULL} value and \code{Obs} contains a list of observations, \code{CreateInputsCrit} returns an object of class \emph{Multi} that is a list of lists such as the one described above. The \code{\link{ErrorCrit}} function will then compute the different criteria prepared by \code{CreateInputsCrit}. \cr
When \code{Weights} is a list of at least 2 numerical values, \code{CreateInputsCrit} returns an object of class \emph{Compo} that is a list of lists such as the one described above. This object will be useful to compute composite criterion with the \code{\link{ErrorCrit}} function. \cr
To calculate composite or multiple criteria, it is necessary to use the \code{ErrorCrit} function. The other \code{ErrorCrit_*} functions (e.g. \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}}) can only use objects of class \emph{Single} (and not \emph{Multi} or \emph{Compo}). \cr
}


\details{
Users wanting to use \code{FUN_CRIT} functions that are not included in the package must create their own InputsCrit object accordingly. \cr \cr

## --- Period of calculation

Criteria can be calculated over discontinuous periods (i.e. only over winter periods, or when observed discharge is below a certain threshold). To do so, please indicate in \code{Bool_Crit} which indices must be used for calcullation. Discontinuous periods are allowed in the \code{Bool_Crit} argument.

## --- Transformations

Transformations are simple functions applied to the observed and simulated variables used in order to change their distribution. Transformations are often used in hydrology for modifying the weight put on errors made for high flows or low flows. The following transformations are available: \cr \cr
\itemize{
  \item \code{""}: no transformation is used (default case)
  \item \code{"sqrt"}: squared root transformation
  \item \code{"log"}: logarithmic transformation (see below regarding the specific case of KGE or KGE2)
  \item \code{"inv"}: inverse transformation
  \item \code{"sort"}: sort transformation (the simulated and observed variables are sorted from lowest to highest)
  \item \code{"boxcox"}: Box-Cox transformation (see below for details)
  \item numeric: power transformation (see below for details)
}
We do not advise computing KGE or KGE' with log-transformation as it might be wrongly influenced by discharge values close to 0 or 1 and the criterion value is dependent on the discharge unit. See Santos et al. (2018) for more details and alternative solutions (see the references list below). \cr \cr
In order to make sure that KGE and KGE2 remain dimensionless and are not impacted by zero values, the Box-Cox transformation (\code{transfo = "boxcox"}) uses the formulation given in Equation 10 of Santos et al. (2018). Lambda is set to 0.25 accordingly. \cr \cr
The syntax of the power transformation allows a numeric or a string of characters. For example for a squared transformation, the following can be used: \code{transfo = 2}, \code{transfo = "2"} or \code{transfo = "^2"}. Negative values are allowed. Fraction values are not allowed (e.g., \code{"-1/2"} must instead be written \code{"-0.5"}).\cr \cr

## --- The epsilon value

The epsilon value is useful when \code{"log"} or \code{"inv"} transformations are used (to avoid calculation of the inverse or of the logarithm of zero). If an epsilon value is provided, then it is added to the observed and simulated variable time series at each time step and before the application of a transformation. The epsilon value has no effect when the \code{"boxcox"} transformation is used. The impact of this value and a recommendation about the epsilon value to use (usually one hundredth of average observation) are discussed in Pushpalatha et al. (2012) for NSE and in Santos et al. (2018) for KGE and KGE'. \cr \cr

## --- Single, multiple or composite criteria calculation

Users can set the following arguments as atomic or list: \code{FUN_CRIT}, \code{Obs}, \code{VarObs}, \code{BoolCrit}, \code{transfo}, \code{Weights}. If the list format is chosen, all the lists must have the same length. \cr
Calculation of a single criterion (e.g. NSE computed on discharge) is prepared by providing to \code{CreateInputsCrit} arguments atomics only. \cr
Calculation of multiple criteria (e.g. NSE computed on discharge and RMSE computed on discharge) is prepared by providing to \code{CreateInputsCrit} arguments lists except for \code{Weights} that must be set as \code{NULL}. \cr
Calculation of a composite criterion (e.g. the average between NSE computed on discharge and NSE computed on log of discharge) is prepared by providing to \code{CreateInputsCrit} arguments lists including \code{Weights}. \cr
\code{\link{ErrorCrit_RMSE}} cannot be used in a composite criterion since it is not a unitless value.
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
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

## single efficiency criterion: Nash-Sutcliffe Efficiency
InputsCritSingle <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                     InputsModel = InputsModel, RunOptions = RunOptions,
                                     Obs = list(BasinObs$Qmm[Ind_Run]),
                                     VarObs = "Q", transfo = "",
                                     Weights = NULL)
str(InputsCritSingle)
invisible(ErrorCrit(InputsCrit = InputsCritSingle, OutputsModel = OutputsModel))

## 2 efficiency criteria: RMSE and Nash-Sutcliffe Efficiency
InputsCritMulti <- CreateInputsCrit(FUN_CRIT = list(ErrorCrit_RMSE, ErrorCrit_NSE),
                                    InputsModel = InputsModel, RunOptions = RunOptions,
                                    Obs = list(BasinObs$Qmm[Ind_Run],
                                               BasinObs$Qmm[Ind_Run]),
                                    VarObs = list("Q", "Q"), transfo = list("", "sqrt"),
                                    Weights = NULL)
str(InputsCritMulti)
invisible(ErrorCrit(InputsCrit = InputsCritMulti, OutputsModel = OutputsModel))

## efficiency composite criterion: Nash-Sutcliffe Efficiency mixing
##                                 both raw and log-transformed flows
InputsCritCompo <- CreateInputsCrit(FUN_CRIT = list(ErrorCrit_NSE, ErrorCrit_NSE),
                                    InputsModel = InputsModel, RunOptions = RunOptions,
                                    Obs = list(BasinObs$Qmm[Ind_Run],
                                               BasinObs$Qmm[Ind_Run]),
                                    VarObs = list("Q", "Q"), transfo = list("", "log"),
                                    Weights = list(0.4, 0.6))
str(InputsCritCompo)
invisible(ErrorCrit(InputsCrit = InputsCritCompo, OutputsModel = OutputsModel))
}


\author{
Olivier Delaigue, Laurent Coron, Guillaume Thirel
}


\references{
Pushpalatha, R., Perrin, C., Le Moine, N. and Andréassian, V. (2012).
  A review of efficiency criteria suitable for evaluating low-flow simulations.
  Journal of Hydrology, 420-421, 171-182, doi: 10.1016/j.jhydrol.2011.11.055.
\cr\cr
Santos, L., Thirel, G. and Perrin, C. (2018).
  Technical note: Pitfalls in using log-transformed flows within the KGE criterion.
  Hydrol. Earth Syst. Sci., 22, 4583-4591, doi: 10.5194/hess-22-4583-2018.
}


\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateCalibOptions}}, \code{\link{ErrorCrit}}
}


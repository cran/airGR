\encoding{UTF-8}


\name{CreateRunOptions}
\alias{CreateRunOptions}


\title{Creation of the RunOptions object required to the RunModel functions}


\description{
Creation of the RunOptions object required to the \code{RunModel*} functions.
}


\usage{
CreateRunOptions(FUN_MOD, InputsModel,
                 IndPeriod_WarmUp = NULL, IndPeriod_Run,
                 IniStates = NULL, IniResLevels = NULL, Imax = NULL,
                 Outputs_Cal = NULL, Outputs_Sim = "all",
                 MeanAnSolidPrecip = NULL, IsHyst = FALSE,
                 warnings = TRUE, verbose = TRUE)
}


\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. \code{\link{RunModel_GR4J}}, \code{\link{RunModel_CemaNeigeGR4J}})}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{IndPeriod_WarmUp}{(optional) [numeric] index of period to be used for the model warm-up [-]. See details}

\item{IndPeriod_Run}{[numeric] index of period to be used for the model run [-]. See details}

\item{IniStates}{(optional) [numeric] object of class \code{IniStates} [mm and °C], see \code{\link{CreateIniStates}} for details}

\item{IniResLevels}{(optional) [numeric] vector of initial fillings for the GR stores (4 values; use NA when not relevant for a given model) [- and/or mm]. See details}

\item{Imax}{(optional) [numeric] an atomic vector of the maximum capacity of the GR5H interception store [mm]; see \code{\link{RunModel_GR5H}}}

\item{Outputs_Cal}{(optional) [character] vector giving the outputs needed for the calibration \cr (e.g. c("Qsim")), the fewer outputs
 the faster the calibration}

\item{Outputs_Sim}{(optional) [character] vector giving the requested outputs \cr (e.g. c(\code{"DatesR"}, \code{"Qsim"}, \code{"SnowPack"})), default = \code{"all"}}

\item{MeanAnSolidPrecip}{(optional) [numeric] vector giving the annual mean of average solid precipitation for each layer (computed from InputsModel if not defined) [mm/y]}

\item{IsHyst}{[boolean] boolean indicating if the hysteresis version of CemaNeige is used. See details}

\item{warnings}{(optional) [boolean]  boolean indicating if the warning messages are shown, default = \code{TRUE}}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}
}


\value{
[list] object of class \emph{RunOptions} containing the data required to evaluate the model outputs; it can include the following:
  \tabular{ll}{
    \emph{IndPeriod_WarmUp } \tab [numeric] index of period to be used for the model warm-up [-] \cr
    \emph{IndPeriod_Run    } \tab [numeric] index of period to be used for the model run [-] \cr
    \emph{IniStates        } \tab [numeric] vector of initial model states [mm and °C] \cr
    \emph{IniResLevels     } \tab [numeric] vector of initial filling rates for production and routing stores [-] and level for the exponential store for GR6J [mm]\cr
    \emph{Outputs_Cal      } \tab [character] character vector giving only the outputs needed for the calibration \cr
    \emph{Outputs_Sim      } \tab [character] character vector giving the requested outputs \cr
    \emph{Imax             } \tab [numeric] vector giving the maximal capacity of the GR5H interception store \cr
    \emph{MeanAnSolidPrecip} \tab [numeric] vector giving the annual mean of average solid precipitation for each layer [mm/y] \cr
  }
}


\details{
Users wanting to use \code{FUN_MOD} functions that are not included in
the package must create their own \code{RunOptions} object accordingly.

## --- IndPeriod_WarmUp and IndPeriod_Run

Since the hydrological models included in airGR are continuous models, meaning that internal states of the models are propagated to the next time step, \code{IndPeriod_WarmUp} and \code{IndPeriod_Run} must be continuous periods, represented by continuous indices values; no gaps are allowed. To calculate criteria or to calibrate a model over discontinuous periods, please see the \code{Bool_Crit} argument of the \code{\link{CreateInputsCrit}} function.

## --- Initialisation options

The model initialisation options can either be set to a default configuration or be defined by the user.

This is done via three vectors: \cr \code{IndPeriod_WarmUp}, \code{IniStates}, \code{IniResLevels}. \cr
A default configuration is used for initialisation if these vectors are not defined.

(1) Default initialisation options:

\itemize{
\item \code{IndPeriod_WarmUp} default setting ensures a one-year warm-up using the time steps preceding the \code{IndPeriod_Run}.
The actual length of this warm-up might be shorter depending on data availability (no missing value of climate inputs being allowed in model input series).

\item \code{IniStates} and \code{IniResLevels} are automatically set to initialise all the model states at 0, except for the production and routing stores levels which are respectively initialised at 30 \% and 50 \% of their capacity. In case GR5H is used with an interception store, the intercetion store level is initialised by default with 0 mm. In case GR6J is used, the exponential store level is initialised by default with 0 mm. This initialisation is made at the very beginning of the model call (i.e. at the beginning of \code{IndPeriod_WarmUp} or at the beginning of \code{IndPeriod_Run} if the warm-up period is disabled).
}

(2) Customisation of initialisation options:

\itemize{
\item \code{IndPeriod_WarmUp} can be used to specify the indices of the warm-up period (within the time series prepared in InputsModel).
\itemize{
\item remark 1:	for most common cases, indices corresponding to one or several years preceding \code{IndPeriod_Run} are used (e.g. \code{IndPeriod_WarmUp = 1000:1365} and \code{IndPeriod_Run = 1366:5000)}. \cr
However, it is also possible to perform a long-term initialisation if other indices than the warm-up ones are set in \code{IndPeriod_WarmUp} (e.g. \code{IndPeriod_WarmUp = c(1:5000, 1:5000, 1:5000, 1000:1365)}). \cr
\item remark 2:	it is also possible to completely disable the warm-up period when using \code{IndPeriod_WarmUp = 0L}. This is necessary if you want \code{IniStates} and/or \code{IniResLevels} to be the actual initial values of the model variables from your simulation (e.g. to perform a forecast form a given initial state).
}
\item \code{IniStates} and \code{IniResLevels} can be used to specify the initial model states.
\itemize{
\item remark 1: \code{IniStates} and \code{IniResLevels} can not be used with GR1A. \cr
\item remark 2:	if \code{IniStates} is used, two possibilities are offered:\cr
- \code{IniStates} can be set to the \emph{$StateEnd} output of a previous \code{RunModel} call, as \emph{$StateEnd} already respects the correct format; \cr
- \code{IniStates} can be created with the \code{\link{CreateIniStates}} function.
\item remark 3:	in addition to \code{IniStates}, \code{IniResLevels} allows to set the filling rate of the production and routing stores for the GR models. For instance for GR4J and GR5J: \code{IniResLevels = c(0.3, 0.5, NA, NA)} should be used to obtain initial fillings of 30 \% and 50 \% for the production and routing stores, respectively. For GR6J, \code{IniResLevels = c(0.3, 0.5, 0, NA)} should be used to obtain initial fillings of 30 \% and 50 \% for the production and routing stores levels and 0 mm for the exponential store level, respectively. For GR5H with an interception store, \code{IniResLevels = c(0.3, 0.5, NA, 0.4)} should be used to obtain initial fillings of 30 \%, 50 \% and 40 \% for the production, routing and interception stores levels, respectively. \code{IniResLevels} is optional and can only be used if \code{IniStates} is also defined (the state values corresponding to these two other stores in \code{IniStates} are not used in such case).
}
}

## --- CemaNeige version

If \code{IsHyst = FALSE}, the original CemaNeige version from Valéry et al. (2014) is used.  \cr
If \code{IsHyst = TRUE}, the CemaNeige version from Riboust et al. (2019) is used. Compared to the original version, this version of CemaNeige needs two more parameters and it includes a representation of the hysteretic relationship between the Snow Cover Area (SCA) and the Snow Water Equivalent (SWE) in the catchment. The hysteresis included in airGR is the Modified Linear hysteresis (LH*); it is represented on panel b) of Fig. 3 in Riboust et al. (2019). Riboust et al. (2019) advise to use the LH* version of CemaNeige with parameters calibrated using an objective function combining 75 \% of KGE calculated on discharge simulated from a rainfall-runoff model compared to observed discharge and 5 \% of KGE calculated on SCA on 5 CemaNeige elevation bands compared to satellite (e.g. MODIS) SCA (see Eq. (18), Table 3 and Fig. 6). Riboust et al. (2019)'s tests were realized with GR4J as the chosen rainfall-runoff model. \cr
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
OutputsModel <- RunModel(InputsModel = InputsModel,
                         RunOptions = RunOptions, Param = Param,
                         FUN_MOD = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions,
                                Obs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}


\author{
Laurent Coron, Olivier Delaigue, Guillaume Thirel
}


\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateInputsCrit}},
\code{\link{CreateCalibOptions}}, \code{\link{CreateIniStates}}, \code{\link{Imax}}
}

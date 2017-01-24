\encoding{UTF-8}
\name{CreateRunOptions}
\alias{CreateRunOptions}
\title{Creation of the RunOptions object required to the RunModel functions}
\usage{
CreateRunOptions(FUN_MOD, InputsModel, IndPeriod_WarmUp = NULL, IndPeriod_Run,
  IniStates = NULL, IniResLevels = NULL, Outputs_Cal = NULL,
  Outputs_Sim = "all", RunSnowModule = TRUE, MeanAnSolidPrecip = NULL,
  verbose = TRUE)
}
\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. RunModel_GR4J, RunModel_CemaNeigeGR4J)}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{IndPeriod_WarmUp}{(optional) [numeric] index of period to be used for the model warm-up [-]}

\item{IndPeriod_Run}{[numeric] index of period to be used for the model run [-]}

\item{IniStates}{(optional) [numeric] vector of initial model internal states [mm]}

\item{IniResLevels}{(optional) [numeric] vector of initial filling rates for production and routing stores (2 values between 0 and 1) [-]}

\item{Outputs_Cal}{(optional) [character] vector giving the outputs needed for the calibration \cr (e.g. c("Qsim")), the fewer outputs
 the faster the calibration}

\item{Outputs_Sim}{(optional) [character] vector giving the requested outputs \cr (e.g. c("DatesR","Qsim","SnowPack")), default="all"}

\item{RunSnowModule}{(optional) [boolean] option indicating whether CemaNeige should be activated, default=TRUE}

\item{MeanAnSolidPrecip}{(optional) [numeric] vector giving the annual mean of average solid precipitation for each layer (computed from InputsModel if not defined) [mm/y]}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default=TRUE}
}
\value{
[list] object of class \emph{RunOptions} containing the data required to evaluate the model outputs; it can include the following:
         \tabular{ll}{
         \emph{IndPeriod_WarmUp }  \tab  [numeric] index of period to be used for the model warm-up [-] \cr
         \emph{IndPeriod_Run    }  \tab  [numeric] index of period to be used for the model run [-] \cr
         \emph{IniStates        }  \tab  [numeric] vector of initial model states [mm] \cr
         \emph{IniResLevels     }  \tab  [numeric] vector of initial filling rates for production and routing stores [-] \cr
         \emph{Outputs_Cal      }  \tab  [character] character vector giving only the outputs needed for the calibration \cr
         \emph{Outputs_Sim      }  \tab  [character] character vector giving the requested outputs \cr
         \emph{RunSnowModule    }  \tab  [boolean] option indicating whether CemaNeige should be activated \cr
         \emph{MeanAnSolidPrecip}  \tab  [numeric] vector giving the annual mean of average solid precipitation for each layer [mm/y] \cr
         }
}
\description{
Creation of the RunOptions object required to the RunModel functions.
}
\details{
Users wanting to use FUN_MOD functions that are not included in 
the package must create their own RunOptions object accordingly.

##### Initialisation options #####

The model initialisation options can either be set to a default configuration or be defined by the user.

This is done via three vectors: \cr \emph{IndPeriod_WarmUp}, \emph{IniStates}, \emph{IniResLevels}. \cr
A default configuration is used for initialisation if these vectors are not defined.

(1) Default initialisation options:

\itemize{
\item \emph{IndPeriod_WarmUp} default setting ensures a one-year warm-up using the time steps preceding the \emph{IndPeriod_Run}. 
The actual length of this warm-up might be shorter depending on data availability (no missing value of climate inputs being allowed in model input series).

\item \emph{IniStates} and \emph{IniResLevels} are automatically set to initialise all the model states at 0, except for the production and routing stores which are initialised at 50 \% of their capacity. This initialisation is made at the very beginning of the model call (i.e. at the beginning of \emph{IndPeriod_WarmUp} or at the beginning of IndPeriod_Run if the warm-up period is disabled).
}

(2) Customisation of initialisation options:

\itemize{
\item \emph{IndPeriod_WarmUp} can be used to specify the indices of the warm-up period (within the time-series prepared in InputsModel). \cr
- remark 1:	for most common cases, indices corresponding to one or several years preceding \emph{IndPeriod_Run} are used (e.g. \emph{IndPeriod_WarmUp = 1000:1365} and \emph{IndPeriod_Run = 1366:5000)}. \cr
However, it is also possible to perform a long-term initialisation if other indices than the warm-up ones are set in \emph{IndPeriod_WarmUp} (e.g. \emph{IndPeriod_WarmUp <- c( 1:5000 , 1:5000 , 1:5000 ,1000:1365 )}). \cr
- remark 2:	it is also possible to completely disable the warm-up period when using \emph{IndPeriod_WarmUp = 0L}.

\item \emph{IniStates} and \emph{IniResLevels} can be used to specify the initial model states. \cr
- remark 1:	if \emph{IniStates} is used, all model states must be provided (e.g. 60 floats [mm] are required for GR4J, GR5J and GR6J; 60+2*NLayers floats [mm] are required for CemaNeigeGR4J, CemaNeigeGR5J and CemaNeigeGR6J; see fortran source code for details). \cr
- remark 2:	in addition to \emph{IniStates}, \emph{IniResLevels} allows to set the filling rate of the production and routing stores for the GR models. For instance for GR4J, GR5J and GR6J: \emph{IniResLevels <- c(0.3,0.5)} should be used to obtain initial fillings of 30\% and 50\% for the production and routing stores, respectively.  \emph{IniResLevels} is optional and can only be used if \emph{IniStates} is also defined (the state values corresponding to these two stores in \emph{IniStates} are not used in such case). \cr \cr
}
}
\examples{
library(airGR)

## loading catchment data
data(L0123001)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="01/01/1990 00:00"), 
               which(format(BasinObs$DatesR, format = "\%d/\%m/\%Y \%H:\%M")=="31/12/1999 00:00"))

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## simulation
Param <- c(734.568, -0.840, 109.809, 1.971)
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param, 
                         FUN_MOD = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                                RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}
\author{
Laurent Coron (June 2014)
}
\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}
}


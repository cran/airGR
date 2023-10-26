\encoding{UTF-8}


\name{CreateIniStates}
\alias{CreateIniStates}


\title{Creation of the IniStates object possibly required by the CreateRunOptions functions}


\description{
Creation of the \emph{IniStates} object possibly required by the \code{\link{CreateRunOptions}} function.
}


\usage{
CreateIniStates(FUN_MOD, InputsModel,
                IsHyst = FALSE, IsIntStore = FALSE,
                ProdStore = 350, RoutStore = 90,
                ExpStore = NULL, IntStore = NULL,
                UH1 = NULL, UH2 = NULL,
                GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                GthrCemaNeigeLayers = NULL, GlocmaxCemaNeigeLayers = NULL,
                SD = NULL, verbose = TRUE)
}


\arguments{
\item{FUN_MOD}{[function] hydrological model function (e.g. RunModel_GR4J, RunModel_CemaNeigeGR4J)}

\item{InputsModel}{[object of class \code{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{IsHyst}{[boolean] boolean indicating if the hysteresis version of CemaNeige is used. See details}

\item{IsIntStore}{[boolean] boolean indicating if the interception store is used in GR5H. See details}

\item{ProdStore}{[numeric] production store level [mm] for all GR models except GR1A}

\item{RoutStore}{[numeric] routing store level [mm] for all GR models except GR1A}

\item{ExpStore}{(optional) [numeric] series of exponential store level (negative) [mm] for the GR6J model}

\item{IntStore}{(optional) [numeric] series of rainfall neutralisation or interception store level [mm] for the GR5H model}

\item{UH1}{(optional) [numeric] unit hydrograph 1 levels [mm]}

\item{UH2}{(optional) [numeric] unit hydrograph 2 levels [mm]}

\item{GCemaNeigeLayers}{(optional) [numeric] snow pack [mm], possibly used to create the CemaNeige model initial state}

\item{eTGCemaNeigeLayers}{(optional) [numeric] snow pack thermal state [°C], possibly used to create the CemaNeige model initial state}

\item{GthrCemaNeigeLayers}{(optional) [numeric] melt threshold [mm], possibly used to create the CemaNeige model initial state in case the Linear Hysteresis version is used}

\item{GlocmaxCemaNeigeLayers}{(optional) [numeric] local melt threshold for hysteresis [mm], possibly used to create the CemaNeige model initial state in case the Linear Hysteresis version is used}

\item{SD}{(optional) [list] of [numeric] states of delayed upstream flows for semi-distributed models, the nature of the state and the unit depend on the model and the unit of the upstream flow}

\item{verbose}{(optional) [boolean] boolean indicating if the function is run in verbose mode or not, default = \code{TRUE}}

}


\value{
[list] object of class \code{IniStates} containing the initial model internal states; it always includes the following:
  \tabular{ll}{
    \emph{$Store          } \tab [numeric] list of store levels (\emph{$Prod}, \emph{$Rout} and \emph{$Exp}) \cr
    \emph{$UH             } \tab [numeric] list of unit hydrographs levels (\emph{$UH1} and \emph{$UH2}) \cr
    \emph{$CemaNeigeLayers} \tab [numeric] list of CemaNeige variables (\emph{$G}, \emph{$eTG}, \emph{$GthrCemaNeigeLayers} and \emph{$GlocmaxCemaNeigeLayers})
  }
}


\details{
20 numeric values are required for UH1 and 40 numeric values are required for UH2 if GR4J, GR5J or GR6J are used (respectively 20*24 and 40*24 for the hourly models GR4H and GR5H). Please note that depending on the X4 parameter value that will be provided when running the model, not all the values may be used (only the first int(X4)+1 values are used for UH1 and the first 2*int(X4)+1 for UH2). \cr
\code{GCemaNeigeLayers} and \code{eTGCemaNeigeLayers} require each numeric values as many as given in \code{\link{CreateInputsModel}} with the \code{NLayers} argument. \code{eTGCemaNeigeLayers} values can be negative.\cr
The structure of the object of class \code{IniStates} returned is always exactly the same for all models (except for the unit hydrographs levels that contain more values with GR4H and GR5H), even if some states do not exist (e.g. \emph{$UH$UH1} for GR2M).  \cr
If CemaNeige is not used, \emph{$CemaNeigeLayers$G}, \emph{$CemaNeigeLayers$eTG} \emph{$CemaNeigeLayers$GthrCemaNeigeLayers} and \emph{$CemaNeigeLayers$GlocmaxCemaNeigeLayers} are set to \code{NA}. \cr

Nota: the \code{StateEnd} objects from the outputs of \code{RunModel*} functions already respect the format given by the \code{CreateIniStates} function.
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


### preparation of the IniStates object with low values of ProdStore and RoutStore
IniStates <- CreateIniStates(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                             ProdStore = 0, RoutStore = 0, ExpStore = NULL,
                             UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
                             UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
                             GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                             GthrCemaNeigeLayers = NULL, GlocmaxCemaNeigeLayers = NULL)
str(IniStates)

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                               IndPeriod_WarmUp = 0L,
                               IndPeriod_Run = Ind_Run, IniStates = IniStates)

## simulation
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])


### preparation of the IniStates object with high values of ProdStore and RoutStore
IniStates <- CreateIniStates(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                             ProdStore = 450, RoutStore = 100, ExpStore = NULL,
                             UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
                             UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
                             GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                             GthrCemaNeigeLayers = NULL, GlocmaxCemaNeigeLayers = NULL)
str(IniStates)

## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J, InputsModel = InputsModel,
                               IndPeriod_WarmUp = 0L,
                               IndPeriod_Run = Ind_Run, IniStates = IniStates)

## simulation
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## results preview
plot(OutputsModel, Qobs = BasinObs$Qmm[Ind_Run])
}


\author{
Olivier Delaigue
}


\seealso{
\code{\link{CreateRunOptions}}
}

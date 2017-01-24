\encoding{UTF-8}
\name{CreateInputsCrit}
\alias{CreateInputsCrit}
\title{Creation of the InputsCrit object required to the ErrorCrit functions}
\usage{
CreateInputsCrit(FUN_CRIT, InputsModel, RunOptions, Qobs, BoolCrit = NULL,
  transfo = "", Ind_zeroes = NULL, epsilon = NULL)
}
\arguments{
\item{FUN_CRIT}{[function] error criterion function (e.g. ErrorCrit_RMSE, ErrorCrit_NSE)}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link{CreateInputsModel}} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link{CreateRunOptions}} for details}

\item{Qobs}{[numeric] series of observed discharges [mm/time step]}

\item{BoolCrit}{(optional) [boolean] boolean giving the time steps to consider in the computation (all time steps are consider by default)}

\item{transfo}{(optional) [character] name of the transformation (e.g. "", "sqrt", "log", "inv", "sort")}

\item{Ind_zeroes}{(optional) [numeric] indices of the time steps where zeroes are observed}

\item{epsilon}{(optional) [numeric] epsilon to add to all Qobs and Qsim if \emph{$Ind_zeroes} is not empty}
}
\value{
[list] object of class \emph{InputsCrit} containing the data required to evaluate the model outputs; it can include the following:
         \tabular{ll}{
         \emph{$BoolCrit  }  \tab   [boolean] boolean giving the time steps considered in the computation \cr
         \emph{$Qobs      }  \tab   [numeric] series of observed discharges [mm/time step] \cr
         \emph{$transfo   }  \tab   [character] name of the transformation (e.g. "", "sqrt", "log", "inv", "sort") \cr
         \emph{$Ind_zeroes}  \tab   [numeric] indices of the time steps where zeroes are observed \cr
         \emph{$epsilon   }  \tab   [numeric] epsilon to add to all Qobs and Qsim if \emph{$Ind_zeroes} is not empty \cr
         }
}
\description{
Creation of the InputsCrit object required to the ErrorCrit functions.
}
\details{
Users wanting to use FUN_CRIT functions that are not included in 
the package must create their own InputsCrit object accordingly.
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
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, 
                         Param = Param, FUN = RunModel_GR4J)

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Nash-Sutcliffe Efficiency on log-transformed flows
transfo <- "log"
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run],
                               transfo = transfo)
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Nash-Sutcliffe Efficiency above a threshold (q75\%)
BoolCrit <- rep(TRUE, length(BasinObs$Qmm[Ind_Run])); 
BoolCrit[BasinObs$Qmm[Ind_Run]<quantile(BasinObs$Qmm[Ind_Run], 0.75, na.rm = TRUE)] <- FALSE
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run],
                               BoolCrit = BoolCrit)
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$Qmm[Ind_Run])
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency below a threshold (q10\%) on log-transformed flows
transfo  <- "log"
BoolCrit <- rep(TRUE, length(BasinObs$Qmm[Ind_Run])); 
BoolCrit[BasinObs$Qmm[Ind_Run]>quantile(BasinObs$Qmm[Ind_Run], 0.10, na.rm = TRUE)] <- FALSE
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE,
                               InputsModel = InputsModel,RunOptions = RunOptions, 
                               Qobs = BasinObs$Qmm[Ind_Run], BoolCrit = BoolCrit, transfo = transfo)
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
}
\author{
Laurent Coron (June 2014)
}
\seealso{
\code{\link{RunModel}}, \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateCalibOptions}}
}


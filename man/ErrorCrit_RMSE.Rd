\encoding{UTF-8}


\name{ErrorCrit_RMSE}
\alias{ErrorCrit_RMSE}
\title{Error criterion based on the RMSE}


\usage{
ErrorCrit_RMSE(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
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


\description{
Function which computes an error criterion based on the root mean square error (RMSE).
}


\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) which allows 
the use of the function for model calibration: the product CritValue * Multiplier is the criterion to be minimised 
(Multiplier = +1 for RMSE).
}


\examples{
## see example of the ErrorCrit function
}


\author{
Laurent Coron, Ludovic Oudin
}


\seealso{
\code{\link{ErrorCrit_NSE}}, \code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}
}


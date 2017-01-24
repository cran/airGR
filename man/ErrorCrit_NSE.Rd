\encoding{UTF-8}
\name{ErrorCrit_NSE}
\alias{ErrorCrit_NSE}
\title{Error criterion based on the NSE formula}
\usage{
ErrorCrit_NSE(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
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
         \emph{$Ind_notcomputed}   \tab   [numeric] indices of the time steps where InputsCrit$BoolCrit=FALSE or no data is available \cr
         }
}
\description{
Function which computes an error criterion based on the NSE formula proposed by Nash & Sutcliffe (1970).
}
\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) which allows 
the use of the function for model calibration: the product CritValue*Multiplier is the criterion to be minimised 
(Multiplier=-1 for NSE).
}
\examples{
## see example of the ErrorCrit function
}
\author{
Laurent Coron (June 2014)
}
\references{
Nash, J.E. and Sutcliffe, J.V. (1970), 
      River flow forecasting through conceptual models part 1.
      A discussion of principles, Journal of Hydrology, 10(3), 282-290, doi:10.1016/0022-1694(70)90255-6. \cr
}
\seealso{
\code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}
}


\encoding{UTF-8}
\name{ErrorCrit_KGE2}
\alias{ErrorCrit_KGE2}
\title{Error criterion based on the KGE' formula}
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
         \emph{$Ind_notcomputed}   \tab   [numeric] indices of the time steps where InputsCrit$BoolCrit=FALSE or no data is available \cr
         }
}
\description{
Function which computes an error criterion based on the KGE' formula proposed by Kling et al. (2012).
}
\details{
In addition to the criterion value, the function outputs include a multiplier (-1 or +1) which allows 
the use of the function for model calibration: the product CritValue*Multiplier is the criterion to be minimised (Multiplier=-1 for KGE2).
}
\examples{
## see example of the ErrorCrit function
}
\author{
Laurent Coron (June 2014)
}
\references{
Gupta, H. V., Kling, H., Yilmaz, K. K. and Martinez, G. F. (2009), 
      Decomposition of the mean squared error and NSE performance criteria: Implications
      for improving hydrological modelling, Journal of Hydrology, 377(1-2), 80-91, doi:10.1016/j.jhydrol.2009.08.003. \cr
  Kling, H., Fuchs, M. and Paulin, M. (2012), 
      Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios,
      Journal of Hydrology, 424-425, 264-277, doi:10.1016/j.jhydrol.2012.01.011.
}
\seealso{
\code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}}, \code{\link{ErrorCrit_KGE}}
}


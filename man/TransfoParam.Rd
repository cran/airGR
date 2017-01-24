\encoding{UTF-8}
\name{TransfoParam}
\alias{TransfoParam}
\title{Transformation of the parameters using the provided function}
\usage{
TransfoParam(ParamIn, Direction, FUN_TRANSFO)
}
\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use "RT" for Raw->Transformed and "TR" for Transformed->Raw}

\item{FUN_TRANSFO}{[function] model parameters transformation function (e.g. TransfoParam_GR4J, TransfoParam_CemaNeigeGR4J)}
}
\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}
\description{
Function which transforms model parameters using the provided function (from raw to transformed parameters and vice versa).
}
\examples{
library(airGR)

## transformation Raw->Transformed for the GR4J model
    Xraw  <- matrix(c(+221.41, -3.63,  +30.00, +1.37,
                      +347.23, -1.03,  +60.34, +1.76,
                      +854.06, -0.10, +148.41, +2.34),
                      ncol = 4, byrow = TRUE)
    Xtran <- TransfoParam(ParamIn = Xraw, Direction = "RT", FUN_TRANSFO = TransfoParam_GR4J)

## transformation Transformed->Raw for the GR4J model
    Xtran <- matrix(c(+3.60, -2.00, +3.40, -9.10,
                      +3.90, -0.90, +4.10, -8.70,
                      +4.50, -0.10, +5.00, -8.10),
                      ncol = 4, byrow = TRUE)
    Xraw  <- TransfoParam(ParamIn = Xtran, Direction = "TR", FUN_TRANSFO = TransfoParam_GR4J)
}
\author{
Laurent Coron (June 2014)
}
\seealso{
\code{\link{TransfoParam_GR4J}}, \code{\link{TransfoParam_GR5J}}, \code{\link{TransfoParam_GR6J}}, \code{\link{TransfoParam_CemaNeige}}
}


\encoding{UTF-8}
\name{TransfoParam_CemaNeige}
\alias{TransfoParam_CemaNeige}
\title{Transformation of the parameters of the CemaNeige module}
\usage{
TransfoParam_CemaNeige(ParamIn, Direction)
}
\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use "RT" for Raw->Transformed and "TR" for Transformed->Raw}
}
\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}
\description{
Function which transforms model parameters of the CemaNeige module (from raw to transformed parameters and vice versa).
}
\examples{
library(airGR)

## transformation Raw->Transformed for the CemaNeige module
    Xraw  <- matrix(c(+0.19, +1.73,
                      +0.39, +2.51,
                      +0.74, +4.06),
                      ncol = 2, byrow = TRUE)
    Xtran <- TransfoParam_CemaNeige(ParamIn = Xraw , Direction = "RT")

## transformation Transformed->Raw for the CemaNeige module
    Xtran <- matrix(c(-6.26, +0.55,
                      -2.13, +0.92,
                      +4.86, +1.40),
                      ncol = 2, byrow = TRUE)
    Xraw  <- TransfoParam_CemaNeige(ParamIn = Xtran, Direction = "TR")
}
\author{
Laurent Coron (December 2013)
}
\seealso{
\code{\link{TransfoParam}}, \code{\link{TransfoParam_GR4J}}, \code{\link{TransfoParam_GR5J}}, \code{\link{TransfoParam_GR6J}}
}


\encoding{UTF-8}
\name{TransfoParam_GR5J}
\alias{TransfoParam_GR5J}
\title{Transformation of the parameters of the GR5J model}
\usage{
TransfoParam_GR5J(ParamIn, Direction)
}
\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use "RT" for Raw->Transformed and "TR" for Transformed->Raw}
}
\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}
\description{
Function which transforms model parameters of the GR5J model (from real to transformed parameters and vice versa).
}
\examples{
library(airGR)

## transformation Raw->Transformed for the GR5J model
    Xraw  <- matrix(c(+221.41, -2.65,  +27.11, +1.37, -0.76,
                      +347.23, -0.64,  +60.34, +1.76, +0.30,
                      +854.01, -0.10, +148.41, +2.34, +0.52),
                      ncol = 5, byrow = TRUE)
    Xtran <- TransfoParam_GR5J(ParamIn = Xraw , Direction = "RT")

## transformation Transformed->Raw for the GR5J model
    Xtran <- matrix(c(+3.60, -1.70, +3.30, -9.10, -0.70,
                      +3.90, -0.60, +4.10, -8.70, +0.30,
                      +4.50, -0.10, +5.00, -8.10, +0.50),
                      ncol = 5, byrow = TRUE)
    Xraw  <- TransfoParam_GR5J(ParamIn = Xtran, Direction = "TR")
}
\author{
Laurent Coron (December 2013)
}
\seealso{
\code{\link{TransfoParam}}, \code{\link{TransfoParam_GR4J}}, \code{\link{TransfoParam_GR6J}}, \code{\link{TransfoParam_CemaNeige}}
}


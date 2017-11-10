\encoding{UTF-8}


\name{TransfoParam_GR6J}
\alias{TransfoParam_GR6J}


\title{Transformation of the parameters of the GR6J model}


\usage{
TransfoParam_GR6J(ParamIn, Direction)
}


\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use \code{"RT"} for Raw -> Transformed and \code{"TR"} for Transformed -> Raw}
}


\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}


\description{
Function which transforms model parameters of the GR6J model (from real to transformed parameters and vice versa).
}


\examples{
library(airGR)

## transformation Raw -> Transformed for the GR6J model
Xraw  <- matrix(c(+221.41, -1.18,  +27.11, 1.37, -0.18,  +20.09,
                  +347.23, -0.52,  +60.34, 1.76, +0.02,  +54.60,
                  +854.06, +0.52, +148.41, 2.34, +0.22, +148.41),
                  ncol = 6, byrow = TRUE)
Xtran <- TransfoParam_GR6J(ParamIn = Xraw , Direction = "RT")

## transformation Transformed -> Raw for the GR6J model
Xtran <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00,
                  +3.90, -0.50, +4.10, -8.70, +0.10, +4.00,
                  +4.50, +0.50, +5.00, -8.10, +1.10, +5.00),
                  ncol = 6, byrow = TRUE)
Xraw  <- TransfoParam_GR6J(ParamIn = Xtran, Direction = "TR")
}


\author{
Laurent Coron
}


\seealso{
\code{\link{TransfoParam}}, \code{\link{TransfoParam_GR4J}}, \code{\link{TransfoParam_GR5J}}, \code{\link{TransfoParam_CemaNeige}}
}


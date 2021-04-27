\encoding{UTF-8}


\name{TransfoParam}
\alias{TransfoParam}
\alias{TransfoParam_GR1A}
\alias{TransfoParam_GR2M}
\alias{TransfoParam_GR4J}
\alias{TransfoParam_GR5J}
\alias{TransfoParam_GR6J}
\alias{TransfoParam_GR4H}
\alias{TransfoParam_GR5H}
\alias{TransfoParam_CemaNeige}
\alias{TransfoParam_CemaNeigeHyst}
\alias{TransfoParam_Lag}


\title{Transformation of the parameters using the provided function}


\description{
Function which transforms model parameters using the provided function (from raw to transformed parameters and vice versa).
}


\usage{
## Generic function
TransfoParam(ParamIn, Direction, FUN_TRANSFO)

## Specific functions
TransfoParam_GR1A(ParamIn, Direction)
TransfoParam_GR2M(ParamIn, Direction)
TransfoParam_GR4J(ParamIn, Direction)
TransfoParam_GR5J(ParamIn, Direction)
TransfoParam_GR6J(ParamIn, Direction)
TransfoParam_GR4H(ParamIn, Direction)
TransfoParam_GR5H(ParamIn, Direction)
TransfoParam_CemaNeige(ParamIn, Direction)
TransfoParam_CemaNeigeHyst(ParamIn, Direction)
}


\arguments{
\item{ParamIn}{[numeric] vector or matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use \code{"RT"} for Raw -> Transformed and \code{"TR"} for Transformed -> Raw}

\item{FUN_TRANSFO}{[function] model parameters transformation function (e.g. \code{TransfoParam_GR4J}, \code{TransfoParam_CemaNeige})}
}


\details{
The transformation functions proposed in airGR for calibrating the GR models result from numerous testings at INRAE-Antony (HYCAR Research Unit, France). The proposed transformations were obtained with the Calibration_Michel algorithm and may differ for the same parameter of different models (e.g. X5 in GR5J and GR6J).
}

\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}


\examples{
library(airGR)

## --- generic function

## transformation Raw -> Transformed for the GR4J model
Xraw  <- matrix(c(+221.41, -3.63,  +30.00, +1.37,
                  +347.23, -1.03,  +60.34, +1.76,
                  +854.06, -0.10, +148.41, +2.34),
                  ncol = 4, byrow = TRUE)
Xtran <- TransfoParam(ParamIn = Xraw, Direction = "RT", FUN_TRANSFO = TransfoParam_GR4J)

## transformation Transformed -> Raw for the GR4J model
Xtran <- matrix(c(+3.60, -2.00, +3.40, -9.10,
                  +3.90, -0.90, +4.10, -8.70,
                  +4.50, -0.10, +5.00, -8.10),
                  ncol = 4, byrow = TRUE)
Xraw  <- TransfoParam(ParamIn = Xtran, Direction = "TR", FUN_TRANSFO = TransfoParam_GR4J)


## --- specific function

## transformation Raw -> Transformed for the GR4J model
Xraw  <- matrix(c(+221.41, -3.63,  +30.00, +1.37,
                  +347.23, -1.03,  +60.34, +1.76,
                  +854.06, -0.10, +148.41, +2.34),
                  ncol = 4, byrow = TRUE)
Xtran <- TransfoParam_GR4J(ParamIn = Xraw , Direction = "RT")

## transformation Transformed -> Raw for the GR4J model
Xtran <- matrix(c(+3.60, -2.00, +3.40, -9.10,
                  +3.90, -0.90, +4.10, -8.70,
                  +4.50, -0.10, +5.00, -8.10),
                  ncol = 4, byrow = TRUE)
Xraw  <- TransfoParam_GR4J(ParamIn = Xtran, Direction = "TR")
}


\author{
Laurent Coron, Olivier Delaigue
}

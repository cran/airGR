\encoding{UTF-8}


\name{TransfoParam_GR2M}
\alias{TransfoParam_GR2M}


\title{Transformation of the parameters of the GR2M model}


\usage{
TransfoParam_GR2M(ParamIn, Direction)
}


\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use \code{"RT"} for Raw -> Transformed and \code{"TR"} for Transformed -> Raw}
}
\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}


\description{
Function which transforms model parameters of the GR2M model (from real to transformed parameters and vice versa).
}


\author{
Laurent Coron
}


\encoding{UTF-8}
\name{TransfoParam_GR1A}
\alias{TransfoParam_GR1A}
\title{Transformation of the parameters of the GR1A model}
\usage{
TransfoParam_GR1A(ParamIn, Direction)
}
\arguments{
\item{ParamIn}{[numeric] matrix of parameter sets (sets in line, parameter values in column)}

\item{Direction}{[character] direction of the transformation: use "RT" for Raw->Transformed and "TR" for Transformed->Raw}
}
\value{
\emph{ParamOut} [numeric] matrix of parameter sets (sets in line, parameter values in column)
}
\description{
Function which transforms model parameters of the GR1A model (from real to transformed parameters and vice versa).
}
\author{
Laurent Coron (March 2015)
}


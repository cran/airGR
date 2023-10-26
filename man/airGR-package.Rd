\name{airGR-package}
\alias{airGR}
\alias{airGR-package}
\docType{package}
\encoding{UTF-8}
\title{\packageTitle{airGR}}


\description{
\packageDescription{airGR}
Each model core is coded in Fortran to ensure low computational time. The other package functions (i.e. mainly the calibration algorithm and the computation of the efficiency criteria) are coded in R. \cr\cr


## --- Functions and objects

The airGR package has been designed to fulfil two major requirements: facilitate the use by non-expert users and allow flexibility regarding the addition of external criteria, models or calibration algorithms. The names of the functions and their arguments were chosen to this end.

The package is mostly based on three families of functions:
\itemize{
  \item the functions belonging to the \code{\link{RunModel}} family require three arguments: \emph{InputsModel}, \emph{RunOptions} and \emph{Param};  please refer to help pages \code{\link{CreateInputsModel}} and \code{\link{CreateRunOptions}} for further details and examples;
  \item the functions belonging to the \code{\link{ErrorCrit}} family require two arguments: \emph{InputsCrit} and \emph{OutputsModel}; please refer to help pages \code{\link{CreateInputsCrit}} and \code{\link{RunModel}} for further details and examples;
  \item the functions belonging to the \code{\link{Calibration}} family require four arguments: \emph{InputsModel}, \emph{RunOptions}, \emph{InputsCrit} and \emph{CalibOptions};  please refer to help pages \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateInputsCrit}} and \code{\link{CreateCalibOptions}} for further details and examples.
}

In order to limit the risk of mis-use and increase the flexibility of these main functions, we imposed the structure of their arguments and defined their class. Most users will not need to worry about these imposed structures since functions are provided to prepare these arguments for them: \code{\link{CreateInputsModel}}, \code{\link{CreateRunOptions}}, \code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}. However, advanced users wishing to supplement the package with their own models will need to comply with these imposed structures and refer to the package source codes to get all the specification requirements. \cr\cr


## --- Models

Seven hydrological models and one snow melt and accumulation model are implemented in airGR. The hydrological models can be applied either on a lumped way or on a semi-distributed way (on sub-catchments). The snow model can either be used alone or with the daily or hourly hydrological models. Naturally each hydrological model can also be used alone. \cr
These models can be called within airGR using the following functions:
\itemize{
  \item \code{\link{RunModel_GR4H}}: four-parameter hourly lumped hydrological model (Mathevet, 2005)
  \item \code{\link{RunModel_GR5H}}: five-parameter hourly lumped hydrological model (Ficchi, 2017; Ficchi \emph{et al.}, 2019)
  \item \code{\link{RunModel_GR4J}}: four-parameter daily lumped hydrological model (Perrin \emph{et al.}, 2003)
  \item \code{\link{RunModel_GR5J}}: five-parameter daily lumped hydrological model (Le Moine, 2008)
  \item \code{\link{RunModel_GR6J}}: six-parameter daily lumped hydrological model (Pushpalatha \emph{et al.}, 2011)
  \item \code{\link{RunModel_GR2M}}: two-parameter monthly lumped hydrological model (Mouelhi, 2003; Mouelhi \emph{et al.}, 2006a)
  \item \code{\link{RunModel_GR1A}}: one-parameter yearly lumped hydrological model (Mouelhi, 2003; Mouelhi \emph{et al.}, 2006b)
  \item \code{\link{RunModel_CemaNeige}}: two-parameter degree-day snow melt and accumulation daily model (Valéry \emph{et al.}, 2014; Riboust \emph{et al.}, 2019)
  \item \code{\link{RunModel_CemaNeigeGR4H}}: combined use of GR4H and CemaNeige
  \item \code{\link{RunModel_CemaNeigeGR5H}}: combined use of GR5H and CemaNeige
  \item \code{\link{RunModel_CemaNeigeGR4J}}: combined use of GR4J and CemaNeige
  \item \code{\link{RunModel_CemaNeigeGR5J}}: combined use of GR5J and CemaNeige
  \item \code{\link{RunModel_CemaNeigeGR6J}}: combined use of GR6J and CemaNeige
}

## --- How to get started

To learn how to use the functions from the airGR package, it is recommended to follow the five steps described below:
\enumerate{
  \item refer to the help for \code{\link{RunModel_GR4J}} then run the provided example to assess how to make a simulation;
  \item refer to the help for \code{\link{CreateInputsModel}} to understand how the inputs of a model are prepared/organised;
  \item refer to the help for \code{\link{CreateRunOptions}} to understand how the run options of a model are parametrised/organised;
  \item refer to the help for \code{\link{ErrorCrit_NSE}} and \code{\link{CreateInputsCrit}} to understand how the computation of an error criterion is prepared/made;
  \item refer to the help for \code{\link{Calibration_Michel}}, run the provided example and then refer to the help for \code{\link{CreateCalibOptions}} to understand how a model calibration is prepared/made.
}

To get started with the package, you can refer to the 'get_started' vignette (\code{vignette("V01_get_started", package = "airGR")}). To know how to use the models on a semi-distributed way, you can refer to the 'sd_model' vignette (\code{vignette("V05_sd_model", package = "airGR")}). For more information, please visit the \href{https://hydrogr.github.io/airGR/}{airGR website}. \cr\cr


## --- References

\itemize{
  \item Ficchi, A. (2017). An adaptive hydrological model for multiple time-steps: Diagnostics and improvements based on fluxes consistency. PhD thesis, UPMC - Irstea Antony, Paris, France.
  \item Ficchi, A., Perrin, C. and Andréassian, V. (2019). Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching. Journal of Hydrology, 575, 1308-1327, \doi{10.1016/j.jhydrol.2019.05.084}.
  \item Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp.
  \item Mathevet, T. (2005). Quels modèles pluie-débit globaux pour le pas de temps horaire ? Développement empirique et comparaison de modèles sur un large échantillon de bassins versants, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 463 pp.
  \item Mouelhi, S. (2003). Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 323 pp.
  \item Mouelhi, S., Michel, C., Perrin, C. and Andréassian, V. (2006a). Stepwise development of a two-parameter monthly water balance model. Journal of Hydrology, 318(1-4), 200-214, \doi{10.1016/j.jhydrol.2005.06.014}.
  \item Mouelhi, S., Michel, C., Perrin, C. and Andréassian, V. (2006b). Linking stream flow to rainfall at the annual time step: the Manabe bucket model revisited. Journal of Hydrology, 328, 283-296, \doi{10.1016/j.jhydrol.2005.12.022}.
  \item Perrin, C., Michel, C. and Andréassian, V. (2003). Improvement of a parsimonious model for streamflow simulation. Journal of Hydrology, 279(1-4), 275-289, \doi{10.1016/S0022-1694(03)00225-7}.
  \item Pushpalatha, R., Perrin, C., Le Moine, N., Mathevet, T. and Andréassian, V. (2011). A downward structural sensitivity analysis of hydrological models to improve low-flow simulation. Journal of Hydrology, 411(1-2), 66-76, \doi{10.1016/j.jhydrol.2011.09.034}.
  \item Riboust, P., Thirel, G., Le Moine N. and Ribstein P. (2019). Revisiting a simple degree-day model for integrating satellite data: Implementation of SWE-SCA hystereses. Journal of Hydrology and Hydromechanics, 67(1), 70–81, \doi{10.1016/j.jhydrol.2014.04.058}.
  \item Valéry, A., Andréassian, V. and Perrin, C. (2014). "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments. Journal of Hydrology, 517(0), 1176-1187, \doi{10.1016/j.jhydrol.2014.04.058}.
}

}

\concept{hydrology}
\concept{model}
\concept{efficiency criterion}
\concept{calibration}
\concept{GR4J}

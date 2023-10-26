
# airGR: Suite of GR Hydrological Models for Precipitation-Runoff Modelling

## Overview

This package brings into R the hydrological modelling tools developed at INRAE-Antony ([Catchment Hydrology research group](https://webgr.inrae.fr/home/) of the HYCAR Research Unit, France), including rainfall-runoff models (**GR4H**, **GR5H**, **GR4J**, **GR5J**, **GR6J**, **GR2M**, **GR1A**) that can be applied either on a **lumped** or **semi-distributed** way. A snow accumulation and melt model (**CemaNeige**) and the associated functions for the calibration and evaluation of models are also included. Each model core is coded in Fortran to ensure low computational time. The other package functions (i.e. mainly the calibration algorithm and the computation of the efficiency criteria) are coded in R.


## Installation

``` r
install.packages("airGR")
```


## Functions and objects

The airGR package has been designed to fulfil two major requirements: facilitate the use by non-expert users and allow flexibility regarding the addition of external criteria, models or calibration algorithms. The names of the functions and their arguments were chosen to this end. 

The package is mostly based on three families of functions:

- the functions belonging to the `RunModel` family require three arguments: `InputsModel`, `RunOptions` and `Param`;  please refer to help pages `CreateInputsModel` and `CreateRunOptions` for further details and examples;
- the functions belonging to the `ErrorCrit` family require two arguments: `InputsCrit` and `OutputsModel`; please refer to help pages `CreateInputsCrit` and `RunModel` for further details and examples;
- the functions belonging to the `Calibration` family require four arguments: `InputsModel`, `RunOptions`, `InputsCrit` and `CalibOptions`;  please refer to help pages `CreateInputsModel`, `CreateRunOptions`, `CreateInputsCrit` and `CreateCalibOptions` for further details and examples.

In order to limit the risk of mis-use and increase the flexibility of these main functions, we imposed the structure of their arguments and defined their class. Most users will not need to worry about these imposed structures since functions are provided to prepare these arguments for them: `CreateInputsModel`, `CreateRunOptions`, `CreateInputsCrit`, `CreateCalibOptions`. However, advanced users wishing to supplement the package with their own models will need to comply with these imposed structures and refer to the package source codes to get all the specification requirements.


## Models

Seven hydrological models and one snow melt and accumulation model are implemented in airGR. The hydrological models can be applied either on a lumped way or on a semi-distributed way (on sub-catchments). The snow model can either be used alone or with the daily or hourly hydrological models. Naturally each hydrological model can also be used alone.
These models can be called within airGR using the following functions: 

  - `RunModel_GR4H`: four-parameter hourly lumped hydrological model (Mathevet, 2005)
  - `RunModel_GR5H`: five-parameter hourly lumped hydrological model (Ficchi, 2017; Ficchi *et al.*, 2019)
  - `RunModel_GR4J`: four-parameter daily lumped hydrological model (Perrin *et al.*, 2003)
  - `RunModel_GR5J`: five-parameter daily lumped hydrological model (Le Moine, 2008)
  - `RunModel_GR6J`: six-parameter daily lumped hydrological model (Pushpalatha *et al.*, 2011)
  - `RunModel_GR2M`: two-parameter monthly lumped hydrological model (Mouelhi, 2003; Mouelhi *et al.*, 2006a)
  - `RunModel_GR1A`: one-parameter yearly lumped hydrological model (Mouelhi, 2003; Mouelhi *et al.*, 2006b)
  - `RunModel_CemaNeige`: two-parameter degree-day snow melt and accumulation daily model (Valéry *et al.*, 2014; Riboust *et al.*, 2019)
  - `RunModel_CemaNeigeGR4H`: combined use of GR4H and CemaNeige
  - `RunModel_CemaNeigeGR5H`: combined use of GR5H and CemaNeige  
  - `RunModel_CemaNeigeGR4J`: combined use of GR4J and CemaNeige
  - `RunModel_CemaNeigeGR5J`: combined use of GR5J and CemaNeige
  - `RunModel_CemaNeigeGR6J`: combined use of GR6J and CemaNeige


## How to get started

To learn how to use the functions from the airGR package, it is recommended to follow the five steps described below:

  1. refer to the help for `RunModel_GR4J` then run the provided example to assess how to make a simulation;
  2. refer to the help for `CreateInputsModel` to understand how the inputs of a model are prepared/organised;
  3. refer to the help for `CreateRunOptions` to understand how the run options of a model are parametrised/organised;
  4. refer to the help for `ErrorCrit_NSE` and `CreateInputsCrit` to understand how the computation of an error criterion is prepared/made;
  5. refer to the help for `Calibration_Michel`, run the provided example and then refer to the help for `CreateCalibOptions` to understand how a model calibration is prepared/made.

To get started with the package, you can refer to the 'get_started' vignette (`vignette("V01_get_started", package = "airGR")`). To know how to use the models on a semi-distributed way, you can refer to the 'sd_model' vignette (`vignette("V05_sd_model", package = "airGR")`). For more information, please visit the [airGR website](https://hydrogr.github.io/airGR/).


## References

- Ficchi, A. (2017). An adaptive hydrological model for multiple time-steps: Diagnostics and improvements based on fluxes consistency. PhD thesis, UPMC - Irstea Antony, Paris, France.
- Ficchi, A., Perrin, C. and Andréassian, V. (2019). Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching. Journal of Hydrology, 575, 1308-1327, doi: [10.1016/j.jhydrol.2019.05.084](https://www.doi.org/10.1016/j.jhydrol.2019.05.084).
- Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp.
- Mathevet, T. (2005). Quels modèles pluie-débit globaux pour le pas de temps horaire ? Développement empirique et comparaison de modèles sur un large échantillon de bassins versants, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 463 pp.
- Mouelhi S. (2003). Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 323 pp.
- Mouelhi, S., Michel, C., Perrin, C. and Andréassian, V. (2006a). Stepwise development of a two-parameter monthly water balance model, Journal of Hydrology, 318(1-4), 200-214, doi: [10.1016/j.jhydrol.2005.06.014](https://www.doi.org/10.1016/j.jhydrol.2005.06.014).
- Mouelhi, S., Michel, C., Perrin, C. and Andréassian, V. (2006b). Linking stream flow to rainfall at the annual time step: the Manabe bucket model revisited, Journal of Hydrology, 328, 283-296, doi: [10.1016/j.jhydrol.2005.12.022](https://www.doi.org/10.1016/j.jhydrol.2005.12.022).
- Perrin, C., Michel, C. and Andréassian, V. (2003). Improvement of a parsimonious model for streamflow simulation, Journal of Hydrology, 279(1-4), 275-289, doi: [10.1016/S0022-1694(03)00225-7](https://www.doi.org/10.1016/S0022-1694(03)00225-7).
- Pushpalatha, R., Perrin, C., Le Moine, N., Mathevet, T. and Andréassian, V. (2011). A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, Journal of Hydrology, 411(1-2), 66-76, doi: [10.1016/j.jhydrol.2011.09.034](https://www.doi.org/10.1016/j.jhydrol.2011.09.034).
- Riboust, P., Thirel, G., Le Moine, N. and Ribstein, P. (2019). Revisiting a simple degree-day model for integrating satellite data: Implementation of SWE-SCA hystereses. Journal of Hydrology and Hydromechanics, 67(1), 70–81, doi: [10.2478/johh-2018-0004](https://www.doi.org/10.2478/johh-2018-0004).
- Valéry, A., Andréassian, V. and Perrin, C. (2014). "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments, Journal of Hydrology, 517(0), 1176-1187, doi: [10.1016/j.jhydrol.2014.04.058](https://www.doi.org/10.1016/j.jhydrol.2014.04.058).



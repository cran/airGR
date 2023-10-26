## Release History of the airGR Package



### 1.7.6 Release Notes (2023-10-25)

#### Bug fixes

- `CreateCalibOptions()` now uses parameter screening for `RunModel_Lag()` which are now expressed in the transformed space instead of the parameter space. ([#156](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/156))
- `RunModel_CemaNeige*()` now takes into account the case when `dG = 0`. ([#178](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/178))


#### Minor user-visible changes

- `Calibration_Michel()` now runs faster as the `ProposeCandidatesGrid()` was improved to create the propose candidates grid. ([#157](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/157)) 
- `TransfoParamGR5J()` now returns the correct  error message when the number of parameters is incorrect. ([#168](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/168)) 


#### CRAN-compatibility updates

- `frun_*` Fortran subroutine does not use anymore the 'DLLEXPORT' command. ([#180](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/180))
- The 'Rmalschains' package is back on CRAN and it is again suggested (cf. the 'param_optim' vignette). ([#175](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/175))
- The 'hydroPSO' package is no longer suggested (but the code linked to its use and is always present in the 'param_optim' vignette). ([#182](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/182))

____________________________________________________________________________________


### 1.7.4 Release Notes (2023-04-11)

#### CRAN-compatibility updates

- The 'Rmalschains' package is no longer suggested (but the code linked to its use and is always present in the 'param_optim' vignette). ([#172](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/172))
- `.ErrorCrit()` and `.FeatModels` function are no more exported. ([#173](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/173))

____________________________________________________________________________________


### 1.7.0 Release Notes (2022-02-21)

#### New features

- Semi-distributed modelling mode can now use the regularisation calibration proposed by [Lavenne et al. (2019)](https://doi.org/10.1029/2018WR024266). Added the `CreateInputsCrit_Lavenne()` to define a composite criterion based on the formula. Added the `CreateErrorCrit_GAPX()` function to compute an error criterion based on the GAPX formula. ([#111](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/111))
- `OutputsModel`, returned by the `RunModel_*GR*()` function, gains a `RunOptions` element which is a list and contains 2 sub-elements: `WarmUpQsim` (vector series of simulated discharge on the warm-up period) and `Param` (vector of the model parameter values). ([#123](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/123))
- `plot.OutputsModel()` gains a `AxisTS` argument in order to manage x-axis representing calendar dates and times. It avoids to display ugly x-axis. ([#122](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/122))


#### Deprecated and defunct

- The deprecated `LatRad` argument has been removed from the `PEdaily_Oudin()` function. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/81))
- The deprecated `Qobs` argument has been removed from the `CreateInputsCrit()` function. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/81))
- The deprecated `Ind_zeroes` argument has been removed from the `CreateInputsCrit()` function. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/81))
- The deprecated `verbose` argument has been removed from the `CreateInputsCrit()` function. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/81))
- The deprecated `FUN_CRIT` argument has been removed from the `ErrorCrit()` function. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/81))


#### Bug fixes

- `SeriesAggreg()` now correctly reorders regime time series when the monthly regime is computed from a time series that does not start in January. It also keeps original `data.frame` column names. ([#133](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/133))
- `DataAltiExtrapolation_Valery()` now correctly extract HypsoData values for each elevation layers. The selected indices were wrong (one less than expected) ([#144](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/144))
- `CreateIniStates()` does not return anymore an error message when `IntStore` is set and `RunModel_GR5H` is used. ([#144](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/145))


#### Major user-visible changes

- `RunModel_Lag()` now handles warm-up period simulation (set in `CreateRunOptions()`). ([#132](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/132))


#### Minor user-visible changes

- `PE_Oudin()` can use inconsistent time series. It allows to mixing time series from different stations. ([#134](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/134))
- Added the use of `.GetFeatModel()` in `CreateCalibOptions()` and `CreateIniStates()` functions in order to simplified their codes. ([#111](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/111))
- Added the `.FunTransfo` in order to manage the parameter transformations and to simplified the code of the `CreateCalibOptions()` function ([#111](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/111))
- Added the `.ArgumentsCheckGR()` function in order to check the arguments of the` RunModel_*()` functions and simplified their codes. ([#129](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/129))
- Added the `.GetOutputsModelGR()` function in order to manage the outputs of the` RunModel_*()` functions and simplified their codes. ([#129](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/129))
- The code of the `plot.OutputsModel()` function has been slightly simplified. ([#122](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/122), [#147](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/147))


#### Version control and issue tracking

- Added tests to check that the parameter sets returned by calibration algorithm do not change for any of the models. ([#120](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/120))
- Added tests to detect Decreased performance of calibration execution time. ([#136](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/136))
- Fixed the reverse package dependencies checked by the CI pipelines. ([#146](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/146))

____________________________________________________________________________________


### 1.6.12 Release Notes (2021-04-27)

#### New features

- `CreateInputsModel()` gains a `QupstrUnit` argument in order to manage the unit of the flow in the `Qupstream` argument in case of the use of a semi-distributed version of a hydrological model. ([#110](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/110))
- `RunModel_Lag()` gains a `QcontribDown` argument containing the time series of the runoff contribution of the downstream sub-basin in case of the use of a semi-distributed version of a hydrological model. ([#109](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/109))


#### Bug fixes

- Fixed bug in `RunModel`. The `RunModel_Lag()` can now be passed to the `FUN_MOD` argument. ([#108](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/108))
- Fixed bug in `RunModel_Lag()`. The function no longer returns two values for a single time step run. ([#102](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/102))
- Fixed bug in `RunModel_Lag()`. The `StateEnd` value is now correct when there are more than a single upstream basin. ([#103](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/103))
- Fixed bug in `RunModel_Lag()`. The `StateEnd` value is now correct when the upstream flow unit is mm/time step. ([#104](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/104))
- Fixed bug in `RunModel_CemaNeigeGR5H()`. The solid precipitation are now taken into account in the GR5H model. ([#105](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/105))
- Fixed bug in `RunModel_CemaNeige()` and `CreateInputsModel()`. `RunModel_CemaNeige()` now runs at the hourly time step. ([#106](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/106))
- Fixed the 'param_optim' vignette. The starting points used for the multi-start approach are now in the transformed space.([#101](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/101))


#### Major user-visible changes

- `LengthHydro` must now be set in kilometers (not anymore in meters) in the `CreateInputsModel()` function. ([#112](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/112))
- `TransfoParam_GR5H()` now use the same transformation as `TransfoParam_GR4H()` for the X1 parameter. The previous transformation set by Ficchì seems unnecessary as it provokes irrealistically high X1 values. ([#50](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/50))


#### Minor user-visible changes

- The `RunModel*()` functions now run faster. The computation times are significantly shorter for long times series with many time steps (e.g. hourly times series), due to a better management of the missing values in and out the Fortran codes. Only simulation computation times have been improved (it is largely invisible to the user for calibration computation times). ([#113](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/113))
- The external calibration algorithms used in 'param_optim' and 'param_mcmc' vignettes now run faster. The `RunModel_*()` functions used during the parameter estimation process now run faster because the outputs contain only the simulated flows (see the `Outputs_Sim` argument in the `CreateRunOptions()` help page).
- Added `.FeatModels()` and `.GetFeatModel()` functions in order to repectively store and get model features (e.g. name, number of parameters, time unit). Therefore the codes of the `CreateInputsModel()` and the `CreateRunOptions()` functions have been simplified. ([#106](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/106))


#### Version control and issue tracking

- The CI pipelines now fail when the checks return a warning message (and not just an error message). ([#86](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/86))
- The reverse dependencies packages (e.g. the 'airGRteaching' or the 'airGRdatassim') are now checked by the CI pipelines. ([#86](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/86))

____________________________________________________________________________________


### 1.6.10.4 Release Notes (2021-01-29)

#### New features

- Added a section 'param_optim' vignette to explain how to manage with multiobjective optimization using the 'caRamel' package. ([#61](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/61))


#### Major user-visible changes

- `Imax()` now returns an error message when `IndPeriod_Run` doesn't select 24 hours by day, instead of `numeric(0)`. ([#92](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/92))


#### Minor user-visible changes

- Fixed warning returned by GCC Fortran when compiling `frun_GR5H.f90`. ([#93](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/93))


#### CRAN-compatibility updates

- Coerce `character` dates into `POSIXlt` in `RunModel_GR1A()` example and in `SeriesAggreg()` tests in order to avoid bad subsetting on time series due to mixing UTC and local time (error returned on macOS flavors). ([#94](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/94))

____________________________________________________________________________________


### 1.6.9.27 Release Notes (2021-01-18)

#### New features

- Added `SeriesAggreg` S3 method with functions for `InputsModel`, `OutputsModel`, `list`, `data.frame` class objects. This new version of the `SeriesAggreg()` function also allows to compute regimes. ([#25](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/25))
- Added `.GetAggregConvertFun()` private function in order to choose automatically the `ConvertFun` to apply on each element of objects used in `SeriesAggreg.InputsModel()` and `SeriesAggreg.OutputsModel()`.
- Added `.AggregConvertFunTable` data.frame that allows the user to see what names of list items or data.frame column names are guessed and eventually customise this correspondence table.
- `PE_Oudin()` now presents a `RunFortran` argument to run the code in Fortran or in R. The Fortran mode is the fastest. ([#62](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/62))
- Added `RunModel_Lag()` which allows to perform a single run for the Lag model over the test period in order to run semi-distributed GR models. ([#34](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/34))
- Added the 'sd_model' vignette to explain how to manage the use of semi-distributed GR models. ([#34](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/34))
- Added `[` S3 method for `InputsModel` class object in order to extract subsets of it. ([#67](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/67))


#### Deprecated and defunct

- The `TimeFormat` argument is now deprecated in `SeriesAggreg()`. ([#41](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/41))
- The `NewTimeFormat` argument is now deprecated in `SeriesAggreg()` and replaced by the `Format` argument. ([#41](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/41))
- The deprecated `RunSnowModule` argument has been removed from the `CreateRunOptions()` function. ([#23](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/23))


#### Bug fixes

- Fixed bug in`SeriesAggreg()`. The function now runs when `TimeLag >= 3600`.
([#41](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/41))
- Fixed bug in`SeriesAggreg()`. The function now runs when the time series contain some columns entirely filled with missing values. ([#43](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/43))
- Fixed bug in `RunModel_GR1A()`. Reversed PotEvap and Precip outputs are now reordered (in the previous versions PotEvap contained the precipitation values and Precip contained the evapotranspiration values, the Qsim values were already correct). ([#65](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/65))


#### Major user-visible changes

- Added output to `RunModel_GR2M()` function (Ps). ([#51](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/51))
- `PE_Oudin()` can now run for several locations (i.e. several latitudes) in the Fortran mode (`RunFortran = TRUE`). In this case `Lat` must be of the same length as `Temp`. ([#62](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/62))
- `RunModel()` now allows to run semi-distributed GR models. ([#34](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/34))
- The `ConvertFun` argument of the `SeriesAggreg()` function can now be set to names of aggregation functions that return value of length 1 (not only `"sum"` or `"mean"`, but e.g. `"min"`, `"max"`, `"Q95"`). ([#82](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/82))


#### Minor user-visible changes

- The `.FortranOutputs()` function is no longer exported in the namespace.
- `RunModel_GR1A()` now uses the Fortran version of the model code. This code is no longer duplicated: the R version which was used was removed. ([#65](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/65))
- Character argument verification now use partial matching in `PE_Oudin()` and `SeriesAggreg()` functions. ([#37](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/37))
- `RunModel_*()` funcions were cleaned up, with no effect on their outputs. ([#14](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/14))
- `.ErrorCrit()` function now returns a warning message when a criterion computed on less than 10 time-steps (whatever the unit of the time step). ([#14](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/14))
- Added the diagram of GR5H in the `RunModel_GR5H()` documentation. ([#49](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/49))
- The `Exch` was renames `AExch` in the `RunModel_GR2M()` output. ([#87](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/87))
- Added 'Es' and 'Ps' on the GR2M diagram available in the `RunModel_GR2M()` help page. ([#88](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/88))
- The `plot.OutputsModel()` function does not check anymore the time step by comparing the calculation of the difference of the last two time steps because it is already checked by the class of the `OutputsModel` object, which is therefore assumed to be necessarily valid. ([#56](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/56))


#### Version control and issue tracking

- Implement automatic tests in the package. ([#52](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/52))


#### CRAN-compatibility updates

- 'airGR' now depends on R >= 3.1.0 because of the use of the `anyNA` function.
- The 'hydroPSO' package is back on CRAN and it is again suggested (cf. the 'param_optim' vignette). ([#38](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/38))
- For more safety, the following "basic" packages are now imported : 'graphics', 'grDevices', 'stats', 'utils. ([#74](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/74))

____________________________________________________________________________________


### 1.4.3.65 Release Notes (2020-02-28)

#### CRAN-compatibility updates

- The run period is reduced in the example of the `Imax()` function in order to run faster.
- The 'hydroPSO' package is no longer suggested (but the code linked to its use and is always present in the 'param_optim' vignette).

____________________________________________________________________________________


### 1.4.3.60 Release Notes (2020-01-29)

#### New features

- A digital object identifier (DOI) now allows to identify the manual of the 'airGR' package. When you use 'airGR' in your work, please always cite both the article and the manual. The last one allows to know the version of the package that is used in order to enhance reproducible research. The references can be displayed with the `citation("airGR")` command.

#### Bug fixes

- Fixed bug in `Imax()`. The default value of the `TestedValues` argument was wrong due to a mistyped argument name in the `seq()` function.

____________________________________________________________________________________


### 1.4.3.52 Release Notes (2020-01-21)


#### New features

- `plot.Outputsmodel()` now allows to draw actual evapotranspiration when `which = "ActEvap"` or `which = "All"` (overlaid to potential evapotranspiration if already drawn). ([#2](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/2))
- Added `RunModel_GR5H()` and `RunModel_CemaNeigeGR5H()` functions to run the hourly model GR5H (with or without the CemaNeige module). These models present an optional additionnal interception store. ([#13](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/13))
- Added `Imax()` which allows to estimate the maximum capacity of the GR5H interception store. ([#13](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/13))


#### Bug fixes

- Fixed bug in `TransfoParam_GR1A()`. The number of model parameters was wrong (2 instead of 1) which caused an error during the GR1A model calibration. ([#1](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/1))
- Fixed bug in `plot.OutputsModel()`. The function does not return any error message when `log_scale = TRUE`, `Qobs = NULL` and user want to draw flows time series.
- Fixed bug in `RunModel_*GR*()`. The functions do not return any error message anymore due to slightly negative values returned by GR4H, GR4J, GR5J or GR6J Fortran codes (the message was returned by `CreateIniStates()` when the final states were created). The `RunModel_*GR*()` functions now return zero instead of these slightly negative values, except for the ExpStore where negatives values are allowed.
- Fixed bug in the `.ErrorCrit()` function. The Box-Cox transformation formula is now corrected when the `ErrorCrit*()` functions are used.


#### Major user-visible changes

- Added outputs to `RunModel_GR4H()` function (Pn, Ps, AExch1, AExch2).


#### Minor user-visible changes

- Added the diagram of GR2M in the `RunModel_GR2M()` documentation.
- Fortran codes cleaned and translated from F77 to F90. ([#18](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/18))


#### CRAN-compatibility updates

- Cleaning of the Fortran codes (comment formatting).

____________________________________________________________________________________


### 1.3.2.42 Release Notes (2019-09-20)


#### Version control and issue tracking

- Users can now track changes (`https://gitlab.irstea.fr/HYCAR-Hydro/airgr`) and issues (`https://gitlab.irstea.fr/HYCAR-Hydro/airgr/issues`).


#### Bug fixes

- Fixed bug in `RunModel_CemaNeige()`. The function now runs correctly when `IndPeriod_WarmUp = 0L` in `CreateRunOptions()` in order to completely disable the warm-up period (e.g. to perform a forecast form a given initial state).
- Fixed bug in `CreateIniStates()`. The function now returns the right number of end states when CemaNeige is used without hysteresis.
- Fixed bug in the `RunModel_CemaNeige*()` functions. G and Gthr end states are no more inverted in the output values.


#### Minor user-visible changes

- Spurious flows set to `NA` into the `BasinObs` time series of the `L0123001` dataset.

____________________________________________________________________________________


### 1.3.2.23 Release Notes (2019-06-20)


#### New features

- `CreateInputsCrit()` now allows power (as a numeric or as a character) and the Box-Cox transformations in the `transfo` argument.
- Added `RunModel_CemaNeigeGR4H()` function to run the hourly model GR4H with the CemaNeige module.
- Added `PE_Oudin()` function to compute Oudin's potential evapotranspiration for hourly or daily time steps.
- `plot.OutputsModel()` now presents a `LayoutMat` argument (and additionnal related argument: `LayoutWidths`, `LayoutHeights`) to specify complex plot arrangements.


#### Deprecated and defunct

- The `PEdaily_Oudin()` function is deprecated and his use has been replaced by the use of `PE_Oudin()`.


#### Bug fixes

- Fixed bug in `plot.OutputsModel()`. The function now runs correctly when the `which` argument contains the `"CorQQ"` value without `"CumFreq"`.


#### Major user-visible changes

- `plot.OutputsModel()` can now draw PE or error time series if the `which` argument is set to `"all"` or `"PotEvap"` or `"Error"`.
- `plot.OutputsModel()` now allows new values for the which argument: `"all"` corresponds to all graphs, `"synth"` corresponds to the main graphs (default value; corresponding to `"all"` in the previous versions of the package) (i.e. `c("Precip", "Temp", "SnowPack", "Flows", "Regime", "CumFreq", "CorQQ")`), `"ts"` corresponds to the time series graphs (i.e. `c("Precip", "PotEvap", "Temp", "SnowPack", "Flows")`) and "perf" corresponds to the performance graphs (i.e. `c("Error", "Regime", "CumFreq", "CorQQ")`).


#### Minor user-visible changes

- `.ErrorCrit()` private function added to check inputs into `ErrorCrit_*()` functions. The `ErrorCrit_*()` functions were simplified accordingly.
- `CreateInputsCrit()` now returns `FUN_CRIT` as a character string.
- An example is addeed to illustred the use of the `plot.OutputsModel()` function.

____________________________________________________________________________________


### 1.2.13.16 Release Notes (2019-04-03)


#### New features

- `CreateInputsCrit()` now presents a `VarObs` argument in order to allow to prepare an `InputsCrit` object in order to run a criterion on other variables than observed discharges with the `ErrorCrit()` function (at the moment SCA and SWE).
- `CreateInputsCrit()` can now prepare an `InputsCrit` object in order to compute a single criterion (`Single` class), multiple criteria (`Multi` class) or a composite criterion (`Compo` class) with the `ErrorCrit()` function.
- `CreateInputsCrit()` now presents a `Weights` argument in order to allow to prepare an `InputsCrit` object in order to compute a composite criterion (`Compo` class) with `ErrorCrit()` or `Calibration_Michel()`.
- `CreateInputsCrit()` now returns a `idLayer` element to indicate which layer(s) to use for SCA or SWE aggregation.
- `CreateInputsCrit()` now presents a `warnings` argument to replace the verbose action (the `verbose` argument is kept to print messages).
- In `CreateInputsCrit()`, it is now possible to set the following arguments as atomic (as before) or as list: `FUN_CRIT`, `VarObs`, `Obs`, `BoolCrit`, `transfo`, `Weights`. If the list format is chosen, all the lists must have the same length.
- `CreateRunOptions()`, `CreateIniStates()` and `CreateCalibOptions()` now present a `IsHyst` argument to give the possibility to use the Linear Hysteresis with CemaNeige. The objects returned present an `hysteresis` class.
- `CreateRunOptions()` now presents a `warnings` argument to replace the verbose action (the `verbose` argument is kept to print messages).
- Added `TransfoParam_CemaNeigeHyst()` function in order to take into account transformation of the parameters of the CemaNeige module when the Linear Hysteresis is used.
- Added the `X0310010` dataset to run the examples using the Linear Hysteresis with CemaNeige (it contains necessary SCA data).
- Added the 'cemaneige_hysteresis' vignette to explain how to manage the use of the Linear Hysteresis with CemaNeige.


#### Deprecated and defunct

- The `Qobs` argument is now deprecated in `CreateInputsCrit()` and has been renamed `Obs`.
- The `FUN_CRIT` argument is now deprecated in `ErrorCrit()`. This function now gets this information from the `InputsCrit` argument.
- The `FUN_CRIT` argument is now deprecated in `Calibration_Michel()`. This function now gets this information from the `InputsCrit` argument.
- The `plot_OutputsModel()` had been deprecated in 'airGR' 1.0.4 (it had been replaced by the use of `plot.OutputsModel()` or `plot()`) and is defunct now.


#### Major user-visible changes

- `CreateInputsCrit()` now return a list of `InputsCrit` (each element is of the `Single` class) in the cases of multiple or a composite criteria.
- `ErrorCrit_*()` functions now return an error message if the `InputsCrit` object is of class `Multi` or `Compo`.
- `ErrorCrit()` function can now run on a multiple or a composite `InputsCrit`. In these cases, it returns a list of `ErrorCrit`.
- `ErrorCrit()` and `ErrorCrit_*()` functions can now assess Q, SCA or SWE simulations.
- `Calibration_Michel()` function can now run on a composite `InputsCrit`. It returns a composite value of error and the formula used to calculate it.
- Model diagrams added in documentations of `RunModel_GR4J()`, `RunModel_GR5J()` and `RunModel_GR6J()` functions.
- It is now possible to be redirected to the `plot.OutputsModel()` documentation with `?plot`.
- It is now possible to use a character vector for all `FUN_*` arguments (in addition to function objects) in the following functions: `Calibration()`, `Calibration_Michel()`, `CreateCalibOptions()`, `CreateIniStates()`, `CreateIniStates()`, `CreateInputsCrit()`, `CreateInputsModel()`, `CreateRunOptions()`, `ErrorCrit()`, `RunModel()` and `TransfoParam()`.


#### Minor user-visible changes

- `ErrorCrit_*()` functions now return objects of class `ErrorCrit` and `NSE`, `KGE`, `KGE2` or `RMSE`.
- `.FortranOutputs()` private function added to manage Fortran outputs.
- Outputs of `frun_GR2M` Fortran subroutine were reordered.
- `DataAltiExtrapolation_Valery()` now returns named elements of lists relative to elevation layer.
- `Calibration()` function now returns an error message if `FUN_CALIB` is not a function.
- Inputs of `PEdaily_Oudin()` are now checked.
- `PEdaily_Oudin()` example corrected (the Julian day was one day too early).
- `plot.OutputsModel()` does not return a warning message anymore when `Qobs = NULL`.
- Inputs of `TransfoParam*()` functions are now checked.
- The order of authors has been updated in the DESCRIPTION and the CITATION files.


#### CRAN-compatibility updates

- Tabulations removed, unused variables removed and variable statements fixed in Fortran files.

____________________________________________________________________________________


### 1.0.15.2 Release Notes (2018-10-10)


#### Bug fixes

- Fixed bug in `CreateRunOptions()`. The function now accounts correctly for leap years when no warm-up period is defined.


#### Minor user-visible changes

- `CreateRunOptions()` was cleaned up, with no effect on its outputs.


#### CRAN-compatibility updates

- The `vignetteParam*.rda` datasets moved to the inst directory. It contains different objects needed for 'param_optim' and 'param_mcmc' vignettes.

____________________________________________________________________________________


### 1.0.14.1 Release Notes (2018-09-28)


#### New features

- `PEdaily_Oudin()` now presents a `LatUnit` argument which allows to choose the unit of the latitude (radians and degrees).


#### Deprecated and defunct

- The `LatRad` argument is now deprecated in `PEdaily_Oudin()` and replaced by the `Lat` argument.
- The unused `Ind_zeroes` argument of the `CreateInputsCrit()` function is now deprecated.
- The `verbose` argument is now deprecated in `CreateInputsCrit()` and replaced by the `warnings` argument.


#### Major user-visible changes

- `Calibration_Michel()` is now faster during the grid-screening step when a parameter is set using `FixedParam` in `CreateCalibOptions()`.
- `CreateCalibOptions()` now returns an error when all the parameters are set in the `FixedParam` argument and a warning message when all the parameters are free (NA) in the `FixedParam` argument.
- `CreateInputsCrit()` now returns an error when `epsilon` is not positive.
- `CreateInputsCrit()` now returns a warning message in the following case: there are zeroes values in `Qobs`, `epsilon = NULL` and `transfo = log` or `inv`.
- `ErrorCrit_*()` functions now return a warning message in the following case: there are zeroes values in `Qobs` or `Qsim`, `epsilon = NULL` and `transfo = log` or `inv`.


#### Minor user-visible changes

- Several functions of the package were cleaned up or slightly modified, with no effect on their outputs.
- Dubious Qls and Qmm values set to NA values between 1997-01-05 and 1997-01-21 in the `L0123001` dataset.
- ORCID numbers are now joined to the names of the authors of the package.


#### CRAN-compatibility updates

- Function name changed in a vignettes to avoid error during the check on Debian distribution
- As recomanded by CRAN managers, the NEWS file is now at the text format and is no more just a link to the 'airGR' Website
- Added the `Vignette_Param.` datasets in order to reduce runtime during the re-building of vignettes. It contains different objects needed for param_optim and param_mcmc vignettes.

____________________________________________________________________________________


### 1.0.10.11 Release Notes (2018-06-29)


#### Bug fixes

- Fixed bug in `RunModel_GR2M()`. The function now returns the total precipitation (P) instead of the net rainfall (P1).


#### Major user-visible changes

- `RunModel_GR2M()` now returns more explicit precipitation outputs names.
- `CreateInputsCrit()` now returns a warning message when the KGE (or KGE') is used with a log transformation on flows.
- The article reference is corrected.


#### Minor user-visible changes

- The documentation and help of several functions were improved.

____________________________________________________________________________________


### 1.0.9.64 Release Notes (2017-11-10)


#### New features

- An article describing the 'airGR' package has been published. Its reference has been added and will be displayed with `citation("airGR")`.
- Added `CreateIniStates()` function in order to help user to format the `IniStates` argument for `CreateRunOptions()`.
- Added the `Param_Sets_GR4J` dataset. It contains generalist parameter sets for the GR4J model.
- Three vignettes have been added. They are relative to different calibration methods (including the generalist parameters sets of the GR4J model).


#### Deprecated and defunct

- The `RunSnowModule` argument is now deprecated in `CreateRunOptions()`.


#### Bug fixes

- Fixed bug in `RunModel_GR4H()`: in `frun_GR4H` Fortran subroutine, `St(2)` is now set to 0 (instead of `St(1)`) when `St(2) < 0`.
- Fixed bug in `plot.OutputsModel()` for the regime plot when the period is less than 1 year.
- Fixed bug in `plot.OutputsModel()` when there is no common data to plot the cumulative frequency or the correlation QQ.
- Fixed bug in `plot.OutputsModel()` for the y-axis labelling of flows time series when `log_scale = TRUE` and `BasinArea` is used.


#### Major user-visible changes

- `RunModel_GR4J()`, `RunModel_GR5J()` and `RunModel_GR6J()` (and `CemaNeige_GR*J()`) now return Ps, Pn and actual exchanges. See the model Fortran codes for more details about the calculation of these variables.
- `CreateInputsModel()` now returns an error when `DatesR` contains duplicated values.
- `RunModel_GR5J` now returns `StateEnd` in the same order as the other models.


#### Minor user-visible changes

- `plot.OutputsModel()` now returns a warning message when the length of Qobs is different from the length of Qsim.
- The X1 parameter from GR4H, GR4J, GR2M, GR5J and GR6J, the X3 parameter from GR4H, GR4J, GR5J and GR6J and the X6 parameter from GR6J are now set to 1e-2 when they are fixed to lower values. `RunModel_*()` functions now return a warning message in this case. `RunModel_*()` functions now return a warning when X4 < 0.5 and its value is set to 0.5.
- The commands `?L0123001`, `?L0123002` and `?L0123003` now return the documentation page related to `BasinObs`.
- Many functions of the package were cleaned up or slightly modified, with no effect on their outputs.
- The documentation and help of several functions were improved.


#### CRAN-compatibility updates

- "airGR.c" file registers native routines.

____________________________________________________________________________________


### 1.0.5.12 Release Notes (2017-01-23)


#### New features

- `DataAltiExtrapolation_Valery()` and `CreateInputsModel()` now present a `PrecipScale` argument which allows rescaling precipitation when it is interpolated on the elevation layers when CemaNeige is used.


#### Bug fixes

- Fixed bug in `DataAltiExtrapolation_Valery()`. The elevation gradients for air temperature returned by `CreateInputsModel()` are improved.


#### User-visible changes

- `DataAltiExtrapolation_Valery()` has been improved. `DataAltiExtrapolation_Valery()` now runs faster (and by consequence `CreateInputsModel()` too, when CemaNeige is used).

____________________________________________________________________________________


### 1.0.4 Release Notes (2017-01-18)


#### New features

- `RunModel_CemaNeige()`, `RunModel_CemaNeigeGR4J()`, `RunModel_CemaNeigeGR5J()` and `RunModel_CemaNeigeGR6J()` now return air temperature for each elevation layer.


#### Deprecated and defunct

- S3 plot method defined for `OutputsModel` objects. It means that the `plot_OutputsModel()` function is deprecated and his use has been replaced by the use of `plot.OutputsModel()` or `plot()`.
- In `plot.OutputsModel()` the `PlotChoice` argument is deprecated and has been renamed `which`.


#### User-visible changes

- `plot.OutputsModel()` displays air temperature time series for each layer when `CemaNeige` is used (argument `which = "Temp"` or `"all"`).

____________________________________________________________________________________


### 1.0.3 Release Notes (2016-12-09)


#### New features

- `ErrorCrit_*()` functions gain a `warnings` argument to replace the verbose action and the `verbose` argument now prints the criterion value(s).


#### Bug fixes

- Fixed bug in `CreateCalibOptions()` when `StartParamList` or `StartParamDistrib` arguments are used.


#### User-visible changes

- `CreateInputsModel()` now returns an error if `NLayers <= 0` when `CemaNeige` is used.
- `plot_OutputsModel()` now displays raw values on the y-axis when the discharge time series is represented with log scale (formerly, log values of discharges were displayed on the y-axis).

____________________________________________________________________________________


### 1.0.2 Release Notes (2016-11-03)


#### New features

- `SeriesAggreg()` gains a `TimeLag` argument that corresponds to a numeric value indicating a time lag (in seconds) for the time series aggregation (useful to aggregate hourly time series to the daily time step for instance).
 In addition, the function now accepts input dates in both `POSIXt` formats (`POSIXct` and `POSIXlt`). The output is in `POSIXct` format.
- `plot_OutputsModel()` gains a `log_scale` argument in order to plot the flow with a log scale.
- A tutorial is available online on the following link: https://hydrogr.github.io/airGR/.
 It can also be displayed with the `vignette("airGR")` command.


#### Deprecated and defunct

- `CreateCalibOptions()` loses the `OptimParam` argument that was redundant with the `FixedParam` argument. The `Calibration_Michel()` was modified to take into account this change by using directly `FixedParam`, but this is transparent to the user.
- `CreateCalibOptions()` loses the `StartParam` argument that was not used.


#### Bug fixes

- The value `sort` for the `transfo` argument of `CreateInputsCrit()` was not taken into account. It is now fixed.


#### Major user-visible changes

- The `RunModel_GR6J()` and `RunModel_CemaNeigeGR6J()` models were modified back to versions previous to 1.0.1 to prevent from unwanted efficiency criteria deterioration related to the calibration with `Calibration_Michel()`.
 The actual model codes were not modified but the `TransfoParam_GR6J()` and `CreateCalibOptions()` functions were modified regarding the X5 parameter.
 It is strongly advised to use airGR 1.0.2 for the `RunModel_GR6J()` and `RunModel_CemaNeigeGR6J()` functions if you are using `Calibration_Michel()`, as they are much more efficient.
 In case you were using your own calibration algorithm, you will not notice any difference.


#### Minor user-visible changes

- `CreateInputsModel()` and `DataAltiExtrapolation_Valery()` functions now allow both `POSIXt` formats (`POSIXct` and `POSIXlt`).

____________________________________________________________________________________


### 1.0.1 Release Notes (2016-04-21)


#### Deprecated and defunct

- The `Calibration_HBAN()` and `DataAltiExtrapolation_HBAN()` functions have respectively been renamed as `Calibration_Michel()` and `DataAltiExtrapolation_Valery()` after the names of their creators.
- The `Calibration_optim()` function has been removed from the package.
- The silent mode is now defined by the `verbose = TRUE` argument (formerly `quiet = FALSE`) in the following functions:
`Calibration()`, `Calibration_Michel()`, `CreateInputsModel()`, `CreateRunOptions()`, `DataAltiExtrapolation_Valery()`, `ErrorCrit()`, `ErrorCrit_KGE()`, `ErrorCrit_KGE2()`, `ErrorCrit_NSE()`, `ErrorCrit_RMSE()`, `plot_OutputsModel()`, `SeriesAggreg()`.


#### Major user-visible changes
- The GR5J model has been modified: previously, two unit hydrographs were used, now only one is remaining.
 As a consequence, simulations from the GR5J (`RunModel_GR5J()` function) and CemaNeige (`RunModel_CemaNeigeGR5J()` function) models will be different.
- An important proportion of the transformations of the parameters have been modified (`TransfoParam_*()` functions). Since this modifies the local search, calibration results will be different .
- The quantiles of the parameters have been recalculated with the new transformations (`CreateCalibOptions()` function). Since these quantiles constitute the starting point of the calibration algorithm, calibration results will be different.


#### Minor user-visible changes

- The Fortran model core codes have been modified:
	- optimisation of the codes for fastening of computation;
	- simplification of the internal variables for easier reading and understanding.
- The list of the contributors and authors is now full.
- The references of the package has been updated; they are returned by the following R-command `citation("airGR")`.

____________________________________________________________________________________


### 0.8.1.2 Release Notes (2015-08-21)


#### Bug fixes

- Fixed bug in `CreateInputsModel()` that was related to the handling of missing values.
- Fixed bug in `CreateRunOptions()` that prevented the correct use of the `IniResLevels` argument (to manually set the filling rate of the production and routing stores).


#### Minor user-visible changes

- Removal of an unnecessary warning when `IndPeriod_WarmUp = 0`.


#### CRAN-compatibility updates

- Modification of namespace file to ensure proper use under linux without compilation issues.

____________________________________________________________________________________


### 0.8.0.2 Release Notes (2015-04-15)


#### New features

- Three new hydrological models: `RunModel_GR4H() function for ` GR4H (hourly), `RunModel_GR2M()` function for GR2M (monthly) and `RunModel_GR1A()` function for GR1A (yearly).
- New function `SeriesAggreg()` to easily aggreg timesteps.


#### Bug fixes

- Fixed bug in `ErrorCrit_RMSE()` which led to incorrect calibration (the criterion was maximised instead of minimised).


#### Major user-visible changes

- Update of the functions `CreateRunOptions()`, `CreateCalibOptions()` and `plot_OutputsModel()` to handle the new models.
- Modification of CemaNeige Fortran code to add an update of Gratio after the SnowPack update (no impact on snow simulation).


#### Minor user-visible changes

- Improvement of the `plot_OutputsModel()` function to allow a selection among available plots.
- Minor update in `ErrorCrit_KGE()` and `ErrorCrit_KGE2()` to handle case when only one values in not NA.
- Update of the scripts in airGR-advanced-example to match the structures of the `BasinData` objects.
- Correction of formatting issue in airGR-advanced-example regarding the "List_HypsoData.txt" file.

____________________________________________________________________________________


### 0.7.4 Release Notes (2014-11-01)


#### New features

- New argument in many functions (`quiet = TRUE` or `FALSE`) to choose if the warnings should be suppressed or not.


#### Deprecated and defunct

- The `CalibrationAlgo_*()` functions were renamed into `Calibration_*()`.


#### Bug fixes

- Fixed bug in `CreateCalibOptions()` to handle models with only one parameter.
- Fixed bug in `Calibration_HBAN()`. The function was not working properly with models having only one parameter.


#### Major user-visible changes

- CemaNeige users must now specify one `MeanAnSolidPrecip` for each elevation layer. The `CreateRunOptions()` function is impacted.
- CemaNeige users can now specify the mean elevation of the input series (before it was always considered equal to the catchment median elevation).
 The impacted functions are `CreateInputsModel()` and `DataAltiExtrapolation_HBAN()`.
- New architecture with better format verification procedure (using classes) and simpler setting of default configuration.
- New architecture where the model, calibration and error functions are in the arguments of the functions
 (the exotic use of "generic function" created by the users has been removed).
- Improved documentation and examples.


#### Minor user-visible changes

- Improvements allowing the arrival of new models.
- Improvements of the argument verifications in `CreateInputsModel()`, `CreateRunOptions()`, `CreateInputsCrit()`, `CreateCalibOptions()`.
- Improvements of all the `ErrorCrit()` functions to better account for the cases with constant flow values or local zeros.
- Improvement of the `plot_OutputsModel` function (to handle 0 in Qobs and Qsim).
- Improved documentation.

____________________________________________________________________________________


### 0.6.2 Release Notes (2014-02-12)


#### New features

- Additional functions for results plotting (the 'zoo' package is required for some of them).
- Add multi-objective calibration using `nsga2()` (the 'mco' package is required).
- The field Multiplier has been added in the ErrorCrit() outputs, to indicate whether the criterion is an error (to minimise) or and efficiency (to maximise).
 This allows to provide real efficiency values in the outputs e.g. NSE[Q] instead of (-1) &times; NSE[Q].


#### Deprecated and defunct

- `EfficiencyCrit()` have been replaced by `ErrorCrit()` to avoid misunderstanding (by default, the algorithms minimise the error criterion).


#### Bug fixes

- RC11 bug correction: the automatic selection of the warm-up period was not working properly when no data was available from warm-up (i.e. when the user had set the run to start at the very first index).
- RC10 bug correction: the `CalibrationAlgo_HBAN()` function was not working in the very rare case when the diagonal search was activated and lead to a set outside the authorised range.
- RC9 bug correction: the `CalibrationAlgo_HBAN()` function was not working properly with models having only one parameter.
- RC8 bug correction of the `ModelDefaultIniOptions()` function (this bug was introduced in the RC7 and caused an error when `IndPeriod_WarmUp = NULL`.
- RC7 bug correction of the `ModelDefaultIniOptions()` function (the automatic selection of one year for warm-up was not handling properly missing data).
- RC6 correction of the help files (the description of CemaNeige parameters were inverted).
- RC5 differs from previous releases in the way the data are read and stored (in a list instead of individual vectors).
 The package is similar, only the examples of Main and the files in MyScriptBlocks have changed.
 All basin data are now stored inside a list named `BasinData`. This will greatly ease the future use of Rdata files (instead of txt files) as storage format for the time series of observation.


#### Major user-visible changes

- The definition of the generic function is now made in a much simpler way (e.g. see `DefineFunctions_Model()` or `DefineFunctions_ErrorCrit()`).


#### Minor user-visible changes

- Code improvements to reduce the computation time.
- Clearer instructions for the adding and modification of a model.
- Improvements of the documentation.


____________________________________________________________________________________


### 0.5.2 Release Notes (2014-02-05)


#### Deprecated and defunct

- The `SelectPer` arguments are replaced by `IndPeriod` to ease understanding.
- The `PE` arguments are replaced by `PotEvap()` to ease understanding.
- The `Fsol` arguments are replaced by `FracSolidPrecip` to ease understanding.


#### Major user-visible changes

- R <= 2.15 in not supported by default.
- The check that `SelectPer_Run()` is continuous is now made in the `CheckArg()` functions.
- Check of the model functioning time step.
- Name of the calibration criterion provided in `OutputsAlgo()`.


#### Minor user-visible changes

- Missing values in Fortran are now -999.999 instead of -9.999.


____________________________________________________________________________________


### 0.5.1 Release Notes (2014-01-27)


#### New features

- New `EfficiencyCrit_NSE_sqrtQ()` function to compute NSE criterion on sqrt flows.


#### Bug fixes

- Incorrect arguments in the call to `RunModelAndCrit` from `CalibrationAlgo_optim_stats` and `CalibrationAlgo_nlminb_stats`.
- `CalibrationAlgo_nlminb_stats` argument was wrongly defined in `DefineFunctions_CalibrationAlgo()` (`optim` instead of `nlminb`).
- Format checking for `RunOptions` was incorrectly made in `CheckArg()` function.

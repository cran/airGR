## Release History of the airGR Package



### 1.4.3.52 Release Notes (2020-01-20)


#### New features

- <code>plot.Outputsmodel()</code> now allows to draw actual evapotranspiration when <code>which = "ActuEvap"</code> or <code>which = "All"</code> (overlaid to potential evapotranspiration if already drawn).
- Added <code>RunModel_GR5H()</code> and <code>RunModel_CemaNeigeGR5H()</code> functions to run the hourly model GR5H (with or without the CemaNeige module). These models present an optional additionnal interception store.
- Added <code>Imax()</code> which allows to estimate the maximum capacity of the GR5H interception store.


#### Bug fixes

- Fixed bug in <code>TransfoParam_GR1A()</code>. The number of model parameters was wrong (2 instead of 1) which caused an error during the GR1A model calibration.
- Fixed bug in <code>plot.OutputsModel()</code>. The function does not return any error message when <code>log_scale = TRUE</code>, <code>Qobs = NULL</code> and user want to draw flows time series.
- Fixed bug in <code>RunModel_&#42;GR&#42;()</code>. The functions do not return any error message anymore due to slightly negative values returned by GR4H, GR4J, GR5J or GR6J Fortran codes (the message was returned by <code>CreateIniStates()</code> when the final states were created). The <code>RunModel_&#42;GR&#42;()</code> functions now return zero instead of these slightly negative values, except for the ExpStore where negatives values are allowed.
- Fixed bug in the <code>.ErrorCrit()</code> function. The Box-Cox transformation formula is now corrected when the <code>ErrorCrit&#42;()</code> functions are used.


#### Major user-visible changes

- Added outputs to <code>RunModel_GR4H()</code> function (Pn, Ps, AExch1, AExch2).


#### Minor user-visible changes

- Added the diagram of GR2M in the <code>RunModel_GR2M()</code> documentation.
- Fortran codes cleant and translated from F77 to F90.


#### CRAN-compatibility updates

- Cleaning of the Fortran codes (comment formatting).

____________________________________________________________________________________


### 1.3.2.42 Release Notes (2019-09-20)


#### Version control and issue tracking

- Users can now track changes (<code>https://gitlab.irstea.fr/HYCAR-Hydro/airgr</code>) and issues (<code>https://gitlab.irstea.fr/HYCAR-Hydro/airgr/issues</code>).


#### Bug fixes

- Fixed bug in <code>RunModel_CemaNeige()</code>. The function now runs correctly when <code>IndPeriod_WarmUp = 0L</code> in <code>CreateRunOptions()</code> in order to completely disable the warm-up period (e.g. to perform a forecast form a given initial state).
- Fixed bug in <code>CreateIniStates()</code>. The function now returns the right number of end states when CemaNeige is used whithout hysteresis.
- Fixed bug in the <code>RunModel_CemaNeige&#42;()</code> functions. G and Gthr end states are no more inverted in the output values.


#### Minor user-visible changes

- Spurious flows set to <code>NA</code> into the <code>BasinObs</code> time series of the <code>L0123001</code> dataset.

____________________________________________________________________________________


### 1.3.2.23 Release Notes (2019-06-20)


#### New features

- <code>CreateInputsCrit()</code> now allows power (as a numeric or as a character) and the Box-Cox transformations in the <code>transfo</code> argument.

- Added <code>RunModel_CemaNeigeGR4H()</code> function to run the hourly model GR4H with the CemaNeige module.

- Added <code>PE_Oudin()</code> function to compute Oudin's potential evapotranspiration for hourly or daily time steps.


#### Deprecated and defunct

- The <code>PEdaily_Oudin()</code> function is deprecated and his use has been replaced by the use of <code>PE_Oudin()</code>.

- <code>plot.OutputsModel()</code> now presents a <code>LayoutMat</code> argument (and additionnal related argument: <code>LayoutWidths</code>, <code>LayoutHeights</code>) to specify complex plot arrangements.


#### Bug fixes

- Fixed bug in <code>plot.OutputsModel()</code>. The function now runs correctly when the <code>which</code> argument contains the <code>"CorQQ"</code> value without <code>"CumFreq"</code>.


#### Major user-visible changes

- <code>plot.OutputsModel()</code> can now draw PE or error time series if the <code>which</code> argument is set to <code>"all"</code> or <code>"PotEvap"</code> or <code>"Error"</code>.

- <code>plot.OutputsModel()</code> now allows new values for the which argument: <code>"all"</code> corresponds to all graphs, <code>"synth"</code> corresponds to the main graphs (default value; corresponding to <code>"all"</code> in the previous versions of the package) (i.e. <code>c("Precip", "Temp", "SnowPack", "Flows", "Regime", "CumFreq", "CorQQ")</code>), <code>"ts"</code> corresponds to the time series graphs (i.e. <code>c("Precip", "PotEvap", "Temp", "SnowPack", "Flows")</code>) and "perf" corresponds to the performance graphs (i.e. <code>c("Error", "Regime", "CumFreq", "CorQQ")</code>).


#### Minor user-visible changes

- <code>.ErrorCrit()</code> private function added to check inputs into <code>ErrorCrit_&#42;()</code> functions. The <code>ErrorCrit_&#42;()</code> functions were simplified accordingly.

- <code>CreateInputsCrit()</code> now returns <code>FUN_CRIT</code> as a character string.

- An example is addeed to illustred the use of the <code>plot.OutputsModel()</code> function.

____________________________________________________________________________________


### 1.2.13.16 Release Notes (2019-04-03)


#### Deprecated and defunct

- The <code>Qobs</code> argument is now deprecated in <code>CreateInputsCrit()</code> and has been renamed <code>Obs</code>.

- The <code>FUN_CRIT</code> argument is now deprecated in <code>ErrorCrit()</code>. This function now gets this information from the <code>InputsCrit</code> argument.

- The <code>FUN_CRIT</code> argument is now deprecated in <code>Calibration_Michel()</code>. This function now gets this information from the <code>InputsCrit</code> argument.

- The <code>plot_OutputsModel()</code> had been deprecated in airGR 1.0.4 (it had been replaced by the use of <code>plot.OutputsModel()</code> or <code>plot()</code>) and is defunct now. 


#### New features

- <code>CreateInputsCrit()</code> now presents a <code>VarObs</code> argument in order to allow to prepare an <code>InputsCrit</code> object in order to run a criterion on other variables than observed discharges with the <code>ErrorCrit()</code> function (at the moment SCA and SWE).

- <code>CreateInputsCrit()</code> can now prepare an <code>InputsCrit</code> object in order to compute a single criterion (<code>Single</code> class), multiple criteria (<code>Multi</code> class) or a composite criterion (<code>Compo</code> class) with the <code>ErrorCrit()</code> function.

- <code>CreateInputsCrit()</code> now presents a <code>Weights</code> argument in order to allow to prepare an <code>InputsCrit</code> object in order to compute a composite criterion (<code>Compo</code> class) with <code>ErrorCrit()</code> or <code>Calibration_Michel()</code>.

- <code>CreateInputsCrit()</code> now returns a <code>idLayer</code> element to indicate which layer(s) to use for SCA or SWE aggregation.

- <code>CreateInputsCrit()</code> now presents a <code>warnings</code> argument to replace the verbose action (the <code>verbose</code> argument is kept to print messages).

- In <code>CreateInputsCrit()</code>, it is now possible to set the following arguments as atomic (as before) or as list: <code>FUN_CRIT</code>, <code>VarObs</code>, <code>Obs</code>, <code>BoolCrit</code>, <code>transfo</code>, <code>Weights</code>. If the list format is chosen, all the lists must have the same length.

- <code>CreateRunOptions()</code>, <code>CreateIniStates()</code> and <code>CreateCalibOptions()</code> now present a <code>IsHyst</code> argument to give the possibility to use the Linear Hysteresis with CemaNeige. The objects returned present an <code>hysteresis</code> class.

- <code>CreateRunOptions()</code> now presents a <code>warnings</code> argument to replace the verbose action (the <code>verbose</code> argument is kept to print messages).

- Added <code>TransfoParam_CemaNeigeHyst()</code> function in order to take into account transformation of the parameters of the CemaNeige module when the Linear Hysteresis is used.

- Added the <code>X0310010</code> dataset to run the examples using the Linear Hysteresis with CemaNeige (it contains necessary SCA data).

- Added the cemaneige_hysteresis vignette to explain how to manage the use of the Linear Hysteresis with CemaNeige.


#### Major user-visible changes

- <code>CreateInputsCrit()</code> now return a list of <code>InputsCrit</code> (each element is of the <code>Single</code> class) in the cases of multiple or a composite criteria.

- <code>ErrorCrit_&#42;()</code> functions now return an error message if the <code>InputsCrit</code> object is of class <code>Multi</code> or <code>Compo</code>.

- <code>ErrorCrit()</code> function can now run on a multiple or a composite <code>InputsCrit</code>. In these cases, it returns a list of <code>ErrorCrit</code>.

- <code>ErrorCrit()</code> and <code>ErrorCrit_&#42;()</code> functions can now assess Q, SCA or SWE simulations.

- <code>Calibration_Michel()</code> function can now run on a composite <code>InputsCrit</code>. It returns a composite value of error and the formula used to calculate it.

- Model diagrams added in documentations of <code>RunModel_GR4J()</code>, <code>RunModel_GR5J()</code> and <code>RunModel_GR6J()</code> functions.

- It is now possible to be redirected to the <code>plot.OutputsModel()</code> documentation with <code>?plot</code>.

- It is now possible to use a character vector for all <code>FUN_&#42;</code> arguments (in addition to function objects) in the following functions: <code>Calibration()</code>, <code>Calibration_Michel()</code>, <code>CreateCalibOptions()</code>, <code>CreateIniStates()</code>, <code>CreateIniStates()</code>, <code>CreateInputsCrit()</code>, <code>CreateInputsModel()</code>, <code>CreateRunOptions()</code>, <code>ErrorCrit()</code>, <code>RunModel()</code> and <code>TransfoParam()</code>.


#### Minor user-visible changes

- <code>ErrorCrit_&#42;()</code> functions now return objects of class <code>ErrorCrit</code> and <code>NSE</code>, <code>KGE</code>, <code>KGE2</code> or <code>RMSE</code>.

- <code>.FortranOutputs()</code> private function added to manage Fortran outputs.

- Outputs of <code>frun_GR2M</code> Fortran subroutine were reordered.

- <code>DataAltiExtrapolation_Valery()</code> now returns named elements of lists relative to elevation layer.

- <code>Calibration()</code> function now returns an error message if <code>FUN_CALIB</code> is not a function.

- Inputs of <code>PEdaily_Oudin()</code> are now checked.

- <code>PEdaily_Oudin()</code> example corrected (the Julian day was one day too early).

- <code>plot.OutputsModel()</code> does not return a warning message anymore when <code>Qobs = NULL</code>.

- Inputs of <code>TransfoParam&#42;()</code> functions are now checked.

- The order of authors has been updated in the DESCRIPTION and the CITATION files.


#### CRAN-compatibility updates

- Tabulations removed, unused variables removed and variable statements fixed in Fortran files.

____________________________________________________________________________________


### 1.0.15.2 Release Notes (2018-10-10)


#### Bug fixes

- Fixed bug in <code>CreateRunOptions()</code>. The function now accounts correctly for leap years when no warm-up period is defined.


#### Minor user-visible changes

- <code>CreateRunOptions()</code> was cleant, with no effect on its outputs.


#### CRAN-compatibility updates

- The <code>vignetteParam&#42;.rda</code> datasets moved to the inst directory. It contains different objects needed for param_optim and param_mcmc vignettes.

____________________________________________________________________________________


### 1.0.14.1 Release Notes (2018-09-28)


#### Deprecated and defunct

- The <code>LatRad</code> argument is now deprecated in <code>PEdaily_Oudin()</code> and replaced by the <code>Lat</code> argument.

- The unused <code>Ind_zeroes</code> argument of the <code>CreateInputsCrit()</code> function is now deprecated.


#### New features

- <code>PEdaily_Oudin()</code> now presents a <code>LatUnit</code> argument which allows to choose the unit of the latitude (radians and degrees).


#### Major user-visible changes

- <code>Calibration_Michel()</code> is now faster during the grid-screening step when a parameter is set using <code>FixedParam</code> in <code>CreateCalibOptions()</code>.

- <code>CreateCalibOptions()</code> now returns an error when all the parameters are set in the <code>FixedParam</code> argument and a warning message when all the parameters are free (NA) in the <code>FixedParam</code> argument.

- <code>CreateInputsCrit()</code> now returns an error when <code>epsilon</code> is not positive.

- <code>CreateInputsCrit()</code> now returns a warning message in the following case: there are zeroes values in <code>Qobs</code>, <code>epsilon = NULL</code> and <code>transfo = log</code> or <code>inv</code>.

- <code>ErrorCrit_&#42;()</code> functions now return a warning message in the following case: there are zeroes values in <code>Qobs</code> or <code>Qsim</code>, <code>epsilon = NULL</code> and <code>transfo = log</code> or <code>inv</code>.


#### Minor user-visible changes

- Several functions of the package were cleant or slightly modified, with no effect on their outputs.

- Dubious Qls and Qmm values set to NA values between 1997-01-05 and 1997-01-21 in the L0123001 dataset.

- ORCID numbers are now joined to the names of the authors of the package.


#### CRAN-compatibility updates

- Function name changed in a vignettes to avoid error during the check on Debian distribution

- As recomanded by CRAN managers, the NEWS file is now at the text format and is no more just a link to the airGR Website

- Added the <code>Vignette_Param.</code> datasets in order to reduce runtime during the re-building of vignettes. It contains different objects needed for param_optim and param_mcmc vignettes.

____________________________________________________________________________________


### 1.0.10.11 Release Notes (2018-06-29)


#### Bug fixes

- Fixed bug in <code>RunModel_GR2M()</code>. The function now returns the total precipitation (P) instead of the net rainfall (P1).


#### Major user-visible changes

- <code>RunModel_GR2M()</code> now returns more explicit precipitation outputs names.

- <code>CreateInputsCrit()</code> now returns a warning message when the KGE (or KGE') is used with a log transformation on flows.

- The article reference is corrected.


#### Minor user-visible changes

- The documentation and help of several functions were improved.

____________________________________________________________________________________


### 1.0.9.64 Release Notes (2017-11-10)


#### New features

- An article describing the airGR package has been published. Its reference has been added and will be displayed with <code>citation("airGR")</code>. 

- Added <code>CreateIniStates()</code> function in order to help user to format the <code>IniStates</code> argument for <code>CreateRunOptions()</code>.

- Added the <code>Param_Sets_GR4J</code> dataset. It contains generalist parameter sets for the GR4J model.

- Three vignettes have been added. They are relative to different calibration methods (including the generalist parameters sets of the GR4J model).


#### Bug fixes

- Fixed bug in <code>RunModel_GR4H()</code>: in <code>frun_GR4H</code> Fortran subroutine, <code>St(2)</code> is now set to 0 (instead of <code>St(1)</code>) when <code>St(2) < 0</code>. 

- Fixed bug in <code>plot.OutputsModel()</code> for the regime plot when the period is less than 1 year.

- Fixed bug in <code>plot.OutputsModel()</code> when there is no common data to plot the cumulative frequency or the correlation QQ.

- Fixed bug in <code>plot.OutputsModel()</code> for the y-axis labelling of flows time series when <code>log_scale = TRUE</code> and <code>BasinArea</code> is used.


#### Deprecated and defunct

- The <code>RunSnowModule</code> argument is now deprecated in <code>CreateRunOptions()</code>.


#### Major user-visible changes

- <code>RunModel_GR4J()</code>, <code>RunModel_GR5J()</code> and <code>RunModel_GR6J()</code> (and <code>CemaNeige_GR&#42;J()</code>) now return Ps, Pn and actual exchanges. See the model Fortran codes for more details about the calculation of these variables.

- <code>CreateInputsModel()</code> now returns an error when <code>DatesR</code> contains duplicated values.

- <code>RunModel_GR5J</code> now returns <code>StateEnd</code> in the same order as the other models.


#### Minor user-visible changes

- <code>plot.OutputsModel()</code> now returns a warning message when the length of Qobs is different from the length of Qsim. 

- The X1 parameter from GR4H, GR4J, GR2M, GR5J and GR6J, the X3 parameter from GR4H, GR4J, GR5J and GR6J and the X6 parameter from GR6J are now set to 1e-2 when they are fixed to lower values. <code>RunModel_&#42;()</code> functions now return a warning message in this case. <code>RunModel_&#42;()</code> functions now return a warning when X4 < 0.5 and its value is set to 0.5.

- The commands <code>?L0123001</code>, <code>?L0123002</code> and <code>?L0123003</code> now return the documentation page related to <code>BasinObs</code>.

- Many functions of the package were cleant or slightly modified, with no effect on their outputs. 

- The documentation and help of several functions were improved.


#### CRAN-compatibility updates

- "airGR.c" file registers native routines.

____________________________________________________________________________________


### 1.0.5.12 Release Notes (2017-01-23)


#### New features

- <code>DataAltiExtrapolation_Valery()</code> and <code>CreateInputsModel()</code> now present a <code>PrecipScale</code> argument which allows rescaling precipitation when it is interpolated on the elevation layers when CemaNeige is used.


#### Bug fixes

- Fixed bug in <code>DataAltiExtrapolation_Valery()</code>. The elevation gradients for air temperature returned by <code>CreateInputsModel()</code> are improved.


#### User-visible changes

- <code>DataAltiExtrapolation_Valery()</code> has been improved. <code>DataAltiExtrapolation_Valery()</code> now runs faster (and by consequence <code>CreateInputsModel()</code> too, when CemaNeige is used).

____________________________________________________________________________________


### 1.0.4 Release Notes (2017-01-18)


#### New features

- <code>RunModel_CemaNeige()</code>, <code>RunModel_CemaNeigeGR4J()</code>, <code>RunModel_CemaNeigeGR5J()</code> and <code>RunModel_CemaNeigeGR6J()</code> now return air temperature for each elevation layer. 


#### Deprecated and defunct

- S3 plot method defined for <code>OutputsModel</code> objects. It means that the <code>plot_OutputsModel()</code> function is deprecated and his use has been replaced by the use of <code>plot.OutputsModel()</code> or <code>plot()</code>.

- In <code>plot.OutputsModel()</code> the <code>PlotChoice</code> argument is deprecated and has been renamed <code>which</code>.


#### User-visible changes

- <code>plot.OutputsModel()</code> displays air temperature time series for each layer when <code>CemaNeige</code> is used (argument <code>which = "Temp"</code> or <code>"all"</code>).

____________________________________________________________________________________


### 1.0.3 Release Notes (2016-12-09)


#### New features

- <code>ErrorCrit_&#42;()</code> functions gain a <code>warnings</code> argument to replace the verbose action and the <code>verbose</code> argument now prints the criterion value(s).


#### Bug fixes

- Fixed bug in <code>CreateCalibOptions()</code> when <code>StartParamList</code> or <code>StartParamDistrib</code> arguments are used.


#### User-visible changes

- <code>CreateInputsModel()</code> now returns an error if <code>NLayers <= 0</code> when <code>CemaNeige</code> is used.

- <code>plot_OutputsModel()</code> now displays raw values on the y-axis when the discharge time series is represented with log scale (formerly, log values of discharges were displayed on the y-axis).


____________________________________________________________________________________


### 1.0.2 Release Notes (2016-11-03)


#### New features

- <code>SeriesAggreg()</code> gains a TimeLag argument that corresponds to a numeric value indicating a time lag (in seconds) for the time series aggregation (useful to aggregate hourly time series to the daily time step for instance).
 In addition, the function now accepts input dates in both <code>POSIXt</code> formats (<code>POSIXct</code> and <code>POSIXlt</code>). The output is in <code>POSIXct</code> format.
 
- <code>plot_OutputsModel()</code> gains a <code>log_scale</code> argument in order to plot the flow with a log scale.

- A tutorial is available online on the following link: from http://webgr.irstea.fr/airGR.
 It can also be displayed with the <code>vignette("airGR")</code> command.
 

#### Bug fixes

- The value <code>sort</code> for the <code>transfo</code> argument of <code>CreateInputsCrit()</code> was not taken into account. It is now fixed. 


#### Deprecated and defunct

- <code>CreateCalibOptions()</code> loses the OptimParam argument that was redundant with the <code>FixedParam</code> argument. The <code>Calibration_Michel()</code> was modified to take into account this change by using directly <code>FixedParam</code>, but this is transparent to the user. 

- <code>CreateCalibOptions()</code> loses the StartParam argument that was not used. 

 
#### Major user-visible changes

- The <code>RunModel_GR6J()</code> and <code>RunModel_CemaNeigeGR6J()</code> models were modified back to versions previous to 1.0.1 to prevent from unwanted efficiency criteria deterioration related to the calibration with <code>Calibration_Michel()</code>. 
 The actual model codes were not modified but the <code>TransfoParam_GR6J()</code> and <code>CreateCalibOptions()</code> functions were modified regarding the X5 parameter. 
 It is strongly advised to use airGR 1.0.2 for the <code>RunModel_GR6J()</code> and <code>RunModel_CemaNeigeGR6J()</code> functions if you are using <code>Calibration_Michel()</code>, as they are much more efficient. 
 In case you were using your own calibration algorithm, you will not notice any difference. 


#### Minor user-visible changes

- <code>CreateInputsModel()</code> and <code>DataAltiExtrapolation_Valery()</code> functions now allow both <code>POSIXt</code> formats (<code>POSIXct</code> and <code>POSIXlt</code>).


____________________________________________________________________________________


### 1.0.1 Release Notes (2016-04-21)


#### Deprecated and defunct

- The <code>Calibration_HBAN()</code> and <code>DataAltiExtrapolation_HBAN()</code> functions have respectively been renamed as <code>Calibration_Michel()</code> and <code>DataAltiExtrapolation_Valery()</code> after the names of their creators.

- The <code>Calibration_optim()</code> function has been removed from the package.

- The silent mode is now defined by the <code>verbose = TRUE</code> argument (formerly <code>quiet = FALSE</code>) in the following functions:
<code>Calibration()</code>, <code>Calibration_Michel()</code>, <code>CreateInputsModel()</code>, <code>CreateRunOptions()</code>, <code>DataAltiExtrapolation_Valery()</code>, <code>ErrorCrit()</code>, <code>ErrorCrit_KGE()</code>, <code>ErrorCrit_KGE2()</code>, <code>ErrorCrit_NSE()</code>, <code>ErrorCrit_RMSE()</code>, <code>plot_OutputsModel()</code>, <code>SeriesAggreg()</code>.


#### Major user-visible changes
- The GR5J model has been modified: previously, two unit hydrographs were used, now only one is remaining.
 As a consequence, simulations from the GR5J (<code>RunModel_GR5J()</code> function) and CemaNeige (<code>RunModel_CemaNeigeGR5J()</code> function) models will be different.

- An important proportion of the transformations of the parameters have been modified (<code>TransfoParam_&#42;()</code> functions). Since this modifies the local search, calibration results will be different .

- The quantiles of the parameters have been recalculated with the new transformations (<code>CreateCalibOptions()</code> function). Since these quantiles constitute the starting point of the calibration algorithm, calibration results will be different.


#### Minor user-visible changes

- The Fortran model core codes have been modified:
	- optimisation of the codes for fastening of computation;
	- simplification of the internal variables for easier reading and understanding.
	
- The list of the contributors and authors is now full.

- The references of the package has been updated; they are returned by the following R-command <code>citation("airGR")</code>.

____________________________________________________________________________________


### 0.8.1.2 Release Notes (2015-08-21)


#### Bug fixes

- Fixed bug in <code>CreateInputsModel()</code> that was related to the handling of missing values.

- Fixed bug in <code>CreateRunOptions()</code> that prevented the correct use of the <code>IniResLevels</code> argument (to manually set the filling rate of the production and routing stores).


#### Minor user-visible changes

- Removal of an unnecessary warning when <code>IndPeriod_WarmUp = 0</code>.


#### CRAN-compatibility updates

- Modification of namespace file to ensure proper use under linux whithout compilation issues.


____________________________________________________________________________________


### 0.8.0.2 Release Notes (2015-04-15)


#### New features

- Three new hydrological models: <code>RunModel_GR4H() function for </code> GR4H (hourly), <code>RunModel_GR2M()</code> function for GR2M (monthly) and <code>RunModel_GR1A()</code> function for GR1A (yearly).

- New function <code>SeriesAggreg()</code> to easily aggreg timesteps.


#### Bug fixes

- Fixed bug in <code>ErrorCrit_RMSE()</code> which led to incorrect calibration (the criterion was maximised instead of minimised).


#### Major user-visible changes

- Update of the functions <code>CreateRunOptions()</code>, <code>CreateCalibOptions()</code> and <code>plot_OutputsModel()</code> to handle the new models.

- Modification of CemaNeige Fortran code to add an update of Gratio after the SnowPack update (no impact on snow simulation).


#### Minor user-visible changes

- Improvement of the <code>plot_OutputsModel()</code> function to allow a selection among available plots.

- Minor update in <code>ErrorCrit_KGE()</code> and <code>ErrorCrit_KGE2()</code> to handle case when only one values in not NA.

- Update of the scripts in airGR-advanced-example to match the structures of the <code>BasinData</code> objects.

- Correction of formatting issue in airGR-advanced-example regarding the "List_HypsoData.txt" file.


____________________________________________________________________________________


### 0.7.4 Release Notes (2014-11-01)


#### New features

- New argument in many functions (<code>quiet = TRUE</code> or <code>FALSE</code>) to choose if the warnings should be suppressed or not.


#### Bug fixes

- Fixed bug in <code>CreateCalibOptions()</code> to handle models with only one parameter.

- Fixed bug in <code>Calibration_HBAN()</code>. The function was not working properly with models having only one parameter.


#### Deprecated and defunct

- The <code>CalibrationAlgo_&#42;()</code> functions were renamed into <code>Calibration_&#42;()</code>.


#### Major user-visible changes

- CemaNeige users must now specify one <code>MeanAnSolidPrecip</code> for each elevation layer. The <code>CreateRunOptions()</code> function is impacted.

- CemaNeige users can now specify the mean elevation of the input series (before it was always considered equal to the catchment median elevation). 
 The impacted functions are <code>CreateInputsModel()</code> and <code>DataAltiExtrapolation_HBAN()</code>.
 
- New architecture with better format verification procedure (using classes) and simpler setting of default configuration.

- New architecture where the model, calibration and error functions are in the arguments of the functions
 (the exotic use of "generic function" created by the users has been removed).

- Improved documentation and examples. 


#### Minor user-visible changes

- Improvements allowing the arrival of new models.

- Improvements of the argument verifications in <code>CreateInputsModel()</code>, <code>CreateRunOptions()</code>, <code>CreateInputsCrit()</code>, <code>CreateCalibOptions()</code>.

- Improvements of all the <code>ErrorCrit()</code> functions to better account for the cases with constant flow values or local zeros.

- Improvement of the <code>plot_OutputsModel</code> function (to handle 0 in Qobs and Qsim).

- Improved documentation.


____________________________________________________________________________________


### 0.6.2 Release Notes (2014-02-12)


#### New features

- Additional functions for results plotting (the <code>{zoo}</code> package is required for some of them).

- Add multi-objective calibration using <code>nsga2()</code> (the <code>{mco}</code> package is required).

- The field Multiplier has been added in the ErrorCrit() outputs, to indicate whether the criterion is an error (to minimise) or and efficiency (to maximise).
 This allows to provide real efficiency values in the outputs e.g. NSE[Q] instead of (-1) &times; NSE[Q].


#### Bug fixes

- RC11 bug correction: the automatic selection of the warm-up period was not working properly when no data was available from warm-up (i.e. when the user had set the run to start at the very first index).

- RC10 bug correction: the <code>CalibrationAlgo_HBAN()</code> function was not working in the very rare case when the diagonal search was activated and lead to a set outside the authorised range.

- RC9 bug correction: the <code>CalibrationAlgo_HBAN()</code> function was not working properly with models having only one parameter.

- RC8 bug correction of the <code>ModelDefaultIniOptions()</code> function (this bug was introduced in the RC7 and caused an error when <code>IndPeriod_WarmUp = NULL</code>.

- RC7 bug correction of the <code>ModelDefaultIniOptions()</code> function (the automatic selection of one year for warm-up was not handling properly missing data).

- RC6 correction of the help files (the description of CemaNeige parameters were inverted).

- RC5 differs from previous releases in the way the data are read and stored (in a list instead of individual vectors).
 The package is similar, only the examples of Main and the files in MyScriptBlocks have changed.
 All basin data are now stored inside a list named <code>BasinData</code>. This will greatly ease the future use of Rdata files (instead of txt files) as storage format for the time series of observation. 


#### Deprecated and defunct

- <code>EfficiencyCrit()</code> have been replaced by <code>ErrorCrit()</code> to avoid misunderstanding (by default, the algorithms minimise the error criterion).


#### Major user-visible changes

- The definition of the generic function is now made in a much simpler way (e.g. see <code>DefineFunctions_Model()</code> or <code>DefineFunctions_ErrorCrit()</code>).


#### Minor user-visible changes

- Code improvements to reduce the computation time.

- Clearer instructions for the adding and modification of a model.

- Improvements of the documentation.


____________________________________________________________________________________


### 0.5.2 Release Notes (2014-02-05)


#### Deprecated and defunct

- The <code>SelectPer</code> arguments are replaced by <code>IndPeriod</code> to ease understanding.

- The <code>PE</code> arguments are replaced by <code>PotEvap()</code> to ease understanding.

- The <code>Fsol</code> arguments are replaced by <code>FracSolidPrecip</code> to ease understanding.


#### Major user-visible changes

- R 2.15 in not supported by default.

- The check that <code>SelectPer_Run()</code> is continuous is now made in the <code>CheckArg()</code> functions.

- Check of the model functioning time step.

- Name of the calibration criterion provided in <code>OutputsAlgo()</code>.


#### Minor user-visible changes

- Missing values in Fortran are now -999.999 instead of -9.999.


____________________________________________________________________________________


### 0.5.1 Release Notes (2014-01-27)


#### New features

- New <code>EfficiencyCrit_NSE_sqrtQ()</code> function to compute NSE criterion on sqrt flows.


#### Bug fixes

- Incorrect arguments in the call to <code>RunModelAndCrit</code> from <code>CalibrationAlgo_optim_stats</code> and <code>CalibrationAlgo_nlminb_stats</code>.

- <code>CalibrationAlgo_nlminb_stats</code> argument was wrongly defined in <code>DefineFunctions_CalibrationAlgo()</code> (<code>optim</code> instead of <code>nlminb</code>).

- Format checking for <code>RunOptions</code> was incorrectly made in <code>CheckArg()</code> function.



## ----warning=FALSE, include=FALSE---------------------------------------------
library(airGR)
load(system.file("vignettesData/vignetteCNHysteresis.rda", package = "airGR"))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  data(X0310010)
#  summary(BasinObs)

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## preparation of the InputsModel object
#  InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                   DatesR = BasinObs$DatesR, Precip = BasinObs$P,
#                                   PotEvap = BasinObs$E, TempMean = BasinObs$T,
#                                   ZInputs = median(BasinInfo$HypsoData),
#                                   HypsoData = BasinInfo$HypsoData, NLayers = 5)
#  
#  ## ---- calibration step
#  
#  ## calibration period selection
#  Ind_Cal <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "2000-09-01"),
#                 which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "2005-08-31"))
#  
#  
#  ## ---- validation step
#  
#  ## validation period selection
#  Ind_Val <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "2005-09-01"),
#                 which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "2010-07-31"))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## preparation of the RunOptions object for the calibration period
#  RunOptions_Cal <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                     InputsModel = InputsModel, IndPeriod_Run = Ind_Cal,
#                                     IsHyst = TRUE)
#  
#  ## preparation of the RunOptions object for the validation period
#  RunOptions_Val <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                     InputsModel = InputsModel, IndPeriod_Run = Ind_Val,
#                                     IsHyst = TRUE)
#  
#  ## preparation of the CalibOptions object
#  CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                     FUN_CALIB = Calibration_Michel,
#                                     IsHyst = TRUE)

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## efficiency criterion: 75 % KGE'(Q) + 5 % KGE'(SCA) on each of the 5 layers
#  InputsCrit_Cal  <- CreateInputsCrit(FUN_CRIT = rep("ErrorCrit_KGE2", 6),
#                                      InputsModel = InputsModel, RunOptions = RunOptions_Cal,
#                                      Obs = list(BasinObs$Qmm[Ind_Cal],
#                                                 BasinObs$SCA1[Ind_Cal],
#                                                 BasinObs$SCA2[Ind_Cal],
#                                                 BasinObs$SCA3[Ind_Cal],
#                                                 BasinObs$SCA4[Ind_Cal],
#                                                 BasinObs$SCA5[Ind_Cal]),
#                                      VarObs = list("Q", "SCA", "SCA", "SCA", "SCA", "SCA"),
#                                      Weights = list(0.75, 0.05, 0.05, 0.05, 0.05, 0.05))
#  
#  InputsCrit_Val  <- CreateInputsCrit(FUN_CRIT = rep("ErrorCrit_KGE2", 6),
#                                      InputsModel = InputsModel, RunOptions = RunOptions_Val,
#                                      Obs = list(BasinObs$Qmm[Ind_Val],
#                                                 BasinObs$SCA1[Ind_Val],
#                                                 BasinObs$SCA2[Ind_Val],
#                                                 BasinObs$SCA3[Ind_Val],
#                                                 BasinObs$SCA4[Ind_Val],
#                                                 BasinObs$SCA5[Ind_Val]),
#                                      VarObs = list("Q", "SCA", "SCA", "SCA", "SCA", "SCA"),
#                                      Weights = list(0.75, 0.05, 0.05, 0.05, 0.05, 0.05))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## calibration
#  OutputsCalib <- Calibration(InputsModel = InputsModel, RunOptions = RunOptions_Cal,
#                              InputsCrit = InputsCrit_Cal, CalibOptions = CalibOptions,
#                              FUN_MOD = RunModel_CemaNeigeGR4J,
#                              FUN_CALIB = Calibration_Michel)

## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  ## run on the calibration period
#  OutputsModel_Cal <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
#                                             RunOptions = RunOptions_Cal,
#                                             Param = OutputsCalib$ParamFinalR)
#  
#  ## evaluation
#  OutputsCrit_Cal <- ErrorCrit(InputsCrit = InputsCrit_Cal, OutputsModel = OutputsModel_Cal)

## ----warning=FALSE------------------------------------------------------------
str(OutputsCrit_Cal, max.level = 2)

## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  ## run on the validation period
#  OutputsModel_Val <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
#                                             RunOptions = RunOptions_Val,
#                                             Param = OutputsCalib$ParamFinalR)
#  
#  ## evaluation
#  OutputsCrit_Val <- ErrorCrit(InputsCrit = InputsCrit_Val, OutputsModel = OutputsModel_Val)

## ----warning=FALSE------------------------------------------------------------
str(OutputsCrit_Val, max.level = 2)

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## preparation of RunOptions object
#  RunOptions_Cal_NoHyst <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                            InputsModel = InputsModel,
#                                            IndPeriod_Run = Ind_Cal,
#                                            IsHyst = FALSE)
#  
#  RunOptions_Val_NoHyst <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                            InputsModel = InputsModel,
#                                            IndPeriod_Run = Ind_Val,
#                                            IsHyst = FALSE)
#  
#  InputsCrit_Cal_NoHyst <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE2,
#                                            InputsModel = InputsModel,
#                                            RunOptions = RunOptions_Cal_NoHyst,
#                                            Obs = BasinObs$Qmm[Ind_Cal], VarObs = "Q")
#  
#  ## preparation of CalibOptions object
#  CalibOptions_NoHyst <- CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
#                                            FUN_CALIB = Calibration_Michel,
#                                            IsHyst = FALSE)

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  ## calibration
#  OutputsCalib_NoHyst <- Calibration(InputsModel = InputsModel,
#                                     InputsCrit = InputsCrit_Cal_NoHyst,
#                                     RunOptions = RunOptions_Cal_NoHyst,
#                                     CalibOptions = CalibOptions_NoHyst,
#                                     FUN_MOD = RunModel_CemaNeigeGR4J,
#                                     FUN_CALIB = Calibration_Michel)

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  OutputsModel_Cal_NoHyst <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
#                                                    RunOptions = RunOptions_Cal_NoHyst,
#                                                    Param = OutputsCalib_NoHyst$ParamFinalR)
#  
#  OutputsModel_Val_NoHyst <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
#                                                    RunOptions = RunOptions_Val_NoHyst,
#                                                    Param = OutputsCalib_NoHyst$ParamFinalR)

## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  OutputsCrit_Cal_NoHyst <- ErrorCrit(InputsCrit = InputsCrit_Cal,
#                                      OutputsModel = OutputsModel_Cal_NoHyst)
#  
#  OutputsCrit_Val_NoHyst <- ErrorCrit(InputsCrit = InputsCrit_Val,
#                                      OutputsModel = OutputsModel_Val_NoHyst)

## ----warning=FALSE------------------------------------------------------------
str(OutputsCrit_Cal_NoHyst, max.level = 2)
str(OutputsCrit_Val_NoHyst, max.level = 2)


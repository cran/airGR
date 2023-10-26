## ----include=FALSE, fig.keep='none', results='hide'---------------------------
library(airGR)
options(digits = 3)

## ----warning=FALSE, include=FALSE---------------------------------------------
library(airGR)
options(digits = 3)

## ----warning=FALSE------------------------------------------------------------
## loading catchment data
data(L0123001)

## -----------------------------------------------------------------------------
QObsDown <- (BasinObs$Qmm + c(0, 0, BasinObs$Qmm[1:(length(BasinObs$Qmm)-2)])) / 2
options(digits = 5)
summary(cbind(QObsUp = BasinObs$Qmm, QObsDown))
options(digits = 3)

## -----------------------------------------------------------------------------
Velocity <- 100 * 1e3 / (2 * 86400)
paste("Velocity: ", format(Velocity), "m/s")

## -----------------------------------------------------------------------------
InputsModelUp <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                   Precip = BasinObs$P, PotEvap = BasinObs$E)
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31"))
RunOptionsUp <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                 InputsModel = InputsModelUp,
                                 IndPeriod_WarmUp = NULL, IndPeriod_Run = Ind_Run,
                                 IniStates = NULL, IniResLevels = NULL)
# Error criterion is KGE computed on the root-squared discharges
InputsCritUp <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModelUp,
                                 RunOptions = RunOptionsUp,
                                 VarObs = "Q", Obs = BasinObs$Qmm[Ind_Run],
                                 transfo = "sqrt")
CalibOptionsUp <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
OutputsCalibUp <- Calibration_Michel(InputsModel = InputsModelUp, RunOptions = RunOptionsUp,
                                     InputsCrit = InputsCritUp, CalibOptions = CalibOptionsUp,
                                     FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
OutputsModelUp <- RunModel_GR4J(InputsModel = InputsModelUp, RunOptions = RunOptionsUp,
                                Param = OutputsCalibUp$ParamFinalR)

## -----------------------------------------------------------------------------
InputsModelDown1 <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
  Precip = BasinObs$P, PotEvap = BasinObs$E,
  Qupstream = matrix(BasinObs$Qmm, ncol = 1), # upstream observed flow
  LengthHydro = 100, # distance between upstream catchment outlet & the downstream one [km]
  BasinAreas = c(180, 180) # upstream and downstream areas [km²]
)

## -----------------------------------------------------------------------------
Qsim_upstream <- rep(NA, length(BasinObs$DatesR))
# Simulated flow during warm-up period (365 days before run period)
Qsim_upstream[Ind_Run[seq_len(365)] - 365] <- OutputsModelUp$RunOptions$WarmUpQsim
# Simulated flow during run period
Qsim_upstream[Ind_Run] <- OutputsModelUp$Qsim

InputsModelDown2 <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
  Precip = BasinObs$P, PotEvap = BasinObs$E,
  Qupstream = matrix(Qsim_upstream, ncol = 1), # upstream observed flow
  LengthHydro = 100, # distance between upstream catchment outlet & the downstream one [km]
  BasinAreas = c(180, 180) # upstream and downstream areas [km²]
)

## -----------------------------------------------------------------------------
RunOptionsDown <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModelDown1,
                                   IndPeriod_WarmUp = NULL, IndPeriod_Run = Ind_Run,
                                   IniStates = NULL, IniResLevels = NULL)
InputsCritDown <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModelDown1,
                                   RunOptions = RunOptionsDown,
                                   VarObs = "Q", Obs = QObsDown[Ind_Run],
                                   transfo = "sqrt")
CalibOptionsDown <- CreateCalibOptions(FUN_MOD = RunModel_GR4J,
                                       FUN_CALIB = Calibration_Michel,
                                       IsSD = TRUE) # specify that it's a SD model
OutputsCalibDown1 <- Calibration_Michel(InputsModel = InputsModelDown1,
                                        RunOptions = RunOptionsDown,
                                        InputsCrit = InputsCritDown,
                                        CalibOptions = CalibOptionsDown,
                                        FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
OutputsModelDown1 <- RunModel(InputsModel = InputsModelDown2,
                              RunOptions = RunOptionsDown,
                              Param = OutputsCalibDown1$ParamFinalR,
                              FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
KGE_down1 <- ErrorCrit_KGE(InputsCritDown, OutputsModelDown1)

## -----------------------------------------------------------------------------
OutputsCalibDown2 <- Calibration_Michel(InputsModel = InputsModelDown2,
                                        RunOptions = RunOptionsDown,
                                        InputsCrit = InputsCritDown,
                                        CalibOptions = CalibOptionsDown,
                                        FUN_MOD = RunModel_GR4J)
ParamDown2 <- OutputsCalibDown2$ParamFinalR

## -----------------------------------------------------------------------------
ParamDownTheo <- c(Velocity, OutputsCalibUp$ParamFinalR)

## -----------------------------------------------------------------------------
IC_Lavenne <- CreateInputsCrit_Lavenne(InputsModel = InputsModelDown2,
                                    RunOptions = RunOptionsDown,
                                    Obs = QObsDown[Ind_Run],
                                    AprParamR = ParamDownTheo,
                                    AprCrit = OutputsCalibUp$CritFinal)

## -----------------------------------------------------------------------------
OutputsCalibDown3 <- Calibration_Michel(InputsModel = InputsModelDown2,
                                        RunOptions = RunOptionsDown,
                                        InputsCrit = IC_Lavenne,
                                        CalibOptions = CalibOptionsDown,
                                        FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
OutputsModelDown3 <- RunModel(InputsModel = InputsModelDown2,
                              RunOptions = RunOptionsDown,
                              Param = OutputsCalibDown3$ParamFinalR,
                              FUN_MOD = RunModel_GR4J)
KGE_down3 <- ErrorCrit_KGE(InputsCritDown, OutputsModelDown3)

## -----------------------------------------------------------------------------
mVelocity <- matrix(c(Velocity,
                      OutputsCalibDown1$ParamFinalR[1],
                      OutputsCalibDown2$ParamFinalR[1],
                      OutputsCalibDown3$ParamFinalR[1]),
                    ncol = 1,
                    dimnames = list(c("theoretical",
                                      "calibrated with observed upstream flow",
                                      "calibrated with simulated  upstream flow",
                                      "calibrated with sim upstream flow and regularisation"),
                                    c("Velocity parameter")))
knitr::kable(mVelocity)

## -----------------------------------------------------------------------------
OutputsModelDownTheo <- RunModel(InputsModel = InputsModelDown2,
                                 RunOptions = RunOptionsDown,
                                 Param = ParamDownTheo,
                                 FUN_MOD = RunModel_GR4J)
KGE_downTheo <- ErrorCrit_KGE(InputsCritDown, OutputsModelDownTheo)

## -----------------------------------------------------------------------------
comp <- matrix(c(0, OutputsCalibUp$ParamFinalR,
                 rep(OutputsCalibDown1$ParamFinalR, 2),
                 OutputsCalibDown2$ParamFinalR,
                 OutputsCalibDown3$ParamFinalR,
                 ParamDownTheo),
               ncol = 5, byrow = TRUE)
comp <- cbind(comp, c(OutputsCalibUp$CritFinal,
                      OutputsCalibDown1$CritFinal,
                      KGE_down1$CritValue,
                      OutputsCalibDown2$CritFinal,
                      KGE_down3$CritValue,
                      KGE_downTheo$CritValue))
colnames(comp) <- c("Velocity", paste0("X", 1:4), "KGE(√Q)")
rownames(comp) <- c("Calibration of the upstream subcatchment",
                    "Calibration 1 with observed upstream flow",
                    "Validation 1 with simulated upstream flow",
                    "Calibration 2 with simulated upstream flow",
                    "Calibration 3 with simulated upstream flow and regularisation",
                    "Validation theoretical set of parameters")
knitr::kable(comp)


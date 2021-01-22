## ---- include=FALSE, fig.keep='none', results='hide'--------------------------
library(airGR)
options(digits = 3)
library(imputeTS)

## ---- warning=FALSE, include=FALSE--------------------------------------------
library(airGR)
options(digits = 3)

## ---- warning=FALSE-----------------------------------------------------------
## loading catchment data
data(L0123001)

## -----------------------------------------------------------------------------
QObsDown <- (BasinObs$Qmm + c(0, 0, BasinObs$Qmm[1:(length(BasinObs$Qmm)-2)])) / 2
summary(cbind(QObsUp = BasinObs$Qmm, QObsDown))

## -----------------------------------------------------------------------------
InputsModelUp <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                   Precip = BasinObs$P, PotEvap = BasinObs$E)
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31"))
RunOptionsUp <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                 InputsModel = InputsModelUp, IndPeriod_Run = Ind_Run,
                                 IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)
InputsCritUp <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModelUp,
                                 RunOptions = RunOptionsUp, VarObs = "Q", Obs = BasinObs$Qmm[Ind_Run])
CalibOptionsUp <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
OutputsCalibUp <- Calibration_Michel(InputsModel = InputsModelUp, RunOptions = RunOptionsUp,
                                     InputsCrit = InputsCritUp, CalibOptions = CalibOptionsUp,
                                     FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
OutputsModelUp <- RunModel_GR4J(InputsModel = InputsModelUp, RunOptions = RunOptionsUp,
                                Param = OutputsCalibUp$ParamFinalR)

## -----------------------------------------------------------------------------
QObsUp <- imputeTS::na_interpolation(BasinObs$Qmm)

## -----------------------------------------------------------------------------
InputsModelDown1 <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
  Precip = BasinObs$P, PotEvap = BasinObs$E,
  Qupstream = matrix(QObsUp, ncol = 1), # Upstream observed flow
  LengthHydro = 100 * 1000, # Distance between upstream catchment outlet and the downstream one in m
  BasinAreas = c(180, 180) # Upstream and downstream areas in km²
)

## -----------------------------------------------------------------------------
RunOptionsDown <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = InputsModelDown1, IndPeriod_Run = Ind_Run,
                                   IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)
InputsCritDown <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModelDown1,
                                   RunOptions = RunOptionsDown, VarObs = "Q", Obs = QObsDown[Ind_Run])
CalibOptionsDown <- CreateCalibOptions(FUN_MOD = RunModel_GR4J,
                                       FUN_CALIB = Calibration_Michel,
                                       IsSD = TRUE) # Don't forget to specify that it's an SD model here
OutputsCalibDown1 <- Calibration_Michel(InputsModel = InputsModelDown1, RunOptions = RunOptionsDown,
                                        InputsCrit = InputsCritDown, CalibOptions = CalibOptionsDown,
                                        FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
InputsModelDown2 <- InputsModelDown1
InputsModelDown2$Qupstream[Ind_Run] <- OutputsModelUp$Qsim

## -----------------------------------------------------------------------------
OutputsModelDown1 <- RunModel(InputsModel = InputsModelDown2,
                              RunOptions = RunOptionsDown,
                              Param = OutputsCalibDown1$ParamFinalR,
                              FUN_MOD = RunModel_GR4J)

## -----------------------------------------------------------------------------
CritDown1 <- ErrorCrit_NSE(InputsCritDown, OutputsModelDown1)

## -----------------------------------------------------------------------------
OutputsCalibDown2 <- Calibration_Michel(InputsModel = InputsModelDown2, RunOptions = RunOptionsDown,
                                        InputsCrit = InputsCritDown, CalibOptions = CalibOptionsDown,
                                        FUN_MOD = RunModel_GR4J)
ParamDown2 <- OutputsCalibDown2$ParamFinalR

## -----------------------------------------------------------------------------
Lag <- InputsModelDown1$LengthHydro / (2 * 86400)
paste(format(Lag), "m/s")

## -----------------------------------------------------------------------------
mLag <- matrix(c(Lag, OutputsCalibDown1$ParamFinalR[1], OutputsCalibDown2$ParamFinalR[1]), ncol = 1)
rownames(mLag) = c("theoretical", "calibrated with observed upstream flow",
                   "calibrated with simulated  upstream flow")
colnames(mLag) = c("Lag parameter")
knitr::kable(mLag)

## -----------------------------------------------------------------------------
ParamDownTheo <- c(Lag, OutputsCalibUp$ParamFinalR)
OutputsModelDownTheo <- RunModel(InputsModel = InputsModelDown2,
                              RunOptions = RunOptionsDown,
                              Param = ParamDownTheo,
                              FUN_MOD = RunModel_GR4J)
CritDownTheo <- ErrorCrit_NSE(InputsCritDown, OutputsModelDownTheo)

## -----------------------------------------------------------------------------
comp <- matrix(c(0, OutputsCalibUp$ParamFinalR, rep(OutputsCalibDown1$ParamFinalR, 2),
                 OutputsCalibDown2$ParamFinalR, ParamDownTheo), ncol = 5, byrow = TRUE)
comp <- cbind(comp, c(OutputsCalibUp$CritFinal, OutputsCalibDown1$CritFinal,
                      CritDown1$CritValue,  OutputsCalibDown2$CritFinal, CritDownTheo$CritValue))
colnames(comp) <- c("Lag", paste0("x", 1:4), "NSE")
rownames(comp) <- c("Calibration of the upstream subcatchment",
                    "Calibration 1 with observed upstream flow",
                    "Validation 1 with simulated upstream flow",
                    "Calibration 2 with simulated upstream flow",
                    "Validation theoretical set of parameters")
knitr::kable(comp)

context("RunModel_Lag")

data(L0123001)

test_that("'BasinAreas' must have one more element than 'LengthHydro'", {
  expect_error(
    InputsModel <- CreateInputsModel(
      FUN_MOD = RunModel_GR4J,
      DatesR = BasinObs$DatesR,
      Precip = BasinObs$P,
      PotEvap = BasinObs$E,
      Qupstream = matrix(BasinObs$Qmm, ncol = 1),
      LengthHydro = 1,
      BasinAreas = 1
    ),
    regexp = "'BasinAreas' must have one more element than 'LengthHydro'"
  )
})

BasinAreas <- c(BasinInfo$BasinArea, BasinInfo$BasinArea)

# Qupstream = sinusoid synchronised on hydrological year from 0 mm to mean value of Qobs
Qupstream <- floor((sin((seq_along(BasinObs$Qmm)/365*2*3.14))+1) * mean(BasinObs$Qmm, na.rm = TRUE))

InputsModel <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E,
  Qupstream = matrix(Qupstream, ncol = 1),
  LengthHydro = 1,
  BasinAreas = BasinAreas
)

Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31"))

RunOptions <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                InputsModel = InputsModel,
                                                IndPeriod_Run = Ind_Run))

test_that("QcontribDown parameter should be a numeric vector or an OutputModel object", {
  regexp = "'QcontribDown' must be a numeric vector or a 'OutputsModel' object"
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = "A"),
    regexp = regexp
  )
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = NULL),
    regexp = regexp
  )
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = matrix(1, ncol = 1)),
    regexp = regexp
  )
})

Param <- c(257.237556, 1.012237, 88.234673, 2.207958) #  From vignettes/V01_get_started

RunOptionsGR4J <- RunOptions
RunOptionsGR4J$FeatFUN_MOD$NbParam <- 4
OutputsGR4JOnly <- RunModel_GR4J(InputsModel = InputsModel,
                                 RunOptions = RunOptionsGR4J,
                                 Param = Param)

test_that("QcontribDown should contain a Qsim key", {
  QcontribDown <- OutputsGR4JOnly
  QcontribDown$Qsim <- NULL
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = QcontribDown),
    regexp = "should contain a key 'Qsim'"
  )
})

test_that("'QcontribDown$Qim' should have the same length as 'RunOptions$IndPeriod_Run'", {
  QcontribDown <- OutputsGR4JOnly
  QcontribDown$Qsim <- c(QcontribDown$Qsim, 0)
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = QcontribDown),
    regexp = "should have the same length as"
  )
})

test_that("OutputsModel must have a item 'QsimDown' equal to GR4J Qsim contribution", {
  expect_equal(OutputsGR4JOnly$Qsim,
               RunModel_Lag(InputsModel = InputsModel,
                            RunOptions = RunOptions,
                            Param = 1,
                            QcontribDown = OutputsGR4JOnly)$QsimDown)
})

test_that("RunModel(FUN=RunModel_Lag) should give same result as RunModel_Lag", {
  QcontribDown <- OutputsGR4JOnly
  Output_RunModel_Lag <- RunModel_Lag(InputsModel = InputsModel,
                                      RunOptions = RunOptions,
                                      Param = 1,
                                      QcontribDown = QcontribDown)
  Output_RunModel <- RunModel(InputsModel = InputsModel,
                              RunOptions = RunOptions,
                              Param = 1,
                              FUN_MOD = RunModel_Lag,
                              QcontribDown = QcontribDown)
  expect_equal(Output_RunModel, Output_RunModel_Lag)
})

test_that("'Qupstream' contain NA values", {
  expect_warning(
    InputsModel <- CreateInputsModel(
      FUN_MOD = RunModel_GR4J,
      DatesR = BasinObs$DatesR,
      Precip = BasinObs$P,
      PotEvap = BasinObs$E,
      Qupstream = matrix(BasinObs$Qmm, ncol = 1),
      LengthHydro = 1,
      BasinAreas = BasinAreas
    ),
    regexp = "'Qupstream' contains NA values: model outputs will contain NAs"
  )

  RunOptions <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                  InputsModel = InputsModel,
                                                  IndPeriod_Run = Ind_Run))
  QcontribDown <- OutputsGR4JOnly
  # Warning with RunModel_Lag
  expect_warning(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = QcontribDown),
    regexp = "time steps with NA values"
  )
  # No warning during calibration
  RunOptions$Outputs_Sim <- RunOptions$Outputs_Cal
  expect_warning(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1, QcontribDown = QcontribDown),
    regexp = NA
  )
})

test_that("Upstream basin with nil area should return same Qdown as GR4J alone", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(Qupstream, ncol = 1),
    LengthHydro = 1,
    BasinAreas = c(0,BasinAreas[2])
  )
  OutputsSD <- RunModel(InputsModel,
                        RunOptions,
                        Param = c(1, Param),
                        FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsGR4JOnly$Qsim, OutputsSD$Qsim)
})

test_that("Downstream basin with nil area and nul upstream length should return same Qdown as Qupstream alone", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(Qupstream, ncol = 1),
    LengthHydro = 0,
    BasinAreas = c(BasinInfo$BasinArea, 0)
  )
  OutputsSD <- RunModel(InputsModel,
                        RunOptions,
                        Param = c(1, Param),
                        FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsSD$Qsim, Qupstream[Ind_Run])
})

ParamSD <- c(InputsModel$LengthHydro * 1e3 / (24 * 60 * 60), Param) # Speed corresponding to one time step delay

Qm3GR4Only <- OutputsGR4JOnly$Qsim * InputsModel$BasinAreas[2L] * 1e3

test_that("1 input with lag of 1 time step delay out gives an output delayed of one time step", {
  OutputsSD <- RunModel(InputsModel, RunOptions, Param = ParamSD, FUN_MOD = RunModel_GR4J)
  Qm3SdSim <- OutputsSD$Qsim_m3
  Qm3UpstLagObs <- Qupstream[Ind_Run - 1] * InputsModel$BasinAreas[1] * 1e3
  expect_equal(Qm3SdSim - Qm3GR4Only, Qm3UpstLagObs)
})

test_that("1 input with lag of 0.5 time step delay out gives an output delayed of 0.5 time step", {
  OutputsSD <- RunModel(InputsModel, RunOptions,
                        Param = c(InputsModel$LengthHydro * 1e3 / (12 * 3600), Param),
                        FUN_MOD = RunModel_GR4J)
  Qm3SdSim <- OutputsSD$Qsim_m3
  Qm3UpstLagObs <- (Qupstream[Ind_Run] + Qupstream[Ind_Run - 1]) / 2 * InputsModel$BasinAreas[1] * 1e3
  expect_equal(Qm3SdSim - Qm3GR4Only, Qm3UpstLagObs)
})

test_that("Qupstream in different units should return the same result", {
  OutputsSD_mm <- RunModel(InputsModel, RunOptions,
                           Param = ParamSD,
                           FUN_MOD = RunModel_GR4J)
  InputsModel_m3 <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(Qupstream, ncol = 1) * BasinAreas[1] * 1e3,
    LengthHydro = 1,
    BasinAreas = BasinAreas,
    QupstrUnit = "m3"
  )
  OutputsSD_m3 <- RunModel(InputsModel_m3, RunOptions,
                           Param = ParamSD,
                           FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsSD_mm$Qsim, OutputsSD_m3$Qsim)

  InputsModel_m3s <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(Qupstream, ncol = 1) * BasinAreas[1] * 1e3 / 86400,
    LengthHydro = 1,
    BasinAreas = BasinAreas,
    QupstrUnit = "m3/s"
  )
  OutputsSD_m3s <- RunModel(InputsModel_m3s, RunOptions,
                            Param = ParamSD,
                            FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsSD_mm$Qsim, OutputsSD_m3s$Qsim)

  InputsModel_ls <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(Qupstream, ncol = 1) * BasinAreas[1] * 1e6 / 86400,
    LengthHydro = 1,
    BasinAreas = BasinAreas,
    QupstrUnit = "L/s"
  )
  OutputsSD_ls <- RunModel(InputsModel_ls, RunOptions,
                           Param = ParamSD,
                           FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsSD_mm$Qsim, OutputsSD_ls$Qsim)
})

InputsCrit <- CreateInputsCrit(
  FUN_CRIT = ErrorCrit_NSE,
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  VarObs = "Q",
  Obs = (Qupstream[Ind_Run - 1] * BasinAreas[1L] +
           BasinObs$Qmm[Ind_Run] * BasinAreas[2L]) / sum(BasinAreas)
)

test_that("Params from calibration with simulated data should be similar to initial params", {
  CalibOptions <- CreateCalibOptions(
    FUN_MOD = RunModel_GR4J,
    FUN_CALIB = Calibration_Michel,
    IsSD = TRUE
  )
  OutputsCalib <- Calibration_Michel(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions,
    FUN_MOD = RunModel_GR4J
  )
  expect_equal(OutputsCalib$ParamFinalR[2:5] / ParamSD[2:5], rep(1, 4), tolerance = 1e-2)
  expect_equal(OutputsCalib$ParamFinalR[1L], ParamSD[1L], tolerance = 2e-3)
})

test_that("Params from calibration with simulated data should be similar to initial params", {
  CalibOptions <- CreateCalibOptions(
    FUN_MOD = RunModel_Lag,
    FUN_CALIB = Calibration_Michel,
    IsSD = FALSE
  )
  OutputsCalib <- Calibration_Michel(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions,
    FUN_MOD = RunModel_Lag,
    QcontribDown = OutputsGR4JOnly
  )
  expect_equal(OutputsCalib$ParamFinalR[1L], ParamSD[1L], tolerance = 2e-3)
})

test_that("1 no area input with lag of 1 time step delay out gives an output delayed of one time step converted to mm", {
  Qm3GR4Only <- OutputsGR4JOnly$Qsim * BasinAreas[2L] * 1e3
  # Specify that upstream flow is not related to an area
  InputsModel$BasinAreas <- c(NA, BasinAreas[2L])
  # Convert upstream flow to m3/day
  InputsModel$Qupstream <- matrix(Qupstream, ncol = 1) * BasinAreas[1L] * 1e3

  OutputsSD <- RunModel(InputsModel, RunOptions, Param = ParamSD, FUN_MOD = RunModel_GR4J)

  expect_false(any(is.na(OutputsSD$Qsim)))

  Qm3SdSim <- OutputsSD$Qsim_m3
  Qm3UpstLagObs <- InputsModel$Qupstream[Ind_Run - 1]

  expect_equal(Qm3SdSim - Qm3GR4Only, Qm3UpstLagObs)
})

# *** IniStates tests ***
IM <- InputsModel
IM$BasinAreas <- rep(BasinInfo$BasinArea, 3)
IM$Qupstream <- cbind(IM$Qupstream, IM$Qupstream)
IM$LengthHydro <- c(1, 1.5)

PSDini <- ParamSD
PSDini[1] <- PSDini[1] / 2 # 2 time step delay
Ind_Run1 <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
                which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-12-31"))
Ind_Run2 <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1991-01-01"),
                which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1991-12-31"))

# 1990
RunOptions1 <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                 InputsModel = IM,
                                                 IndPeriod_Run = Ind_Run1))
OutputsModel1 <- RunModel(InputsModel = IM,
                          RunOptions = RunOptions1, Param = PSDini,
                          FUN_MOD = RunModel_GR4J)
# 1990-1991
RunOptions <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                InputsModel = IM,
                                                IndPeriod_Run = c(Ind_Run1, Ind_Run2)))
OutputsModel <- RunModel(InputsModel = IM,
                         RunOptions = RunOptions,
                         Param = PSDini,
                         FUN_MOD = RunModel_GR4J)

test_that("Warm start should give same result as warmed model", {
  # Warm start 1991
  RunOptions2 <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                  InputsModel = IM, IndPeriod_Run = Ind_Run2,
                                  IndPeriod_WarmUp = 0L,
                                  IniStates = OutputsModel1$StateEnd)
  OutputsModel2 <- RunModel(InputsModel = IM,
                            RunOptions = RunOptions2,
                            Param = PSDini,
                            FUN_MOD = RunModel_GR4J)
  # Compare 1991 Qsim from warm started and from 1990-1991
  names(OutputsModel2$Qsim) <- NULL
  expect_equal(OutputsModel2$Qsim, OutputsModel$Qsim[366:730])
})

test_that("Error on Wrong length of iniState$SD", {
  OutputsModel1$StateEnd$SD[[1]] <- c(1,1)
  RunOptions2 <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                  InputsModel = IM, IndPeriod_Run = Ind_Run2,
                                  IndPeriod_WarmUp = 0L,
                                  IniStates = OutputsModel1$StateEnd)
  expect_error(
    RunModel(InputsModel = IM, RunOptions = RunOptions2, Param = PSDini, FUN_MOD = RunModel_GR4J)
  )
})

test_that("First Qupstream time steps must be repeated if warm-up period is too short", {
  IM2 <- IM[2558:3557]
  IM2$BasinAreas[3] <- 0
  IM2$Qupstream <- matrix(rep(1:1000, 2), ncol = 2)
  RunOptions2 <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                  InputsModel = IM2, IndPeriod_Run = seq_len(1000),
                                  IndPeriod_WarmUp = 0L)
  OM2 <- RunModel(InputsModel = IM2,
                  RunOptions = RunOptions2,
                  Param = PSDini,
                  FUN_MOD = RunModel_GR4J)
  expect_equal(OM2$Qsim_m3[1:3], rep(2,3))
})

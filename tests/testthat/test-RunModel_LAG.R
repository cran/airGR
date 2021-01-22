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

test_that("'Qupstream' cannot contain any NA value", {
  expect_error(
    InputsModel <- CreateInputsModel(
      FUN_MOD = RunModel_GR4J,
      DatesR = BasinObs$DatesR,
      Precip = BasinObs$P,
      PotEvap = BasinObs$E,
      Qupstream = matrix(BasinObs$Qmm, ncol = 1),
      LengthHydro = 1,
      BasinAreas = BasinAreas
    ),
    regexp = "'Qupstream' cannot contain any NA value"
  )
})

# Qupstream = sinusoid synchronised on hydrological year from 0 mm to mean value of Qobs
Qupstream <- floor((sin((seq_along(BasinObs$Qmm)/365*2*3.14))+1) * mean(BasinObs$Qmm, na.rm = TRUE))

InputsModel <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E,
  Qupstream = matrix(Qupstream, ncol = 1),
  LengthHydro = 1000,
  BasinAreas = BasinAreas
)

Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31"))

RunOptions <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel,
                               IndPeriod_Run = Ind_Run))

test_that("InputsModel parameter should contain an OutputsModel key", {
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1),
    regexp = "'InputsModel' should contain an 'OutputsModel' key"
  )
})

Param <- c(257.237556, 1.012237, 88.234673, 2.207958) #  From vignettes/V01_get_started

OutputsGR4JOnly <- RunModel_GR4J(InputsModel = InputsModel,
                                 RunOptions = RunOptions,
                                 Param = Param)

test_that("InputsModel$OutputsModel should contain a Qsim key", {
  InputsModel$OutputsModel <- OutputsGR4JOnly
  InputsModel$OutputsModel$Qsim <- NULL
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1),
    regexp = "should contain a key 'Qsim'"
  )
})

test_that("'InputsModel$OutputsModel$Qim' should have the same lenght as 'RunOptions$IndPeriod_Run'", {
  InputsModel$OutputsModel <- OutputsGR4JOnly
  InputsModel$OutputsModel$Qsim <- c(InputsModel$OutputsModel$Qsim, 0)
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1),
    regexp = "should have the same lenght as"
  )
})

test_that("'InputsModel$OutputsModel$Qim' should contain no NA'", {
  InputsModel$OutputsModel <- OutputsGR4JOnly
  InputsModel$OutputsModel$Qsim[10L] <- NA
  expect_error(
    RunModel_Lag(InputsModel = InputsModel, RunOptions = RunOptions, Param = 1),
    regexp = "contain no NA"
  )
})

test_that("Upstream basin with nil area should return same Qdown as GR4J alone", {
  UpstBasinArea <- InputsModel$BasinAreas[1L]
  InputsModel$BasinAreas[1L] <- 0
  OutputsSD <- RunModel(InputsModel,
                        RunOptions,
                        Param = c(1, Param),
                        FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsGR4JOnly$Qsim, OutputsSD$Qsim)
})

test_that("Downstream basin with nil area and nul upstream length should return same Qdown as Qupstream alone", {
  InputsModel$LengthHydro <- 0
  InputsModel$BasinAreas <- c(BasinInfo$BasinArea, 0)
  OutputsSD <- RunModel(InputsModel,
                        RunOptions,
                        Param = c(1, Param),
                        FUN_MOD = RunModel_GR4J)
  expect_equal(OutputsSD$Qsim, Qupstream[Ind_Run])
})

ParamSD <- c(InputsModel$LengthHydro / (24 * 60 * 60), Param) # Speed corresponding to one time step delay

QlsGR4Only <- OutputsGR4JOnly$Qsim * InputsModel$BasinAreas[2L] * 1e6 / 86400

test_that("1 input with lag of 1 time step delay out gives an output delayed of one time step", {
  OutputsSD <- RunModel(InputsModel, RunOptions, Param = ParamSD, FUN_MOD = RunModel_GR4J)
  QlsSdSim <- OutputsSD$Qsim * sum(InputsModel$BasinAreas) * 1e6 / 86400
  QlsUpstLagObs <- c(0, Qupstream[Ind_Run[1:(length(Ind_Run) - 1)]]) * InputsModel$BasinAreas[1L] * 1e6 / 86400
  expect_equal(QlsSdSim - QlsGR4Only, QlsUpstLagObs)
})

test_that("1 input with lag of 0.5 time step delay out gives an output delayed of 0.5 time step", {
  OutputsSD <- RunModel(InputsModel, RunOptions,
                        Param = c(InputsModel$LengthHydro / (12 * 3600), Param),
                        FUN_MOD = RunModel_GR4J)
  QlsSdSim <- OutputsSD$Qsim * sum(InputsModel$BasinAreas) * 1e6 / 86400
  QlsUpstLagObs <- (Qupstream[Ind_Run] + c(0, Qupstream[Ind_Run[1:(length(Ind_Run) - 1)]]))/2 * InputsModel$BasinAreas[1L] * 1e6 / 86400
  expect_equal(QlsSdSim - QlsGR4Only, QlsUpstLagObs)
})

test_that("Params from calibration with simulated data should be similar to initial params", {
  InputsCrit <- CreateInputsCrit(
    FUN_CRIT = ErrorCrit_NSE,
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    VarObs = "Q",
    Obs = (
      c(0, Qupstream[Ind_Run[1:(length(Ind_Run) - 1)]]) * BasinAreas[1L] +
        BasinObs$Qmm[Ind_Run] * BasinAreas[2L]
    ) / sum(BasinAreas)
  )
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

test_that("1 no area input with lag of 1 time step delay out gives an output delayed of one time step converted to mm", {
  Qm3GR4Only <- OutputsGR4JOnly$Qsim * BasinAreas[2L] * 1e3
  # Specify that upstream flow is not related to an area
  InputsModel$BasinAreas <- c(NA, BasinAreas[2L])
  # Convert upstream flow to m3/day
  InputsModel$Qupstream <- matrix(Qupstream, ncol = 1) * BasinAreas[1L] * 1e3

  OutputsSD <- RunModel(InputsModel, RunOptions, Param = ParamSD, FUN_MOD = RunModel_GR4J)

  expect_false(any(is.na(OutputsSD$Qsim)))

  Qm3SdSim <- OutputsSD$Qsim * sum(InputsModel$BasinAreas, na.rm = TRUE) * 1e3
  Qm3UpstLagObs <- c(0, InputsModel$Qupstream[Ind_Run[1:(length(Ind_Run) - 1)]])

  expect_equal(Qm3SdSim - Qm3GR4Only, Qm3UpstLagObs)
})

# *** IniStates tests ***
IM <- InputsModel
IM$BasinAreas <- rep(BasinInfo$BasinArea, 3)
IM$Qupstream <- cbind(IM$Qupstream, IM$Qupstream)
IM$LengthHydro <- c(1000, 1500)

PSDini <- ParamSD
PSDini[1] <- PSDini[1] / 2 # 2 time step delay
Ind_Run1 <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
                which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-12-31"))
Ind_Run2 <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1991-01-01"),
                which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1991-12-31"))

# 1990
RunOptions1 <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                 InputsModel = IM, IndPeriod_Run = Ind_Run1))
OutputsModel1 <- RunModel(InputsModel = IM,
                          RunOptions = RunOptions1, Param = PSDini, FUN_MOD = RunModel_GR4J)
# 1990-1991
RunOptions <- suppressWarnings(CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                                InputsModel = IM, IndPeriod_Run = c(Ind_Run1, Ind_Run2)))
OutputsModel <- RunModel(InputsModel = IM,
                         RunOptions = RunOptions, Param = PSDini, FUN_MOD = RunModel_GR4J)

test_that("Warm start should give same result as warmed model", {
  # Warm start 1991
  RunOptions2 <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                  InputsModel = IM, IndPeriod_Run = Ind_Run2,
                                  IndPeriod_WarmUp = 0L,
                                  IniStates = OutputsModel1$StateEnd)
  OutputsModel2 <- RunModel(InputsModel = IM,
                                 RunOptions = RunOptions2, Param = PSDini, FUN_MOD = RunModel_GR4J)
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
  expect_error(RunModel(InputsModel = IM, RunOptions = RunOptions2, Param = PSDini, FUN_MOD = RunModel_GR4J)
  )
})

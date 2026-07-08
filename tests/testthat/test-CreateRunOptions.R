context("CreateRunOptions")

data(L0123001)
InputsModel <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E
)
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)

test_that("Warm start of GR4J should give same result as warmed model", {
  Ind_Run1 <- seq(
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-12-31")
  )
  Ind_Run2 <- seq(
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1991-01-01"),
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1991-12-31")
  )
  # 1990-1991
  RunOptions <- suppressWarnings(
    # Test for #204 Intempestive warnings in CreateRunOptions since v1.7.8
    expect_no_warning(
      CreateRunOptions(
        FUN_MOD = RunModel_GR4J,
        InputsModel = InputsModel,
        IndPeriod_Run = c(Ind_Run1, Ind_Run2)
      ),
      message = "does not require .* Value set to NA"
    )
  )

  OutputsModel <- RunModel_GR4J(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Param = Param
  )
  # 1990
  RunOptions1 <- suppressWarnings(CreateRunOptions(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel,
    IndPeriod_Run = Ind_Run1
  ))
  OutputsModel1 <- RunModel_GR4J(
    InputsModel = InputsModel,
    RunOptions = RunOptions1,
    Param = Param
  )
  # Warm start 1991
  RunOptions2 <- CreateRunOptions(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel,
    IndPeriod_Run = Ind_Run2,
    IndPeriod_WarmUp = 0L,
    IniStates = OutputsModel1$StateEnd
  )
  OutputsModel2 <- RunModel_GR4J(
    InputsModel = InputsModel,
    RunOptions = RunOptions2,
    Param = Param
  )
  # Compare 1991 Qsim from warm started and from 1990-1991
  expect_equal(OutputsModel2$Qsim, OutputsModel$Qsim[366:730])
})

test_that("CreateRunOptions works with Outputs_Sim containing 'WarmUpQsim'", {
  Ind_Run <- seq(
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-12-31")
  )
  RunOptions <- suppressWarnings(CreateRunOptions(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel,
    IndPeriod_Run = Ind_Run,
    Outputs_Sim = c("WarmUpQsim", "Qsim")
  ))
  OutputsModel <- RunModel(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Param = Param
  )
  expect_equal(names(OutputsModel), c("Qsim", "RunOptions"))
  expect_equal(names(OutputsModel$RunOptions), c("WarmUpQsim"))
  expect_length(OutputsModel$RunOptions$WarmUpQsim, length(RunOptions$IndPeriod_WarmUp))
})

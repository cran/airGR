context("CreateInputsCrit")

data(L0123001)
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))
RunOptions <- suppressWarnings(
  CreateRunOptions(FUN_MOD = RunModel_GR4J,
                   InputsModel = InputsModel, IndPeriod_Run = Ind_Run)
)
Param <- c(X1 = 257.238, X2 = 1.012, X3 = 88.235, X4 = 2.208)
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)
test_that("KGE crit with log transform should return a warning", {
  expect_warning(
    CreateInputsCrit(
      FUN_CRIT = ErrorCrit_KGE,
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = BasinObs$Qmm[Ind_Run],
      transfo = "log"
    ),
    regex = "we do not advise using the KGE with a log transformation on Obs"
  )
})


test_that("Composed crit with two identical should return a warning", {
  expect_warning(
    CreateInputsCrit(
      FUN_CRIT = list(ErrorCrit_KGE, ErrorCrit_KGE),
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = list(BasinObs$Qmm[Ind_Run], BasinObs$Qmm[Ind_Run]),
      VarObs = list("Q", "Q")
    ),
    regex = "the criteria list are identical"
  )
})

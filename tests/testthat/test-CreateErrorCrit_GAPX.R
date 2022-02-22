context("CreateErrorCrit_GAPX")

test_that("Function should return ErrorCrit function", {
  expect_equal(class(CreateErrorCrit_GAPX(TransfoParam_GR1A)), c("FUN_CRIT", "function"))
})

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

ErrorCrit_GAPX <- CreateErrorCrit_GAPX(TransfoParam_GR4J)
ParamT <- TransfoParam_GR4J(Param, "RT")

test_that("ErrorCrit should return 1 for same parameters", {
  IC <- CreateInputsCrit(ErrorCrit_GAPX, InputsModel, RunOptions, Obs = ParamT, VarObs = "ParamT")
  expect_equal(ErrorCrit_GAPX(IC, OutputsModel)$CritValue, 1)
})

test_that("ErrorCrit should return 1-nbParam^0.5/40 for ParamT shifted by 1", {
  ParamT <- ParamT + 1
  IC <- CreateInputsCrit(ErrorCrit_GAPX, InputsModel, RunOptions, Obs = ParamT, VarObs = "ParamT")
  expect_equal(ErrorCrit_GAPX(IC, OutputsModel)$CritValue,
               1 - RunOptions$FeatFUN_MOD$NbParam^0.5 / 20)
})

test_that("ErrorCrit should return 1-(nbParam-1)^0.5/40 for ParamT shifted by 1 with one NA", {
  ParamT <- ParamT + 1
  ParamT[1] <- NA
  IC <- CreateInputsCrit(ErrorCrit_GAPX, InputsModel, RunOptions, Obs = ParamT, VarObs = "ParamT")
  expect_equal(suppressWarnings(ErrorCrit_GAPX(IC, OutputsModel)$CritValue),
               1 - (RunOptions$FeatFUN_MOD$NbParam - 1)^0.5 / 20)
  expect_warning(ErrorCrit_GAPX(IC, OutputsModel)$CritValue,
                 regexp = "criterion GAPX computed on less than 4 parameters")
})

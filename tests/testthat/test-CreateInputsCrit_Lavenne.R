context("CreateInputsCrit_Lavenne")

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

k <- 0.15
IC_DL <- CreateInputsCrit_Lavenne(FUN_CRIT = ErrorCrit_KGE,
                                InputsModel = InputsModel,
                                RunOptions = RunOptions,
                                Obs = BasinObs$Qmm[Ind_Run],
                                AprParamR = Param,
                                k = k)

test_that("should return a composed ErrorCrit", {
  expect_s3_class(IC_DL, "Compo")
  expect_length(IC_DL, 2)
  AprParamT <- TransfoParam_GR4J(Param, "RT")
  expect_equal(IC_DL$IC2$Obs, AprParamT)
})

test_that("should return KGE*(1-k)+k with parameters matching a priori parameters", {
  IC_KGE <- CreateInputsCrit(ErrorCrit_KGE,
                             InputsModel = InputsModel,
                             RunOptions = RunOptions,
                             Obs = BasinObs$Qmm[Ind_Run],
                             transfo = "sqrt")
  expect_equal(ErrorCrit_KGE(IC_KGE, OutputsModel)$CritValue * (1 - k) + k,
               ErrorCrit(IC_DL, OutputsModel)$CritValue)
})

test_that("should return proper error if mismatch number of parameters", {
  expect_error(
    CreateInputsCrit_Lavenne(FUN_CRIT = ErrorCrit_KGE,
                               InputsModel = InputsModel,
                               RunOptions = RunOptions,
                               Obs = BasinObs$Qmm[Ind_Run],
                               AprParamR = Param[-1],
                               k = k),
    regex = "'AprParamR' must be a numeric vector of length 4"
  )
})

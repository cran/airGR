test_that("Imax works", {
## loading catchment data
data(L0123003)

## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5H, DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)

## run period selection
Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d %H")=="2006-01-01 00"),
               which(format(BasinObs$DatesR, format = "%Y-%m-%d %H")=="2006-12-31 23"))

## Imax computation
Imax <- Imax(InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
             TestedValues = seq(from = 0, to = 3, by = 0.2))

## preparation of the RunOptions object
RunOptions <- suppressWarnings(
  CreateRunOptions(FUN_MOD = RunModel_GR5H, Imax = Imax,
                   InputsModel = InputsModel, IndPeriod_Run = Ind_Run)
)

expect_equal(RunOptions$Imax, Imax)

## simulation
Param <- c(X1 = 706.912, X2 = -0.163, X3 = 188.880, X4 = 2.575, X5 = 0.104)
OutputsModel <- RunModel_GR5H(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

expect_true(!is.na(OutputsModel$StateEnd$Store$Int))
})

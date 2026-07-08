test_that("IniStates is taken into account in CreateRunOptions", {
  ## loading catchment data
  data(L0123001)

  # Conversion of Inputs to monthly time step
  Inputs <- SeriesAggreg(
    data.frame(DatesR = BasinObs$DatesR, P = BasinObs$P, E = BasinObs$E),
    Format = "%Y%m",
    ConvertFun = c("sum", "sum")
  )

  ## preparation of the InputsModel object with daily time step data
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR2M,
    DatesR = Inputs$DatesR,
    Precip = Inputs$P,
    PotEvap = Inputs$E
  )

  ## run period selection
  Ind_Run <- seq(
    which(format(InputsModel$DatesR, format = "%Y-%m") == "1990-01"),
    which(format(InputsModel$DatesR, format = "%Y-%m") == "1999-12")
  )

  Param <- c(X1 = 265.072, X2 = 1.040)

  RO <- CreateRunOptions(
    FUN_MOD = RunModel_GR2M,
    InputsModel = InputsModel,
    IndPeriod_Run = Ind_Run,
    IndPeriod_WarmUp = 0L,
    IniStates = CreateIniStates(
      FUN_MOD = RunModel_GR2M,
      InputsModel = InputsModel,
      ProdStore = 200,
      RoutStore = 50
    )
  )

  expect_equal(RO$IniStates[1:2], c(Store.Prod = 200, Store.Rout = 50))
})

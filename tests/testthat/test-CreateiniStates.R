context("CreateIniStates on SD model")

data(L0123001)

test_that("Error: SD argument provided on non-SD 'InputsModel'", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )
  expect_error(
    IniStates <- CreateIniStates(
      FUN_MOD = RunModel_GR4J,
      InputsModel = InputsModel,
      ProdStore = 0,
      RoutStore = 0,
      ExpStore = NULL,
      UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
      UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
      SD = list(rep(0, 10))
    ),
    regexp = "'SD' argument provided and"
  )
})

BasinAreas <- c(BasinInfo$BasinArea, BasinInfo$BasinArea)

# Qupstream = sinusoid synchronised on hydrological year from 0 mm to mean value of Qobs
Qupstream <- floor((sin((
  seq_along(BasinObs$Qmm) / 365 * 2 * 3.14
)) + 1) * mean(BasinObs$Qmm, na.rm = TRUE))

InputsModel <- CreateInputsModel(
  FUN_MOD = RunModel_GR4J,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E,
  Qupstream = matrix(Qupstream, ncol = 1),
  LengthHydro = 1000,
  BasinAreas = BasinAreas
)

test_that("Error: Non-list 'SD' argument", {
  expect_error(
    IniStates <- CreateIniStates(
      FUN_MOD = RunModel_GR4J,
      InputsModel = InputsModel,
      ProdStore = 0,
      RoutStore = 0,
      ExpStore = NULL,
      UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
      UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
      SD = rep(0, 10)
    ),
    regexp = "'SD' argument must be a list"
  )
})

test_that("Error: Non-numeric items in 'SD' list argument", {
  lapply(list(list(list(rep(
    0, 10
  ))), list(toto = NULL)),
  function(x) {
    expect_error(
      IniStates <- CreateIniStates(
        FUN_MOD = RunModel_GR4J,
        InputsModel = InputsModel,
        ProdStore = 0,
        RoutStore = 0,
        ExpStore = NULL,
        UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
        UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
        SD = x
      ),
      regexp = "Each item of 'SD' list argument must be numeric"
    )
  })
})

test_that("Error: Number of items not equal to number of upstream connections", {
  lapply(list(list(), list(rep(0, 10), rep(0, 10))),
         function(x) {
           expect_error(
             IniStates <- CreateIniStates(
               FUN_MOD = RunModel_GR4J,
               InputsModel = InputsModel,
               ProdStore = 0,
               RoutStore = 0,
               ExpStore = NULL,
               UH1 = c(0.52, 0.54, 0.15, rep(0, 17)),
               UH2 = c(0.057, 0.042, 0.015, 0.005, rep(0, 36)),
               SD = x
             ),
             regexp = "list argument must be the same as the number of upstream"
           )
         })
})

test_that("FUN = RunModel_lag must work", {
  IS <- CreateIniStates(RunModel_Lag, InputsModel, SD = list(rep(0, 10)))
  expect_equal(IS$SD[[1]], rep(0, 10))
  expect_s3_class(IS, "SD")
})

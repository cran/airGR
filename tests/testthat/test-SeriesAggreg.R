context("SeriesAggreg")

## load catchment data
data(L0123002)

# Test removed because of #171 to reintegrated later...
# test_that("No warning with InputsModel Cemaneige'", {
#   ## preparation of the InputsModel object
#   InputsModel <- CreateInputsModel(
#     FUN_MOD = RunModel_CemaNeige,
#     DatesR = BasinObs$DatesR,
#     Precip = BasinObs$P,
#     TempMean = BasinObs$T,
#     ZInputs = BasinInfo$HypsoData[51],
#     HypsoData = BasinInfo$HypsoData,
#     NLayers = 5
#   )
#   # Expect no warning: https://stackoverflow.com/a/33638939/5300212
#   expect_warning(SeriesAggreg(InputsModel, Format = "%m"),
#                  regexp = NA)
# })


test_that("Warning: deprecated 'TimeFormat' argument", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )
  expect_warning(SeriesAggreg(InputsModel, Format = "%Y%m", TimeFormat = "daily"),
                 regexp = "deprecated 'TimeFormat' argument")
})


test_that("Warning: deprecated 'NewTimeFormat' argument: please use 'Format' instead",
          {
            InputsModel <- CreateInputsModel(
              FUN_MOD = RunModel_GR4J,
              DatesR = BasinObs$DatesR,
              Precip = BasinObs$P,
              PotEvap = BasinObs$E
            )
            expect_warning(SeriesAggreg(InputsModel, NewTimeFormat = "monthly"),
                           regexp = "deprecated 'NewTimeFormat' argument: please use 'Format' instead")
          })


test_that("Warning: deprecated 'NewTimeFormat' argument: 'Format' argument is used instead",
          {
            InputsModel <- CreateInputsModel(
              FUN_MOD = RunModel_GR4J,
              DatesR = BasinObs$DatesR,
              Precip = BasinObs$P,
              PotEvap = BasinObs$E
            )
            expect_warning(SeriesAggreg(InputsModel, Format = "%Y%m", NewTimeFormat = "monthly"),
                           regexp = "deprecated 'NewTimeFormat' argument: 'Format' argument is used instead")
          })


test_that("Check SeriesAggreg output values on yearly aggregation", {
  TabSeries <- data.frame(
    DatesR = BasinObs$DatesR,
    P = BasinObs$P,
    E = BasinObs$E,
    Qmm = BasinObs$Qmm
  )
  GoodValues <- apply(BasinObs[BasinObs$DatesR >= as.POSIXct("1984-09-01", tz = "UTC") &
                                 BasinObs$DatesR < as.POSIXct("1985-09-01", tz = "UTC"),
                               c("P", "E", "Qmm")],
                      MARGIN = 2, FUN = sum)
  TestedValues <- unlist(SeriesAggreg(TabSeries,
                                      Format = "%Y",
                                      YearFirstMonth = 9,
                                      ConvertFun = rep("sum", 3))[1, c("P", "E", "Qmm")])
  expect_equal(GoodValues, TestedValues)
})


test_that("Regime calculation should switch ConvertFun to 'mean' for InputsModel", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )
  expect_equal(SeriesAggreg(InputsModel, Format = "%m")$Precip,
               SeriesAggreg(BasinObs[, c("DatesR", "P")], Format = "%m", ConvertFun = "mean")$P)
})


test_that("No DatesR should warning", {
  TabSeries <- list(
    Dates = BasinObs$DatesR,
    P = BasinObs$P,
    E = BasinObs$E,
    Qmm = BasinObs$Qmm
  )
  expect_warning(
    SeriesAggreg(TabSeries, Format = "%Y%m", ConvertFun = "sum"),
    regexp = "has been automatically chosen"
  )
})


test_that("Check SeriesAggreg.list 'DatesR' argument", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )
  DatesR <- InputsModel$DatesR
  # No InputsModel$DatesR
  InputsModel$DatesR <- NULL
  expect_error(SeriesAggreg(InputsModel, Format = "%Y%m"), regexp = "'POSIXt'")
  # Other list item chosen
  InputsModel$SuperDates <- DatesR
  expect_warning(SeriesAggreg(InputsModel, Format = "%Y%m"), regexp = "SuperDates")
  # Wrong InputsModel$DatesR
  InputsModel$DatesR <- BasinObs$P
  expect_error(SeriesAggreg(InputsModel, Format = "%Y%m"), regexp = "'POSIXt'")

})


test_that("Check SeriesAggreg.list with embedded lists", {
  InputsModel <-
    CreateInputsModel(
      FUN_MOD = RunModel_CemaNeige,
      DatesR = BasinObs$DatesR,
      Precip = BasinObs$P,
      TempMean = BasinObs$T,
      ZInputs = BasinInfo$HypsoData[51],
      HypsoData = BasinInfo$HypsoData,
      NLayers = 5
    )
  I2 <- SeriesAggreg(InputsModel, Format = "%Y%m")
  expect_equal(length(I2$ZLayers), 5)
  expect_null(I2$LayerPrecip$DatesR)
  expect_equal(length(I2$DatesR), length(I2$LayerPrecip$L1))
})


test_that("Check SeriesAggreg.outputsModel", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_CemaNeigeGR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    TempMean = BasinObs$T,
    ZInputs = median(BasinInfo$HypsoData),
    HypsoData = BasinInfo$HypsoData,
    NLayers = 5
  )

  ## run period selection
  Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
                 which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31"))

  ## preparation of the RunOptions object
  suppressWarnings(
    RunOptions <- CreateRunOptions(
      FUN_MOD = RunModel_CemaNeigeGR4J,
      InputsModel = InputsModel,
      IndPeriod_Run = Ind_Run
    )
  )

  ## simulation
  Param <- c(
    X1 = 408.774,
    X2 = 2.646,
    X3 = 131.264,
    X4 = 1.174,
    CNX1 = 0.962,
    CNX2 = 2.249
  )
  OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel,
                                         RunOptions = RunOptions,
                                         Param = Param)

  O2 <- SeriesAggreg(OutputsModel, Format = "%Y%m")
  expect_equal(length(O2$StateEnd), 3)
  expect_equal(length(O2$DatesR),
               length(O2$CemaNeigeLayers$Layer01$Pliq))
})


test_that("Check data.frame handling in SeriesAggreg.list", {
  InputsModelDown1 <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E,
    Qupstream = matrix(BasinObs$Qmm, ncol = 1),
    # Upstream observed flow
    LengthHydro = 100,
    # Distance between upstream catchment outlet and the downstream one in km
    BasinAreas = c(180, 180) # Upstream and downstream areas in km²
  )
  # Test removed because of #171 to reintegrated later...
  # expect_warning(SeriesAggreg(InputsModelDown1, Format = "%Y%m"),
  #                regexp = NA)
  I2 <- SeriesAggreg(InputsModelDown1, Format = "%Y%m")
  expect_equal(length(I2$DatesR), nrow(I2$Qupstream))
  InputsModelDown1$Qupstream <- InputsModelDown1$Qupstream[-1, , drop = FALSE]
  expect_warning(SeriesAggreg(InputsModelDown1, Format = "%Y%m"),
                 regexp = "it will be ignored in the aggregation")
})


test_that("SeriesAggreg from and to the same time step should return initial time series", {
  InputsModel <- CreateInputsModel(
    FUN_MOD = RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )
  I2 <- SeriesAggreg(InputsModel, Format = "%Y%m")
  expect_warning(SeriesAggreg(I2, Format = "%Y%m"), regexp = "No time-step conversion was performed")
  expect_equal(I2, suppressWarnings(SeriesAggreg(I2, Format = "%Y%m")))
})


test_that("SeriesAggreg.data.frame with first column not named DatesR should work", {
            expect_warning(SeriesAggreg(
              data.frame(BasinObs$DatesR, BasinObs$Qmm),
              Format = "%Y%m",
              ConvertFun = "sum"
            ),
            regexp = NA)
})


test_that("SeriesAggreg should work with ConvertFun 'min', 'max' and 'median'", {
  Qls <- BasinObs[, c("DatesR", "Qls")]
  test_ConvertFunRegime <- function(x, ConvertFun, Format) {
    expect_equal(nrow(SeriesAggreg(x, Format, ConvertFun = ConvertFun)),
                 length(unique(format(BasinObs$DatesR, format = "%Y"))))
  }
  lapply(c("max", "min", "median"), function(x) {test_ConvertFunRegime(Qls, x, Format = "%Y")})
})


test_that("Error on convertFun Q without 0-100", {
  Qls <- BasinObs[, c("DatesR", "Qls")]
  expect_error(SeriesAggreg(Qls, Format = "%Y", "q101"))
  expect_error(SeriesAggreg(Qls, Format = "%Y", "q-2"))
  expect_error(SeriesAggreg(Qls, Format = "%Y", "q12.5"))
})


test_that("ConvertFun q50 should be equal to median", {
  Qls <- BasinObs[, c("DatesR", "Qls")]
  expect_equal(SeriesAggreg(Qls, Format = "%Y", "q50"),
               SeriesAggreg(Qls, Format = "%Y", "median"))
  expect_equal(SeriesAggreg(Qls, Format = "%Y", "q50"),
               SeriesAggreg(Qls, Format = "%Y", "q050"))
})


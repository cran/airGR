context("Calibration")

# helpers functions and dfModels variable are in tests/testthat/helper_scheduled_Calibration.R



TestModelCalibration <- function(model) {
  model <- as.list(model)

  test_that(paste(model$name, ifelse(as.logical(model$IsHyst), "Hysteresis", ""), "works"), {

    ParamFinalR <- ModelCalibration(model)

    expect_equal(ParamFinalR,
                 as.numeric(strsplit(model$ParamFinalR, ";")[[1]]),
                 tolerance = 1E-6)
  })
}

apply(dfModels, 1, TestModelCalibration)

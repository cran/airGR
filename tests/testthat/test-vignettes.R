context("Test vignette chunks")

test_that("V01_get_started works", {
  skip_on_cran()
  rm(list = ls())
  expect_true(RunVignetteChunks("V01_get_started"))
  TestQmmQlsConversion(BasinObs, BasinInfo$BasinArea)
})

test_that("V02.1_param_optim works", {
  skip_on_cran()
  rm(list = ls())
  load(system.file("vignettesData/vignetteParamOptim.rda", package = "airGR"))
  load(system.file("vignettesData/vignetteParamOptimCaramel.rda", package = "airGR"))
  rda_resGLOB <- resGLOB
  rda_resPORT <- resPORT
  rda_optMO <- optMO
  expect_true(RunVignetteChunks("V02.1_param_optim"))
  expect_equal(summary(resGLOB), summary(rda_resGLOB), tolerance = 1e-7)
  expect_equal(resGLOB[, -1], rda_resGLOB[, -1], tolerance = 1e-2) # High tolerance due to randomisation in optimisations
  expect_equal(summary(optMO$parameters), summary(rda_optMO$parameters), tolerance = 1e-7)
})

test_that("V02.2_param_mcmc works", {
  skip_on_cran()
  rm(list = ls())
  load(system.file("vignettesData/vignetteParamMCMC.rda", package = "airGR"))
  rda_gelRub <- gelRub
  rda_multDRAM <- multDRAM
  expect_true(RunVignetteChunks("V02.2_param_mcmc"))
  expect_equal(gelRub, rda_gelRub, tolerance = 1e-7)
  expect_equal(multDRAM, rda_multDRAM, tolerance = 1e-7)
})

test_that("V03_param_sets_GR4J works", {
  skip_on_cran()
  rm(list = ls())
  expect_true(RunVignetteChunks("V03_param_sets_GR4J"))
})

test_that("V04_cemaneige_hysteresis works", {
  skip_on_cran()
  rm(list = ls())
  load(system.file("vignettesData/vignetteCNHysteresis.rda", package = "airGR"))
  rda_OutputsCrit_Cal <- OutputsCrit_Cal
  rda_OutputsCrit_Cal_NoHyst <- OutputsCrit_Cal_NoHyst
  rda_OutputsCrit_Val <- OutputsCrit_Val
  rda_OutputsCrit_Val_NoHyst <- OutputsCrit_Val_NoHyst
  expect_true(RunVignetteChunks("V04_cemaneige_hysteresis"))
  TestQmmQlsConversion(BasinObs, BasinInfo$BasinArea)
  expect_equal(OutputsCrit_Cal, rda_OutputsCrit_Cal, tolerance = 1e-7)
  expect_equal(OutputsCrit_Cal_NoHyst, rda_OutputsCrit_Cal_NoHyst, tolerance = 1e-7)
  expect_equal(OutputsCrit_Val, rda_OutputsCrit_Val, tolerance = 1e-7)
  expect_equal(OutputsCrit_Val_NoHyst, rda_OutputsCrit_Val_NoHyst, tolerance = 1e-7)
})

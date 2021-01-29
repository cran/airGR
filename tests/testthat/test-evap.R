context("Test evaporation")

comp_evap <- function(BasinObs,
                      Lat, LatUnit,
                      TimeStepIn = "daily",
                      TimeStepOut = "daily") {
  PotEvap <- PE_Oudin(JD = as.POSIXlt(BasinObs$DatesR)$yday + 1,
                      Temp = BasinObs$T,
                      Lat = Lat, LatUnit = LatUnit,
                      TimeStepIn = TimeStepIn, TimeStepOut = TimeStepOut)
  PotEvapFor <- PE_Oudin(JD = as.POSIXlt(BasinObs$DatesR)$yday + 1,
                         Temp = BasinObs$T,
                         Lat = Lat, LatUnit = LatUnit,
                         TimeStepIn = TimeStepIn, TimeStepOut = TimeStepOut,
                         RunFortran = TRUE)
  all(range(PotEvap - PotEvapFor) < 0.000001)
}

test_that("PE_Oudin works", {
  skip_on_cran()
  rm(list = ls())

  data(L0123001); BasinObs_L0123001 <- BasinObs
  data(L0123002); BasinObs_L0123002 <- BasinObs

  expect_true(comp_evap(BasinObs = BasinObs_L0123001,
                        Lat = 0.8, LatUnit = "rad",
                        TimeStepIn = "daily", TimeStepOut = "daily"))
  expect_true(comp_evap(BasinObs = BasinObs_L0123001,
                        Lat = 0.8, LatUnit = "rad",
                        TimeStepIn = "daily", TimeStepOut = "hourly"))
  expect_true(comp_evap(BasinObs = BasinObs_L0123002,
                        Lat = 0.9, LatUnit = "rad",
                        TimeStepIn = "daily", TimeStepOut = "daily"))
  expect_true(comp_evap(BasinObs = BasinObs_L0123002,
                        Lat = 0.9, LatUnit = "rad",
                        TimeStepIn = "daily", TimeStepOut = "hourly"))

  ## check with several catchments using different values for Lat

  ## one by one
  PotEvapFor1 <- PE_Oudin(JD = as.POSIXlt(BasinObs_L0123001$DatesR)$yday + 1,
                          Temp = BasinObs_L0123001$T,
                          Lat = 0.8, LatUnit = "rad",
                          RunFortran = TRUE)
  PotEvapFor2 <- PE_Oudin(JD = as.POSIXlt(BasinObs_L0123002$DatesR)$yday + 1,
                          Temp = BasinObs_L0123002$T,
                          Lat = 0.9, LatUnit = "rad",
                          RunFortran = TRUE)

  ## all in one
  BasinObs_L0123001$Lat <- 0.8
  BasinObs_L0123002$Lat <- 0.9
  BasinObs <- rbind(BasinObs_L0123001, BasinObs_L0123002)
  PotEvapFor <- PE_Oudin(JD = as.POSIXlt(BasinObs$DatesR)$yday + 1,
                         Temp = BasinObs$T,
                         Lat = BasinObs$Lat, LatUnit = "rad",
                         RunFortran = TRUE)

  expect_equal(PotEvapFor, c(PotEvapFor1, PotEvapFor2))
})

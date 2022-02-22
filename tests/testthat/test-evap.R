context("Test evaporation")


rm(list = ls())
data(L0123001); BasinObs_L0123001 <- BasinObs
data(L0123002); BasinObs_L0123002 <- BasinObs
data(L0123003); BasinObs_L0123003 <- BasinObs

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

test_that("Inconsitent time series", {
  skip_on_cran()

  msgDaily <- "each day should have only one identical value of Julian days. The time series is not sorted, or contains duplicate or missing dates"
  msgHoury <- "each day must have 24 identical values of Julian days (one for each hour). The time series is not sorted, or contains duplicate or missing dates"

  # duplicated dates
  DatesFor1Dupl <- BasinObs_L0123001$DatesR
  DatesFor1Dupl[5L] <- DatesFor1Dupl[4L]
  expect_warning(object = PE_Oudin(JD = as.POSIXlt(DatesFor1Dupl)$yday + 1,
                                   Temp = BasinObs_L0123001$T,
                                   Lat = 0.8, LatUnit = "rad"),
                 regexp = msgDaily)

  # not ordered daily dates
  DatesFor1Messy <- sample(BasinObs_L0123001$DatesR)
  expect_warning(object = PE_Oudin(JD = as.POSIXlt(DatesFor1Messy)$yday + 1,
                                   Temp = BasinObs_L0123001$T,
                                   Lat = 0.8, LatUnit = "rad"),
                 regexp = msgDaily)

  # not ordered hourly dates
  DatesFor3Messy <- sample(BasinObs_L0123003$DatesR)
  expect_error(object = PE_Oudin(JD = as.POSIXlt(DatesFor3Messy)$yday + 1,
                                   Temp = seq_along(BasinObs_L0123003$T),
                                   Lat = 0.8, LatUnit = "rad", TimeStepIn = "hourly"),
                 regexp = msgHoury, fixed = TRUE)


})



# Calibrations on v1.6.12
sModelCalibrations <- c(
  "name          IsHyst data     aggreg ParamFinalR",
  "GR1A          FALSE  L0123001 %Y    9.112500e-01",
  "GR2M          FALSE  L0123001 %Y%m  2.598228e+02;9.975000e-01",
  "GR4J          FALSE  L0123001 NA    2.236316e+02;5.781516e-01;9.751439e+01;2.217718e+00",
  "GR5J          FALSE  L0123001 NA    2.203864e+02;8.944531e-01;9.356407e+01;1.762872e+00;4.846427e-01",
  "GR6J          FALSE  L0123001 NA    1.928762e+02;6.933087e-01;4.917833e+01;2.214542e+00;5.088240e-01;6.814626e+00",
  "CemaNeigeGR4J FALSE  L0123001 NA    2.043839e+02;5.781516e-01;1.025141e+02;2.217718e+00;0.000000e+00;1.490479e+01",
  "CemaNeigeGR5J FALSE  L0123001 NA    1.983434e+02;8.747758e-01;9.849443e+01;1.768769e+00;4.824825e-01;2.002002e-02;1.505459e+01",
  "CemaNeigeGR6J FALSE  L0123001 NA    1.830941e+02;5.551637e-01;6.034029e+01;2.217718e+00;4.760000e-01;6.049647e+00;2.002002e-02;1.520589e+01",
  "CemaNeigeGR4J TRUE   L0123001 NA    2.085127e+02;5.781516e-01;1.025141e+02;2.227477e+00;2.252252e-02;8.599316e+00;1.345000e+01;1.000000e+00",
  "CemaNeigeGR5J TRUE   L0123001 NA    2.023502e+02;9.015250e-01;9.849443e+01;1.788288e+00;4.834835e-01;2.252252e-02;8.599316e+00;1.345000e+01;1.000000e+00",
  "CemaNeigeGR6J TRUE   L0123001 NA    1.886701e+02;5.666293e-01;6.034029e+01;2.227477e+00;4.760000e-01;5.989452e+00;2.052052e-02;8.599316e+00;1.220000e+01;1.000000e+00",
  "GR4H            FALSE  L0123003 NA     7.116766e+02;-1.158469e+00;1.505561e+02;4.686093e+00",
  "GR5H            FALSE  L0123003 NA     8.040022e+02;-1.898488e-01;1.377525e+02;3.043663e+00;1.951163e-01",
  "CemaNeigeGR4H   FALSE  L0123003 NA     1.581284e+03;-8.457959e-01;2.299844e+02;5.000000e-01;9.475779e-03;9.482445e+01",
  "CemaNeigeGR5H   FALSE  L0123003 NA     3.267232e+01;-5.092029e+00;3.384799e+02;1.578534e+00;2.074272e-01;1.501502e-03;4.369420e+00",
  "CemaNeigeGR4H   TRUE   L0123003 NA     1.746044e+03;-7.052756e-01;2.228887e+02;3.377089e+00;0.000000e+00;5.116962e+01;1.204664e+01;5.052849e-01",
  "CemaNeigeGR5H   TRUE   L0123003 NA     6.717382e+01;-1.522839e+00;1.393246e+02;2.493137e+00;2.333041e-01;1.216408e-03;3.328200e-01;5.369605e+01;9.800789e-01"
)

dfModels <- read.table(text = paste(sModelCalibrations, collapse = "\n"), header = TRUE)

PrepareCalibration <- function(model) {
  model <- as.list(model)
  sModel <- paste0("RunModel_", model$name)
  sIM_FUN_MOD <- sModel

  if (model$data == "L0123003") {
    # hourly time step database
    dates <- c("2004-01-01 00:00", "2004-12-31 23:00", "2005-01-01 00:00", "2008-12-31 23:00")
    date_format = "%Y-%m-%d %H:%M"
    TempMean <- fakeHourlyTemp()
  } else {
    # yearly, monthly, daily time step databases
    dates <- c("1985-01-01", "1985-12-31", "1986-01-01", "2012-12-31")
    date_format <- "%Y-%m-%d"
    if (!is.na(model$aggreg)) {
      # Aggregation on monthly and yearly databases
      sIM_FUN_MOD <- "RunModel_GR4J" # CreateInputsModel with daily data
      date_format <- model$aggreg
    }
  }

  ## loading catchment data
  data(list = model$data)
  if (model$data != "L0123003") TempMean <- BasinObs$T

  # preparation of the InputsModel object
  InputsModel <- CreateInputsModel(FUN_MOD = sIM_FUN_MOD,
                                   DatesR = BasinObs$DatesR,
                                   Precip = BasinObs$P,
                                   PotEvap = BasinObs$E,
                                   TempMean = TempMean,
                                   ZInputs = median(BasinInfo$HypsoData),
                                   HypsoData = BasinInfo$HypsoData,
                                   NLayers = 5)

  if (!is.na(model$aggreg)) {
    # conversion of InputsModel to target time step
    InputsModel <- SeriesAggreg(InputsModel, Format = model$aggreg)

    dfQobs <- SeriesAggreg(data.frame(DatesR = BasinObs$DatesR, Qmm = BasinObs$Qmm),
                           Format = model$aggreg, ConvertFun = "sum")
    Obs <- dfQobs$Qmm
  } else {
    Obs <- BasinObs$Qmm
  }

  # calibration period selection
  dates <- sapply(dates, function(x) format(as.Date(x), format = date_format))
  Ind_WarmUp <- seq(
    which(format(InputsModel$DatesR, format = date_format)==dates[1]),
    which(format(InputsModel$DatesR, format = date_format)==dates[2])
  )
  Ind_Run <- seq(
    which(format(InputsModel$DatesR, format = date_format)==dates[3]),
    which(format(InputsModel$DatesR, format = date_format)==dates[4])
  )

  # preparation of the RunOptions object
  suppressWarnings(
    RunOptions <- CreateRunOptions(
      FUN_MOD = sModel,
      InputsModel = InputsModel,
      IndPeriod_Run = Ind_Run,
      IndPeriod_WarmUp = Ind_WarmUp,
      IsHyst = as.logical(model$IsHyst)
    )
  )

  # calibration criterion: preparation of the InputsCrit object
  InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                                 RunOptions = RunOptions, Obs = Obs[Ind_Run])
  # preparation of CalibOptions object
  CalibOptions <- CreateCalibOptions(sModel, IsHyst = as.logical(model$IsHyst))

  return(environment())

}

ModelCalibration <- function(model) {

  e <- PrepareCalibration(model)
  for(n in ls(e, all.names=TRUE)) assign(n, get(n, e))

  # calibration
  suppressWarnings(OutputsCalib <- Calibration(InputsModel = InputsModel, RunOptions = RunOptions,
                                               InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                               FUN_MOD = sModel))
  OutputsCalib$ParamFinalR
}

#' Create Fake hourly temperature from daily temperatures in L0123001
#'
#' @param start_date [character] start date in format "%Y-%m-%d"
#' @param end_date [character] end date in format "%Y-%m-%d"
#' @return [numeric] hourly temperature time series between `start_date` and `end_date`
fakeHourlyTemp <- function(start_date = "2004-01-01", end_date = "2008-12-31") {
  dates <- as.POSIXct(c(start_date, end_date), tz = "UTC")
  data(L0123002)
  indJ <- seq.int(which(BasinObs$DatesR == as.POSIXct(dates[1])),
                  which(BasinObs$DatesR == as.POSIXct(dates[2])))
  TJ <- BasinObs$T[indJ]

  TH <- approx((seq.int(length(TJ)) - 1) * 24,TJ,
               seq.int(length(TJ) * 24 ) - 1,
               rule = 2)$y
  varT_1J <- -sin(0:23/24 * 2 * pi) # Temp min at 6 and max at 18
  varT <- rep(varT_1J, length(TJ))
  TH <- TH + varT * 5 # For a mean daily amplitude of 10°
  TH
}

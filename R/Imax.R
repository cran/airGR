
Imax <- function(InputsModel,
                 IndPeriod_Run,
                 TestedValues = seq(from = 0.1, to = 3, by = 0.1)) {


  ## ---------- check arguments

  ##  InputsModel
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(InputsModel, "hourly")) {
    stop("'InputsModel' must be of class 'hourly'")
  }

  ## IndPeriod_Run
  if (!is.vector(IndPeriod_Run)) {
    stop("'IndPeriod_Run' must be a vector of numeric values")
  }
  if (!inherits(IndPeriod_Run, "integer")) {
    stop("'IndPeriod_Run' must be of type integer")
  }
  if (!identical(as.integer(IndPeriod_Run), IndPeriod_Run[1]:IndPeriod_Run[length(IndPeriod_Run)])) {
    stop("'IndPeriod_Run' must be a continuous sequence of integers")
  }

  ## TestedValues
  if (!(is.numeric(TestedValues))) {
    stop("'TestedValues' must be 'numeric'")
  }


  ## ---------- hourly inputs aggregation

  ## aggregate data at the daily time step
  daily_data <- SeriesAggreg(InputsModel[IndPeriod_Run], Format = "%Y%m%d")


  ## ---------- calculate interception

  ## calculate total interception of daily GR models on the period
  cum_daily <- sum(pmin(daily_data$Precip, daily_data$PotEvap))
  if (anyNA(cum_daily)) {
    stop("'IndPeriod_Run' must be set to select 24 hours by day")
  }

  ## calculate total interception of the GR5H interception store on the period
  ## and compute difference with daily values
  differences <- array(NA, c(length(TestedValues)))
  for (Imax in TestedValues) {
    C0 <- 0
    cum_hourly <- 0
    for (i in IndPeriod_Run) {
      Ec  <- min(InputsModel$PotEvap[i], InputsModel$Precip[i] + C0)
      Pth <- max(0, InputsModel$Precip[i] - (Imax-C0) - Ec)
      C0  <- C0 + InputsModel$Precip[i] - Ec - Pth
      cum_hourly <- cum_hourly + Ec
    }
    differences[which(Imax == TestedValues)] <- abs(cum_hourly - cum_daily)
  }

  ## return the Imax value that minimises the difference
  return(TestedValues[which.min(differences)])

}

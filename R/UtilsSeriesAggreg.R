.GetSeriesAggregFormat <- function(NewTimeFormat) {
  errNewTimeFormat <- FALSE
  if (missing(NewTimeFormat)) {
    errNewTimeFormat <- TRUE
  } else if (is.null(NewTimeFormat)) {
    errNewTimeFormat <- TRUE
  }
  if (errNewTimeFormat) {
    stop("Argument `Format` is missing")
  }
  if (!is.null(NewTimeFormat)) {
    TimeStep <- c("hourly", "daily", "monthly", "yearly")
    NewTimeFormat <- match.arg(NewTimeFormat, choices = TimeStep)
    Format <- switch(NewTimeFormat,
                     hourly  = "%Y%m%d%h",
                     daily   = "%Y%m%d",
                     monthly = "%Y%m",
                     yearly  = "%Y")
    msgNewTimeFormat <- sprintf("'Format' automatically set to %s", sQuote(Format))
    warning("deprecated 'NewTimeFormat' argument: please use 'Format' instead.",
            msgNewTimeFormat,
            call. = FALSE)
    return(Format)
  }
  return(NULL)
}

.GetSeriesAggregClass <- function(Format) {
  Format <- substr(Format,
                   start = nchar(Format),
                   stop = nchar(Format))
  switch(Format,
         h = "hourly",
         d = "daily",
         m = "monthly",
         Y = "yearly")
}

.GetAggregConvertFun <- function(x, Format) {
  AggregConvertFunTable <- rbind(
    data.frame(ConvertFun = "mean",
               x = c("Prod", "Rout", "Exp", "SnowPack", "ThermalState",
                     "Gratio", "Temp", "Gthreshold", "Glocalmax",
                     "LayerTempMean", "T"),
               stringsAsFactors = FALSE), # R < 4.0 compatibility: avoids mixing numeric and factor into numeric
    # create LayerSolidPrecip in order to compute LayerFracSolidPrecip
    data.frame(ConvertFun = "sum",
               x = c("PotEvap", "Precip", "Pn", "Ps", "AE", "Perc", "PR", "Q9",
                     "Q1", "Exch", "AExch1", "AExch2", "AExch", "QR", "QRExp",
                     "QD", "Qsim", "Pliq", "Psol", "PotMelt", "Melt", "PliqAndMelt",
                     "LayerPrecip", "LayerSolidPrecip", "LayerFracSolidPrecip",
                     "Qmm", "Qls", "E", "P", "Qupstream"),
               stringsAsFactors = FALSE) # R < 4.0 compatibility: avoids mixing numeric and factor into numeric
  )
  res <- sapply(x, function(iX) {
    iRes <- AggregConvertFunTable$ConvertFun[AggregConvertFunTable$x == iX]
    iRes <- ifelse(test = any(is.na(iRes)), yes = NA, no = iRes) # R < 4.0 compatibility
  })
  if (Format %in% c("%d", "%m")) {
    res <- rep("mean", length(res))
  }
  return(res)
}


#' Detect the time step
#'
#' @param x POSIX class
#' @noRd
.GetTimeStep <- function(x) {
  unit <- NA
  step <- NA

  if (length(x) < 2) {
    return(list(step = step, unit = unit))
  }

  diff_sec <- as.numeric(diff(x), units = "secs")
  median_diff_sec <- median(diff_sec, na.rm = TRUE)

  if (is.na(median_diff_sec)) {
    return(list(step = step, unit = unit))
  }

  if (median_diff_sec < 60) {
    unit <- "sec"
    step <- median_diff_sec
  } else if (median_diff_sec < 3600) {
    unit <- "min"
    step <- median_diff_sec / 60
  } else if (median_diff_sec < 86400) {
    unit <- "hour"
    step <- median_diff_sec / 3600
  } else {
    diff_days <- as.numeric(diff(x), units = "days")
    median_diff_days <- median(diff_days, na.rm = TRUE)

    if (!is.na(median_diff_days)) {
      if (median_diff_days < 28) {
        unit <- "day"
        step <- median_diff_days
      } else if (median_diff_days < 365) {
        unit <- "month"
        step <- median_diff_days / 30.44
      } else {
        unit <- "year"
        step <- median_diff_days / 365.25
      }
    }
  }

  list(step = round(step, digits = 1), unit = unit)
}

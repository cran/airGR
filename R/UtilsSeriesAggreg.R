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
                     "Gratio", "Temp", "Gthreshold", "Glocalmax", "LayerTempMean", "T"),
               stringsAsFactors = FALSE), # R < 4.0 compatibility: avoids mixing numeric and factor into numeric
    data.frame(ConvertFun = "sum",
               x = c("PotEvap", "Precip", "Pn", "Ps", "AE", "Perc", "PR", "Q9",
                     "Q1", "Exch", "AExch1", "AExch2", "AExch", "QR", "QRExp",
                     "QD", "Qsim", "Pliq", "Psol", "PotMelt", "Melt", "PliqAndMelt",
                     "LayerPrecip", "LayerFracSolidPrecip", "Qmm", "Qls", "E", "P", "Qupstream"),
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

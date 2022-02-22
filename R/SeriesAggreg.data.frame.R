SeriesAggreg.data.frame <- function(x,
                                    Format,
                                    ConvertFun,
                                    TimeFormat = NULL,    # deprecated
                                    NewTimeFormat = NULL, # deprecated
                                    YearFirstMonth = 1,
                                    TimeLag = 0,
                                    ...) {
  ## Arguments checks
  if (!is.null(TimeFormat)) {
    warning("deprecated 'TimeFormat' argument", call. = FALSE)
  }
  if (missing(Format)) {
    Format <- .GetSeriesAggregFormat(NewTimeFormat)
  } else if (!is.null(NewTimeFormat)) {
    warning("deprecated 'NewTimeFormat' argument: 'Format' argument is used instead",
            call. = FALSE)
  }
  if (is.null(Format)) {
    stop("argument 'Format' is missing")
  }

  ## check x
  if (!is.data.frame(x)) {
    stop("'x' must be a data.frame containing the dates and data to be aggregated")
  }
  if (ncol(x) < 2) {
    stop("'x' must contain at least two columns (including the column of dates)")
  }
  ## check x date column
  if (!inherits(x[[1L]], "POSIXt")) {
    stop("'x' first column must be a vector of class 'POSIXlt' or 'POSIXct'")
  }
  if (inherits(x[[1L]], "POSIXlt")) {
    x[[1L]] <- as.POSIXct(x[[1L]])
  }
  ## check x other columns (boolean converted to numeric)
  apply(x[, -1L, drop = FALSE],
        MARGIN = 2,
        FUN = function(iCol) {
          if (!is.numeric(iCol)) {
            stop("'x' columns (other than the first one) must be of numeric class")
          }
        })
  ## check Format
  listFormat <- c("%Y%m%d", "%Y%m", "%Y", "%m", "%d")
  Format <- gsub(pattern = "[[:punct:]]+", replacement = "%", Format)
  Format <- match.arg(Format, choices = listFormat)

  ## check ConvertFun
  if (length(ConvertFun) != (ncol(x) - 1)) {
    stop(sprintf("'ConvertFun' must be of length %i (ncol(x)-1)", ncol(x) - 1))
  }
  listConvertFun <- lapply(unique(ConvertFun), function(y) {
    if (!grepl("^q\\d+$", y, ignore.case = TRUE)) {
      return(match.fun(y))
    }
  })
  names(listConvertFun) <- unique(ConvertFun)
  lapply(ConvertFun, function(y) {
    if (!grepl("^q\\d+$", y, ignore.case = TRUE)) {
      TestOutput <- listConvertFun[[y]](1:10)
      if (!is.numeric(TestOutput)) {
        stop(sprintf("Returned value of '%s' function should be numeric", y))
      }
      if (length(TestOutput) != 1) {
        stop(sprintf("Returned value of '%s' function should be of length 1", y))
      }
    }
  })

  ## check YearFirstMonth
  msgYearFirstMonth <- "'YearFirstMonth' should be a single vector of numeric value between 1 and 12"
  YearFirstMonth <- match(YearFirstMonth, 1:12)
  if (anyNA(YearFirstMonth)) {
    stop(msgYearFirstMonth)
  }
  if (length(YearFirstMonth) != 1) {
    stop(msgYearFirstMonth)
  }
  if (YearFirstMonth != 1 & Format != "%Y") {
    warning("'YearFirstMonth' is ignored because  Format != '%Y'")
  }
  ## check TimeLag
  msgTimeLag <- "'TimeLag' should be a single vector of a positive numeric value"
  if (!is.vector(TimeLag)) {
    stop(msgTimeLag)
  }
  if (!is.numeric(TimeLag)) {
    stop(msgTimeLag)
  }
  if (length(TimeLag) != 1 | !any(TimeLag >= 0)) {
    stop(msgTimeLag)
  }

  TabSeries0 <- x
  colnames(TabSeries0)[1L] <- "DatesR"
  TabSeries0$DatesR <- TabSeries0$DatesR + TimeLag

  TabSeries2 <- TabSeries0

  if (!Format %in% c("%d", "%m")) {
    start <- sprintf("%i-01-01 00:00:00",
                     as.numeric(format(TabSeries2$DatesR[1L], format = "%Y")) - 1)
    stop  <- sprintf("%i-12-31 00:00:00",
                     as.numeric(format(TabSeries2$DatesR[nrow(TabSeries2)], format = "%Y")) + 1)
    unitTs <- format(diff(x[1:2, 1]))
    if (gsub("[0-9]+ ", "", unitTs) == "hours") {
      byTs <- "hours"
    } else {
      if (gsub(" days$", "", unitTs) == "1") {
        byTs <- "days"
      } else {
        byTs <- "months"
      }
    }
    fakeTs <- data.frame(DatesR = seq(from = as.POSIXct(start, tz = "UTC"),
                                      to   = as.POSIXct(stop , tz = "UTC"),
                                      by   = byTs) + TimeLag)
    TabSeries2 <- merge(fakeTs, TabSeries2, by = "DatesR", all.x = TRUE)
  }
  TabSeries2$DatesRini <- TabSeries2$DatesR %in% TabSeries0$DatesR


  TabSeries2$Selec2 <- format(TabSeries2$DatesR, Format)

  if (nchar(Format) > 2 | Format == "%Y") {
    # Compute aggregation
    TabSeries2$Selec <- !duplicated(TabSeries2$Selec2)
    if (all(TabSeries2$Selec)) {
      warning("the requested time 'Format' is the same as the one in 'x'. No time-step conversion was performed")
      return(x)
    }
    if (Format == "%Y") {
      yfm <- sprintf("%02.f", YearFirstMonth)
      spF1 <- "%m"
      spF2 <- "%Y-%m"
      TabSeries2$Selec1 <- format(TabSeries2$DatesR, spF1)
      TabSeries2$Selec2 <- format(TabSeries2$DatesR, spF2)
      TabSeries2$Selec <- !duplicated(TabSeries2$Selec2) & TabSeries2$Selec1 == yfm
    }
    TabSeries2$Fac2 <- cumsum(TabSeries2$Selec)
  } else {
    # Compute regime
    if (Format == "%d") {
      spF2 <- "%m-%d"
      TabSeries2$Selec2 <- format(TabSeries2$DatesR, spF2)
    }
    TabSeries2$Fac2 <- TabSeries2$Selec2
    TabSeries2$Selec <- !duplicated(TabSeries2$Selec2)
  }
  listTsAggreg <- lapply(names(listConvertFun), function(y) {
    if (any(ConvertFun == y)) {
      colTsAggreg <- c("Fac2", colnames(x)[-1L][ConvertFun == y])
      if (grepl("^q\\d+$", y, ignore.case = TRUE)) {
        probs <- as.numeric(gsub("^q", "", y, ignore.case = TRUE)) / 100
        if (probs < 0 || probs > 1) {
          stop("'Q...' format of argument 'ConvertFun' must be an integer between 0 and 100")
        }
        aggregate(. ~ Fac2,
                  data = TabSeries2[, colTsAggreg],
                  FUN = quantile,
                  na.action = na.pass,
                  probs = probs,
                  type = 8,
                  na.rm = TRUE)
      } else {
        aggregate(. ~ Fac2,
                  data = TabSeries2[, colTsAggreg],
                  FUN = listConvertFun[[y]],
                  na.action = na.pass)
      }
    } else {
      NULL
    }
  })
  listTsAggreg <- listTsAggreg[!sapply(listTsAggreg, is.null)]
  tsAggreg <- do.call(cbind, listTsAggreg)
  tsAggreg <- tsAggreg[, !duplicated(colnames(tsAggreg))]
  tsAggreg <- merge(tsAggreg,
                    TabSeries2[, c("Fac2", "DatesR", "DatesRini", "Selec")],
                    by = "Fac2",
                    all.x = TRUE,
                    all.y = FALSE)
  tsAggreg <- tsAggreg[tsAggreg$Selec & tsAggreg$DatesRini, ]
  tsAggreg <- tsAggreg[, colnames(TabSeries0)]
  tsAggreg <- tsAggreg[order(tsAggreg$DatesR), ] # reorder by date especially for regime time series
  colnames(tsAggreg)[1L] <- colnames(x)[1L]      # keep original column names
  return(tsAggreg)

}

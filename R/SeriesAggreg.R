SeriesAggreg <- function(TabSeries,
                         TimeFormat, NewTimeFormat,
                         ConvertFun,
                         YearFirstMonth = 1, TimeLag = 0,
                         verbose = TRUE) {
  
  
  ##_Arguments_check
  
  ##check_TabSeries
  if (is.null(TabSeries)       ) {
    stop("'TabSeries' must be a data.frame containing the dates and data to be converted")
  }
  if (!is.data.frame(TabSeries)) {
    stop("'TabSeries' must be a data.frame containing the dates and data to be converted")
  }
  if (ncol(TabSeries) < 2) {
    stop("'TabSeries' must contain at least two columns (including the coulmn of dates")
  }
  ##check_TimeFormat
  if (!any(class(TabSeries[, 1]) %in% "POSIXt")) {
    stop("'TabSeries' first column must be a vector of class 'POSIXlt' or 'POSIXct'")
  }
  if (any(class(TabSeries[, 1]) %in% "POSIXlt")) {
    TabSeries[, 1] <- as.POSIXct(TabSeries[, 1])
  }
  for (iCol in 2:ncol(TabSeries)) {
    if (!is.numeric(TabSeries[,iCol])) {
      stop("'TabSeries' columns (other than the first one) be of numeric class")
    }
  }
  if (is.null(TimeFormat)) {
    stop("'TimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (!is.vector(TimeFormat)) {
    stop("'TimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (!is.character(TimeFormat)) {
    stop("'TimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (length(TimeFormat) != 1) {
    stop("'TimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  } 
  if (! TimeFormat %in% c("hourly", "daily", "monthly", "yearly")) {
    stop("'TimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  ##check_NewTimeFormat
  if (is.null(NewTimeFormat)) {
    stop("'NewTimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (!is.vector(NewTimeFormat)) {
    stop("'NewTimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (!is.character(NewTimeFormat)) {
    stop("'NewTimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (length(NewTimeFormat) != 1) {
    stop("'NewTimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  if (! NewTimeFormat %in% c("hourly", "daily", "monthly", "yearly")) {
    stop("'NewTimeFormat' must be one of 'hourly', 'daily', 'monthly' or 'yearly'")
  }
  ##check_ConvertFun
  if (is.null(ConvertFun)) {
    stop("'ConvertFun' must be a vector of character")
  }
  if (!is.vector(ConvertFun)) {
    stop("'ConvertFun' must be a vector of character")
  }
  if (!is.character(ConvertFun)) {
    stop("'ConvertFun' must be a vector of character")
  }
  if (length(ConvertFun) != (ncol(TabSeries) - 1)) {
    stop(
      paste("'ConvertFun' must be of length", ncol(TabSeries) - 1, "(length=ncol(TabSeries)-1)")
    )
  }
  if (sum(ConvertFun %in% c("sum", "mean") == FALSE) != 0) {
    stop("'ConvertFun' elements must be one of 'sum' or 'mean'")
  }
  ##check_YearFirstMonth
  if (is.null(YearFirstMonth)) {
    stop("'YearFirstMonth' must be an integer between 1 and 12")
  }
  if (!is.vector(YearFirstMonth)) {
    stop("'YearFirstMonth' must be an integer between 1 and 12")
  }
  if (!is.numeric(YearFirstMonth)) {
    stop("'YearFirstMonth' must be an integer between 1 and 12")
  }
  YearFirstMonth <- as.integer(YearFirstMonth)
  if (length(YearFirstMonth) != 1) {
    stop("'YearFirstMonth' must be only one integer between 1 and 12")
  }
  if (YearFirstMonth %in% (1:12) == FALSE) {
    stop("'YearFirstMonth' must be only one integer between 1 and 12")
  } 
  ##check_DatesR_integrity
  if (TimeFormat == "hourly") {
    by <- "hours"
  }
  if (TimeFormat == "daily") {
    by <- "days"
  }
  if (TimeFormat == "monthly") {
    by <- "months"
  }
  if (TimeFormat == "yearly") {
    by <- "years"
  }
  TmpDatesR <- seq(from = TabSeries[1, 1], to = tail(TabSeries[, 1], 1), by = by)
  if (!identical(TabSeries[, 1], TmpDatesR)) {
    stop("some dates might not be ordered or are missing in 'TabSeries'")
  }
  ##check_conversion_direction
  if ((TimeFormat == "daily"   & NewTimeFormat %in% c("hourly")                  ) |
      (TimeFormat == "monthly" & NewTimeFormat %in% c("hourly","daily")          ) |
      (TimeFormat == "yearly"  & NewTimeFormat %in% c("hourly","daily","monthly"))) { 
    stop("only time aggregation can be performed")
  } 
  ##check_if_conversion_not_needed
  if ((TimeFormat == "hourly"  & NewTimeFormat == "hourly" ) |
      (TimeFormat == "daily"   & NewTimeFormat == "daily"  ) |
      (TimeFormat == "monthly" & NewTimeFormat == "monthly") |
      (TimeFormat == "yearly"  & NewTimeFormat == "yearly" )) { 
    if (verbose) {
      warning("the old and new format are identical \n\t -> no time-step conversion was performed")
      return(TabSeries)
    }
  }
  
  
  ##_Time_step_conversion
  
  ##_Handle_conventional_difference_between_hourly_series_and_others
  TmpDatesR <- TabSeries[, 1]
  #if (TimeFormat=="hourly") { TmpDatesR <- TmpDatesR - 60*60; }
  TmpDatesR <- TmpDatesR + TimeLag
  Hmax <- "00"
  if (TimeFormat == "hourly") {
    Hmax <- "23"
  }
  
  ##_Identify_the_part_of_the_series_to_be_aggregated
  NDaysInMonth <- list("31", c("28", "29"), "31", "30", "31", "30", "31", "31", "30", "31", "30", "31")
  YearLastMonth <- YearFirstMonth + 11
  if (YearLastMonth > 12) {
    YearLastMonth <- YearLastMonth - 12
  }
  YearFirstMonthTxt <- formatC(YearFirstMonth, format = "d", width = 2, flag = "0")
  YearLastMonthTxt  <- formatC(YearLastMonth , format = "d", width = 2, flag = "0")
  if (NewTimeFormat == "daily") {
    Ind1 <- which(format(TmpDatesR,    "%H") == "00")
    Ind2 <- which(format(TmpDatesR,    "%H") == Hmax)
    if (Ind2[1] < Ind1[1]) {
      Ind2 <- Ind2[2:length(Ind2)]
    }
    if (tail(Ind1, 1) > tail(Ind2, 1)) {
      Ind1 <- Ind1[1:(length(Ind1) - 1)]
    }
    ### Aggr <- NULL; iii <- 0; for(kkk in 1:length(Ind1)) {
    ### iii <- iii+1; Aggr <- c(Aggr,rep(iii,length(Ind1[kkk]:Ind2[kkk]))); }
    Aggr <- as.numeric(format(TmpDatesR[min(Ind1):max(Ind2)], "%Y%m%d"))
    ### more efficient
    NewDatesR <- data.frame(seq(from = TmpDatesR[min(Ind1)], to = TmpDatesR[max(Ind2)], by = "days"))
  }
  if (NewTimeFormat=="monthly") {
    Ind1 <- which(format(TmpDatesR,  "%d%H") == "0100")
    Ind2 <- which(format(TmpDatesR,"%m%d%H") %in% paste0(c("0131", "0228", "0229", "0331", "0430", "0531", "0630", "0731", "0831", "0930", "1031", "1130", "1231"), Hmax))
    Ind2[1:(length(Ind2) - 1)][diff(Ind2) == 1] <- NA
    Ind2 <- Ind2[!is.na(Ind2)] ### to keep only feb 29 if both feb 28 and feb 29 exists
    if (Ind2[1] < Ind1[1]) {
      Ind2 <- Ind2[2:length(Ind2)]
    }
    if (tail(Ind1, 1) > tail(Ind2, 1)) {
      Ind1 <- Ind1[1:(length(Ind1) - 1)]
    }
    ### Aggr <- NULL; iii <- 0; for(kkk in 1:length(Ind1)) { 
    ### iii <- iii+1; Aggr <- c(Aggr,rep(iii,length(Ind1[kkk]:Ind2[kkk]))); }
    Aggr <- as.numeric(format(TmpDatesR[min(Ind1):max(Ind2)],"%Y%m"));  ### more efficient
    NewDatesR <- data.frame(seq(from=TmpDatesR[min(Ind1)],to=TmpDatesR[max(Ind2)],by="months"))
  }
  if (NewTimeFormat == "yearly") {
    Ind1 <- which(format(TmpDatesR, "%m%d%H") %in% paste0(YearFirstMonthTxt, "0100"))
    Ind2 <- which(format(TmpDatesR, "%m%d%H") %in% paste0(YearLastMonthTxt, NDaysInMonth[[YearLastMonth]], Hmax))
    Ind2[1:(length(Ind2) - 1)][diff(Ind2) == 1] <- NA
    Ind2 <- Ind2[!is.na(Ind2)]
    ### to keep only feb 29 if both feb 28 and feb 29 exists
    if (Ind2[1] < Ind1[1]) {
      Ind2 <- Ind2[2:length(Ind2)]
    }
    if (tail(Ind1, 1) > tail(Ind2, 1)) {
      Ind1 <- Ind1[1:(length(Ind1) - 1)]
    }
    Aggr <- NULL
    iii <- 0
    for (kkk in 1:length(Ind1)) {
      iii <- iii + 1
      Aggr <- c(Aggr, rep(iii, length(Ind1[kkk]:Ind2[kkk])))
    }
    ### Aggr <- as.numeric(format(TmpDatesR[min(Ind1):max(Ind2)],"%Y")); ### not working if YearFirstMonth != 01
    NewDatesR <- data.frame(seq(from = TmpDatesR[min(Ind1)], to = TmpDatesR[max(Ind2)], by = "years"))
  }
  ##_Aggreation_and_export
  NewTabSeries <- data.frame(NewDatesR)
  for (iCol in 2:ncol(TabSeries)) {
    AggregData <- aggregate(TabSeries[min(Ind1):max(Ind2), iCol],
                            by = list(Aggr),
                            FUN = ConvertFun[iCol - 1], na.rm = FALSE)[, 2]
    NewTabSeries <- data.frame(NewTabSeries, AggregData)
  }
  names(NewTabSeries) <- names(TabSeries)
  return(NewTabSeries)
  
  
}
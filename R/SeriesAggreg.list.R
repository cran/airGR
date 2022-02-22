SeriesAggreg.list <- function(x,
                              Format,
                              ConvertFun,
                              NewTimeFormat = NULL,
                              simplify = FALSE,
                              except = NULL,
                              recursive = TRUE,
                              ...) {

  classIni <- class(x)
  class(x) <- "list" # in order to avoid the use of '['.InputsModel' or '['.OutputsModel' when x[i] is used

  if (missing(Format)) {
    Format <- .GetSeriesAggregFormat(NewTimeFormat)
  } else if (!is.null(NewTimeFormat)) {
    warning("deprecated 'NewTimeFormat' argument: 'Format' argument is used instead",
            call. = FALSE)
  }
  # Check ConvertFun
  if (any(classIni %in% c("InputsModel", "OutputsModel"))) {
    if (!all(is.na(ConvertFun))) {
      warning("Argument 'ConvertFun' is ignored on 'InputsModel' and 'OutputsModel' objects")
    }
  } else if (length(ConvertFun) != 1) {
    stop("Argument 'ConvertFun' must be of length 1 with 'list' object")
  } else if (!is.character(ConvertFun)) {
    stop("Argument 'ConvertFun' must be a character")
  }

  # Determination of DatesR
  if (!is.null(x$DatesR)) {
    if (!inherits(x$DatesR, "POSIXt")) {
      stop("'x$DatesR' should be of class 'POSIXt'")
    }
    DatesR <- x$DatesR
  } else {
    # Auto-detection of POSIXt item in Tabseries
    itemPOSIXt <- which(sapply(x, function(x) {
      inherits(x, "POSIXt")
    }, simplify = TRUE))[1]
    if (is.na(itemPOSIXt)) {
      stop("At least one item of argument 'x' should be of class 'POSIXt'")
    }
    warning("Item 'DatesR' not found in 'x' argument: the item ",
            names(x)[itemPOSIXt],
            " has been automatically chosen")
    DatesR <- x[[names(x)[itemPOSIXt]]]
  }

  # Selection of numeric items for aggregation
  numericCols <- names(which(sapply(x, inherits, "numeric")))
  arrayCols <- names(which(sapply(x, inherits, "array")))
  numericCols <- setdiff(numericCols, arrayCols)
  if (!is.null(except)) {
    if (!inherits(except, "character")) {
      stop("Argument 'except' should be a 'character'")
    }
    numericCols <- setdiff(numericCols, except)
  }

  cols <- x[numericCols]
  lengthCols <- sapply(cols, length, simplify = TRUE)
  if (any(lengthCols != length(DatesR))) {
    sErr <- paste0(names(lengthCols)[lengthCols != length(DatesR)],
                   " (", lengthCols[lengthCols != length(DatesR)], ")",
                   collapse = ", ")
    warning("The length of the following `numeric` items in 'x' ",
            "is different than the length of 'DatesR (",
            length(DatesR),
            ")': they will be ignored in the aggregation: ",
            sErr)
    cols <- cols[lengthCols == length(DatesR)]
  }
  dfOut <- NULL
  if (length(cols)) {
    # Treating aggregation at root level
    if (is.na(ConvertFun)) {
      ConvertFun2 <- .GetAggregConvertFun(names(cols), Format)
    } else {
      ConvertFun2 <- rep(ConvertFun, length(cols))
    }
    dfOut <- SeriesAggreg(cbind(DatesR, as.data.frame(cols)),
                          Format,
                          ...,
                          ConvertFun = ConvertFun2)
  }

  if (simplify) {
    # Returns data.frame of numeric found in the first level of the list
    return(dfOut)

  } else {
    res <- list()
    # Convert aggregated data.frame into list
    if (!is.null(dfOut)) {
      res <- as.list(dfOut)
      ## To be consistent with InputsModel class and because plot.OutputsModel use the POSIXlt class
      res$DatesR <- as.POSIXlt(res$DatesR)
    }

    # Exploration of embedded lists and data.frames
    if (is.null(recursive) || recursive) {
      listCols <- x[!names(x) %in% except]
      listCols <- listCols[sapply(listCols, inherits, "list")]
      dfCols <- x[sapply(x, inherits, "data.frame")]
      dfCols <- c(dfCols, x[sapply(x, inherits, "matrix")])
      listCols <- listCols[setdiff(names(listCols), names(dfCols))]
      if (length(listCols) > 0) {
        if (is.na(ConvertFun)) {
          # Check for predefined ConvertFun for all sub-elements
          listConvertFun <- .GetAggregConvertFun(names(listCols), Format)
        }
        # Run SeriesAggreg for each embedded list
        listRes <- lapply(names(listCols), function(y) {
          listCols[[y]]$DatesR <- DatesR
          if (is.na(ConvertFun)) {
            SeriesAggreg.list(listCols[[y]],
                       Format = Format,
                       recursive = NULL,
                       ...,
                       ConvertFun = listConvertFun[y])
          } else {
            SeriesAggreg.list(listCols[[y]],
                              Format = Format,
                              recursive = NULL,
                              ...)
          }
        })
        names(listRes) <- names(listCols)
        if (is.null(res$DatesR)) {
          # Copy DatesR in top level list
          res$DatesR <- listRes[[1]]$DatesR
        }
        # Remove DatesR in embedded lists
        lapply(names(listRes), function(x) {
          listRes[[x]]$DatesR <<- NULL
        })
        res <- c(res, listRes)
      }
      if (length(dfCols) > 0) {
        # Processing matrix and dataframes
        for (i in length(dfCols)) {
          key <- names(dfCols)[i]
          m <- dfCols[[i]]
          if (nrow(m) != length(DatesR)) {
            warning(
              "The number of rows of the 'matrix' item ",
              key, " (", nrow(m),
              ") is different than the length of 'DatesR ('", length(DatesR),
              "), it will be ignored in the aggregation"
            )
          } else {
            if (is.na(ConvertFun)) {
              ConvertFun2 <- rep(.GetAggregConvertFun(key, Format), ncol(m))
            } else {
              ConvertFun2 <- rep(ConvertFun, ncol(m))
            }
            res[[key]] <- SeriesAggreg.data.frame(data.frame(DatesR, m),
                                       Format = Format,
                                       ConvertFun = ConvertFun2)
          }
        }
      }
    }

    # Store elements that are not aggregated
    res <- c(res, x[setdiff(names(x), names(res))])

    class(res) <- gsub("hourly|daily|monthly|yearly",
                       .GetSeriesAggregClass(Format),
                       classIni)

    return(res)

  }

}

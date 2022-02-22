plot.OutputsModel <- function(x, Qobs = NULL, IndPeriod_Plot = NULL, BasinArea = NULL, which = "synth", log_scale = FALSE,
                              cex.axis = 1, cex.lab = 0.9, cex.leg = 0.9, lwd = 1,
                              AxisTS = function(x) axis.POSIXct(side = 1, x = x$DatesR, ...),
                              LayoutMat = NULL, LayoutWidths = rep.int(1, ncol(LayoutMat)), LayoutHeights = rep.int(1, nrow(LayoutMat)),
                              verbose = TRUE, ...) {


  ## save default graphical & time parameters and resetting on exit
  opar <- par(no.readonly = TRUE)
  olctime <- Sys.getlocale(category = "LC_TIME")
  suppressWarnings(.TrySetLcTimeEN())
  on.exit({
    par(opar)
    Sys.setlocale(category = "LC_TIME", locale = olctime)
  })


  OutputsModel <- x

  ## index time series
  if (!is.null(IndPeriod_Plot)) {
    if (length(IndPeriod_Plot) == 0) {
      IndPeriod_Plot <- seq_along(OutputsModel$DatesR)
    }
    IndPeriod_Plot <- seq_along(IndPeriod_Plot)
    OutputsModel <- .IndexOutputsModel(OutputsModel, IndPeriod_Plot)
    Qobs <- Qobs[IndPeriod_Plot]
  } else {
    IndPeriod_Plot <- seq_along(OutputsModel$DatesR)
  }


  ## ---------- check arguments

  if (!inherits(OutputsModel, "GR") & !inherits(OutputsModel, "CemaNeige")) {
    stop("'OutputsModel' not in the correct format for default plotting")
  }

  ## check 'OutputsModel'
  BOOL_Dates <- FALSE
  if ("DatesR" %in% names(OutputsModel)) {
    BOOL_Dates <- TRUE
  }

  BOOL_Pobs <- FALSE
  if ("Precip" %in% names(OutputsModel)) {
    BOOL_Pobs <- TRUE
  }

  BOOL_EPobs <- FALSE
  if ("PotEvap" %in% names(OutputsModel)) {
    BOOL_EPobs <- TRUE
  }

  BOOL_EAobs <- FALSE
  if ("AE" %in% names(OutputsModel)) {
    BOOL_EAobs <- TRUE
  }

  BOOL_Qsim <- FALSE
  if ("Qsim"   %in% names(OutputsModel)) {
    BOOL_Qsim <- TRUE
  }

  BOOL_Qobs <- FALSE
  if (BOOL_Qsim & length(Qobs) == length(OutputsModel$Qsim)) {
    if (sum(is.na(Qobs)) != length(Qobs)) {
      BOOL_Qobs <- TRUE
    }
  } else if (inherits(OutputsModel, "GR") & !is.null(Qobs)) {
    warning("incorrect length of 'Qobs'. Time series of observed flow not drawn")
  }

  BOOL_Error <- FALSE
  if (BOOL_Qsim & BOOL_Qobs) {
    BOOL_Error <- TRUE
  }

  BOOL_Snow <- FALSE
  if ("CemaNeigeLayers" %in% names(OutputsModel)) {
    if ("SnowPack" %in% names(OutputsModel$CemaNeigeLayers[[1]])) {
      BOOL_Snow <- TRUE
    }
  }

  BOOL_Psol <- FALSE
  if ("CemaNeigeLayers" %in% names(OutputsModel)) {
    if ("Psol"     %in% names(OutputsModel$CemaNeigeLayers[[1]])) {
      BOOL_Psol <- TRUE
    }
  }


  ## check 'which'
  whichNeedQobs  <- c("Error", "CorQQ")
  whichDashboard <- c("all", "synth", "ts", "perf")
  whichAll   <- c("Precip", "PotEvap", "ActuEvap", "Temp", "SnowPack", "Flows", "Error", "Regime", "CumFreq", "CorQQ")
  whichSynth <- c("Precip"           ,             "Temp", "SnowPack", "Flows"         , "Regime", "CumFreq", "CorQQ")
  whichTS    <- c("Precip", "PotEvap",             "Temp", "SnowPack", "Flows"                                       )
  whichEvap  <- c(          "PotEvap", "ActuEvap"                                                                    )
  whichPerf  <- c(                                                              "Error", "Regime", "CumFreq", "CorQQ")
  whichCN    <- c(                                 "Temp", "SnowPack"                                                )
  warnMsgWhich   <- "'which' must be a vector of character"
  warnMsgNoQobs  <- "the %s plot(s) cannot be drawn if there is no 'Qobs'"
  warnMsgWhichCN <- sprintf("incorrect element found in argument 'which':\n\twithout CemaNeige, %s are not available \n\tit can only contain %s",
                            paste0(shQuote(whichCN), collapse = " and "),
                            paste0(shQuote(c(whichDashboard, whichAll[!whichAll %in% whichCN])), collapse = ", "))
  if (is.null(which)) {
    stop(warnMsgWhich)
  }
  if (!is.vector(which)) {
    stop(warnMsgWhich)
  }
  if (!is.character(which)) {
    stop(warnMsgWhich)
  }
  if (any(!which %in% c(whichDashboard, whichAll))) {
    stop(sprintf("incorrect element found in argument 'which': %s\nit can only contain %s",
                 paste0(shQuote(which[!which %in% c(whichDashboard, whichAll)])),
                 paste0(shQuote(c(whichDashboard, whichAll)), collapse = ", ")))
  }
  if (all(which %in% whichCN) & !inherits(OutputsModel, "CemaNeige")) {
    stop(warnMsgWhichCN)
  }
  if (length(unique(which %in% whichCN)) == 2 & !inherits(OutputsModel, "CemaNeige")) {
    warning(warnMsgWhichCN)
  }
  if (all(!which %in% c("all", "synth", "ts", whichCN)) & !inherits(OutputsModel, "GR")) {
    stop(sprintf("incorrect element found in argument 'which': \nwith CemaNeige alone, only %s are available",
                 paste0(shQuote(c("all", "synth", "ts", "Temp", "SnowPack")), collapse = ", ")))
  }
  if (any(!which %in% c("all", "synth", "ts", whichCN)) & !inherits(OutputsModel, "GR")) {
    warning(sprintf("incorrect element found in argument 'which': \nwith CemaNeige alone, only %s are available",
                    paste0(shQuote(c("all", "synth", "ts", "Temp", "SnowPack")), collapse = ", ")))
  }
  if ("perf" %in% which) {
    which <- c(which, whichPerf)
  }
  if ("ts" %in% which) {
    which <- c(which, whichTS)
  }
  if ("synth" %in% which) {
    which <- c(which, whichSynth)
  }
  if ("all" %in% which) {
    which <- c(which, whichAll)
  }
  if (is.null(Qobs) & inherits(OutputsModel, "GR")) {
    if (length(which) == 1 & (any(which %in% whichNeedQobs))) {
      stop(sprintf(warnMsgNoQobs, shQuote(which)))
    }
    if (length(which) != 1 & any(which %in% whichNeedQobs)) {
      BOOL_CorQQ <- FALSE
      BOOL_Error <- FALSE
      warning(sprintf(warnMsgNoQobs, paste0(shQuote(whichNeedQobs), collapse = " and ")))
    }
  }


  ## check dates
  if (!BOOL_Dates) {
    stop("'OutputsModel' must contain at least 'DatesR' to allow plotting")
  }
  if (inherits(OutputsModel, "GR") & !BOOL_Qsim) {
    stop("'OutputsModel' must contain at least 'Qsim' to allow plotting")
  }

  if (BOOL_Dates) {
    # MyRollMean1 <- function(x, n) {
    #   return(filter(x, rep(1 / n, n), sides = 2))
    # }
    # MyRollMean2 <- function(x, n) {
    #   return(filter(c(tail(x, n %/% 2), x, x[1:(n %/% 2)]), rep(1 / n, n), sides = 2)[(n %/% 2 + 1):(length(x) + n %/% 2)])
    # }
    MyRollMean3 <- function(x, n) {
      return(filter(x, filter = rep(1 / n, n), sides = 2, circular = TRUE))
    }
    BOOL_TS  <- FALSE
    if (inherits(OutputsModel, "hourly")) {
      BOOL_TS <- TRUE
      NameTS <- "hour"
      plotunit <- "[mm/h]"
    } else if (inherits(OutputsModel, "daily")) {
      BOOL_TS <- TRUE
      NameTS <- "day"
      plotunit <- "[mm/d]"
    } else if (inherits(OutputsModel, "monthly")) {
      BOOL_TS <- TRUE
      NameTS <- "month"
      plotunit <- "[mm/month]"
    } else if (inherits(OutputsModel, "yearly")) {
      BOOL_TS <- TRUE
      NameTS <- "year"
      plotunit <- "[mm/y]"
    }
    # if (!BOOL_TS) {
    #   stop("the time step of the model inputs could not be found")
    # }
  }
  if (inherits(OutputsModel, "CemaNeige")) {
    NLayers <- length(OutputsModel$CemaNeigeLayers)
  }
  PsolLayerMean <- NULL
  if (BOOL_Psol) {
    for (iLayer in 1:NLayers) {
      if (iLayer == 1) {
        PsolLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$Psol / NLayers
      } else {
        PsolLayerMean <- PsolLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$Psol / NLayers
      }
    }
  }
  BOOL_QobsZero <- FALSE
  if (BOOL_Qobs) {
    SelectQobsNotZero <- round(Qobs, 4) != 0
    BOOL_QobsZero <- sum(!SelectQobsNotZero, na.rm = TRUE) > 0
  }
  BOOL_QsimZero <- FALSE
  if (BOOL_Qsim) {
    SelectQsimNotZero <- round(OutputsModel$Qsim, 4) != 0
    BOOL_QsimZero <- sum(!SelectQsimNotZero, na.rm = TRUE) > 0
  }
  if ( BOOL_Qobs & !BOOL_Qsim) {
    SelectNotZero <- SelectQobsNotZero
  }
  if (!BOOL_Qobs &  BOOL_Qsim) {
    SelectNotZero <- SelectQsimNotZero
  }
  if ( BOOL_Qobs &  BOOL_Qsim) {
    SelectNotZero <- SelectQobsNotZero & SelectQsimNotZero
  }
  if (BOOL_QobsZero & verbose) {
    warning("zeroes detected in 'Qobs': some plots in the log space will not be created using all time-steps")
  }
  if (BOOL_QsimZero & verbose) {
    warning("zeroes detected in 'Qsim': some plots in the log space will not be created using all time-steps")
  }
  BOOL_FilterZero <- TRUE



  ## ---------- plot

  ## plot choices
  BOOLPLOT_Precip   <- "Precip"   %in% which &  BOOL_Pobs
  BOOLPLOT_PotEvap  <- "PotEvap"  %in% which &  BOOL_EPobs
  BOOLPLOT_ActuEvap <- "ActuEvap" %in% which &  BOOL_EAobs
  BOOLPLOT_Temp     <- "Temp"     %in% which &  BOOL_Snow
  BOOLPLOT_SnowPack <- "SnowPack" %in% which &  BOOL_Snow
  BOOLPLOT_Flows    <- "Flows"    %in% which & (BOOL_Qsim | BOOL_Qobs)
  BOOLPLOT_Error    <- "Error"    %in% which &  BOOL_Error
  BOOLPLOT_Regime   <- "Regime"   %in% which &  BOOL_Qsim & BOOL_TS & (NameTS %in% c("hour", "day", "month"))
  BOOLPLOT_CumFreq  <- "CumFreq"  %in% which & (BOOL_Qsim | BOOL_Qobs) & BOOL_FilterZero
  BOOLPLOT_CorQQ    <- "CorQQ"    %in% which & (BOOL_Qsim & BOOL_Qobs) & BOOL_FilterZero


  ## options
  BLOC <- TRUE
  if (BLOC) {
    lwdk <- 1.8
    line <- 2.6
    bg   <- NA

    ## Set plot arrangement
    if (is.null(LayoutMat)) {
      matlayout <- NULL
      iPlot <- 0
      iHght <- NULL

      listBOOLPLOT1 <- c(Precip = BOOLPLOT_Precip,
                         PotEvap = BOOLPLOT_PotEvap | BOOLPLOT_ActuEvap,
                         Temp   = BOOLPLOT_Temp  , SnowPack = BOOLPLOT_SnowPack,
                         Flows  = BOOLPLOT_Flows , Error    = BOOLPLOT_Error)
      listBOOLPLOT2 <- c(Regime = BOOLPLOT_Regime, CumFreq  = BOOLPLOT_CumFreq,
                         CorQQ = BOOLPLOT_CorQQ)
      Sum1 <- sum(listBOOLPLOT1)
      Sum2 <- sum(listBOOLPLOT2)

      for (k in seq_len(Sum1)) {
        matlayout <- rbind(matlayout, iPlot + c(1, 1, 1))
        iPlot <- iPlot + 1
        iHght <- c(iHght, 0.7)
      }
      ## Flows plot is higher than the other TS
      listBOOLPLOT1 <- listBOOLPLOT1[listBOOLPLOT1]
      listBOOLPLOTF <- (names(listBOOLPLOT1) == "Flows") * BOOLPLOT_Flows
      iHght <- iHght + listBOOLPLOTF * listBOOLPLOT1 * 0.3
      if (Sum2 >= 1) {
        iHght <- c(iHght, 1.0)
      }
      if ((Sum1 >= 1 & Sum2 != 0) | (Sum1 == 0 & Sum2 == 3)) {
        matlayout <- rbind(matlayout, iPlot + c(1, 2, 3))
        iPlot <- iPlot + 3
      }
      if (Sum1 == 0 & Sum2 == 2) {
        matlayout <- rbind(matlayout, iPlot + c(1, 2))
        iPlot <- iPlot + 2
      }
      if (Sum1 == 0 & Sum2 == 1) {
        matlayout <- rbind(matlayout, iPlot + 1)
        iPlot <- iPlot + 1
      }

      iPlotMax <- iPlot
      LayoutWidths  <- rep.int(1, ncol(matlayout))
      LayoutHeights <- iHght #rep.int(1, nrow(matlayout))
    }
    if (!is.null(LayoutMat)) {
      matlayout <- LayoutMat
    }
    layout(matlayout, widths = LayoutWidths, heights = LayoutHeights)




    Xaxis <- as.POSIXct(OutputsModel$DatesR)

    if (!is.null(BasinArea)) {
      Factor_UNIT_M3S <- switch(NameTS,
                                hour  = 60 * 60,
                                day   = 60 * 60 * 24,
                                month = 60 * 60 * 24 * 365.25 / 12,
                                year  = 60 * 60 * 24 * 365.25)
      Factor_UNIT_M3S <- BasinArea / (Factor_UNIT_M3S / 1000)
    }
  }

  kPlot <- 0

  ## vector of Q values for the y-axis when it is expressed in
  Factor <- ifelse(!is.null(BasinArea), Factor_UNIT_M3S, 1)
  seqDATA0 <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
  seqDATA1 <- log(seqDATA0)
  seqDATA2 <- exp(seqDATA1)
  if (!is.null(BasinArea)) {
    seqDATA1ba <- log(seqDATA0 * Factor_UNIT_M3S)
    seqDATA2ba <- round(exp(seqDATA1ba), digits = 2)
  }

  ## Precip
  if (BOOLPLOT_Precip) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)

    par(new = FALSE, mar = mar)
    ylim1 <- range(OutputsModel$Precip, na.rm = TRUE)
    ylim2 <- ylim1 * c(1.0, 1.1)
    ylim2 <- rev(ylim2)

    lwdP <- lwd * 0.7
    if (NameTS %in% c("month", "year")) {
      lwdP <- lwd * 2
    }
    plot(Xaxis, OutputsModel$Precip,
         type = "h", xaxt = "n", yaxt = "n", yaxs = "i", ylim = ylim2,
         col = "royalblue", lwd = lwdP * lwdk, lend = 1,
         xlab = "", ylab = "", ...)
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)
    mtext(side = 2, paste("precip.", plotunit), cex = cex.lab, adj = 1, line = line)

    if (BOOL_Psol) {
      legend("bottomright", legend = c("solid","liquid"),
             col = c("lightblue", "royalblue"), lty = c(1, 1), lwd = c(lwd, lwd),
             bty = "o", bg = bg, box.col = bg, cex = cex.leg)
      points(Xaxis, PsolLayerMean,
             type = "h", xaxt = "n", yaxt = "n", yaxs = "i", ylim = ylim2,
             col = "lightblue", lwd = lwdP * lwdk, lend = 1,
             xlab = "", ylab = "", ...)
    }

    AxisTS(OutputsModel)

    box()
  }


  ## PotEvap
  if (BOOLPLOT_PotEvap) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)

    par(new = FALSE, mar = mar)
    if (!BOOLPLOT_ActuEvap) {
      ylim1 <- range(OutputsModel$PotEvap, na.rm = TRUE)
      xlabE <- "pot. evap."
    } else {
      ylim1 <- range(c(OutputsModel$PotEvap,
                       OutputsModel$AE),
                     na.rm = TRUE)
      xlabE <- "evap."
    }
    ylim2 <- ylim1 #* c(1.0, 1.1)


    plot(Xaxis, OutputsModel$PotEvap,
         type = "l", xaxt = "n", yaxt = "n", ylim = ylim2,
         col = "green3", lwd = lwd * lwdk,
         xlab = "", ylab = "", ...)
    if (BOOLPLOT_ActuEvap) {
      lines(Xaxis, OutputsModel$AE,
            type = "l", xaxt = "n", yaxt = "n", ylim = ylim2,
            col = "green4", lwd = lwd * lwdk, lty = 3)
      legend("topright", legend = c("pot.", "actu."), col = c("green3", "green4"),
             lty = c(1, 3), lwd = c(lwd * 1.0, lwd * 0.8),
             bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    }
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)

    mtext(side = 2, paste(xlabE, plotunit), cex = cex.lab, line = line)

    AxisTS(OutputsModel)

    box()
  }

  ## ActuEvap
  if (BOOLPLOT_ActuEvap & !BOOLPLOT_PotEvap) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)

    par(new = FALSE, mar = mar)
    ylim1 <- range(OutputsModel$AE, na.rm = TRUE)
    ylim2 <- ylim1 #* c(1.0, 1.1)

    plot(Xaxis, OutputsModel$AE,
         type = "l", xaxt = "n", yaxt = "n", ylim = ylim2,
         col = "green4", lwd = lwd * lwdk,
         xlab = "", ylab = "", ...)
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)

    mtext(side = 2, paste("actu. evap.", plotunit), cex = cex.lab, line = line)

    AxisTS(OutputsModel)

    box()
  }


  ## Temp
  if (BOOLPLOT_Temp) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)
    par(new = FALSE, mar = mar)
    ylim1 <- c(+99999, -99999)
    for (iLayer in 1:NLayers) {
      ylim1[1] <- min(ylim1[1], OutputsModel$CemaNeigeLayers[[iLayer]]$Temp)
      ylim1[2] <- max(ylim1[2], OutputsModel$CemaNeigeLayers[[iLayer]]$Temp)
      if (iLayer == 1) {
        SnowPackLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$Temp/NLayers
      } else {
        SnowPackLayerMean <- SnowPackLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$Temp/NLayers
      }
    }
    plot(Xaxis, SnowPackLayerMean, type = "n", ylim = ylim1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    for (iLayer in 1:NLayers) {
      lines(Xaxis, OutputsModel$CemaNeigeLayers[[iLayer]]$Temp, lty = 3, col = "orchid", lwd = lwd * lwdk * 0.8)
    }
    abline(h = 0, col = "grey", lty = 2)
    lines(Xaxis, SnowPackLayerMean, type = "l", lwd = lwd * lwdk * 1.0, col = "darkorchid4")
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)

    mtext(side = 2, expression(paste("temp. [", degree, "C]"), sep = ""),  padj = 0.2, line = line, cex = cex.lab)

    legend("topright", legend = c("mean", "layers"), col = c("darkorchid4", "orchid"),
           lty = c(1, 3), lwd = c(lwd * 1.0, lwd * 0.8),
           bty = "o", bg = bg, box.col = bg, cex = cex.leg)

    AxisTS(OutputsModel)

    box()
  }


  ## SnowPack
  if (BOOLPLOT_SnowPack) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)
    par(new = FALSE, mar = mar)
    ylim1 <- c(+99999, -99999)
    for (iLayer in 1:NLayers) {
      ylim1[1] <- min(ylim1[1], OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack)
      ylim1[2] <- max(ylim1[2], OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack)
      if (iLayer == 1) {
        SnowPackLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack/NLayers
      } else {
        SnowPackLayerMean <- SnowPackLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack/NLayers
      }
    }
    plot(Xaxis, SnowPackLayerMean, type = "l", ylim = ylim1, lwd = lwd * lwdk * 1.2, col = "royalblue", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    for (iLayer in 1:NLayers) {
      lines(Xaxis, OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack, lty = 3, col = "royalblue", lwd = lwd * lwdk * 0.8)
    }
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)

    mtext(side = 2, paste("snow pack", "[mm]"), line = line, cex = cex.lab)
    legend("topright", legend = c("mean", "layers"), col = c("royalblue", "royalblue"),
           lty = c(1, 3), lwd = c(lwd * 1.2, lwd * 0.8),
           bty = "o", bg = bg, box.col = bg, cex = cex.leg)

    AxisTS(OutputsModel)

    box()
  }


  ## Flows
  if (BOOLPLOT_Flows & log_scale) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)
    par(new = FALSE, mar = mar)

    if (BOOL_Qobs) {
      DATA2 <- Qobs
      DATA2[!SelectQobsNotZero] <- mean(Qobs, na.rm = TRUE) / 10000
      DATA2 <- log(DATA2)
    }
    DATA3 <- OutputsModel$Qsim
    DATA3[!SelectQsimNotZero] <- mean(OutputsModel$Qsim, na.rm = TRUE) / 10000
    DATA3 <- log(DATA3)

    ylim1 <- range(DATA3, na.rm = TRUE)
    if (BOOL_Qobs) {
      ylim1 <- range(c(ylim1, DATA2), na.rm = TRUE)
    }
    ylim2 <- c(ylim1[1], 1.1 * ylim1[2])
    plot(Xaxis, rep(NA, length(Xaxis)), type = "n", ylim = ylim2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    txtleg <- NULL
    colleg <- NULL
    if (BOOL_Qobs) {
      lines(Xaxis, DATA2, lwd = lwd * lwdk, lty = 1, col = par("fg"))
      txtleg <- c(txtleg, "observed")
      colleg <- c(colleg, par("fg"))
    }
    if (BOOL_Qsim) {
      lines(Xaxis, DATA3, lwd = lwd * lwdk, lty = 1, col = "orangered")
      txtleg <- c(txtleg, "simulated")
      colleg <- c(colleg, "orangered")
    }
    axis(side = 2, at = seqDATA1, labels = seqDATA2, cex.axis = cex.axis, ...)
    mtext(side = 2, paste("flow", plotunit), line = line, cex = cex.lab)
    if (!is.null(BasinArea)) {
      Factor <- Factor_UNIT_M3S
      axis(side = 4, at = seqDATA1ba, labels = seqDATA2ba, cex.axis = cex.axis, ...)
      mtext(side = 4, paste("flow", "[m3/s]"), line = line, cex = cex.lab)
    }
    AxisTS(OutputsModel)
    legend("topright", txtleg, col = colleg, lty = 1, lwd = lwd * lwdk, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    box()
  }
  if (BOOLPLOT_Flows & !log_scale) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)
    par(new = FALSE, mar = mar)
    ylim1 <- range(OutputsModel$Qsim, na.rm = TRUE)
    if (BOOL_Qobs) {
      ylim1 <- range(c(ylim1, Qobs), na.rm = TRUE)
    }
    ylim2 <- c(ylim1[1], 1.1 * ylim1[2])
    plot(Xaxis, rep(NA, length(Xaxis)), type = "n", ylim = ylim2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    txtleg <- NULL
    colleg <- NULL
    if (BOOL_Qobs) {
      lines(Xaxis, Qobs, lwd = lwd * lwdk, lty = 1, col = par("fg"))
      txtleg <- c(txtleg, "observed")
      colleg <- c(colleg, par("fg"))
    }
    if (BOOL_Qsim) {
      lines(Xaxis, OutputsModel$Qsim, lwd = lwd * lwdk, lty = 1, col = "orangered")
      txtleg <- c(txtleg, "simulated")
      colleg <- c(colleg, "orangered")
    }
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)
    mtext(side = 2, paste("flow", plotunit), line = line, cex = cex.lab)
    if (!is.null(BasinArea)) {
      Factor <- Factor_UNIT_M3S
      axis(side = 4, at = pretty(ylim1 * Factor)/Factor, labels = pretty(ylim1 * Factor), cex.axis = cex.axis, ...)
      mtext(side = 4, paste("flow", "[m3/s]"), line = line, cex = cex.lab)
    }
    AxisTS(OutputsModel)
    legend("topright", txtleg, col = colleg, lty = 1, lwd = lwd * lwdk, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    box()
  }


  ## Error
  if (BOOLPLOT_Error) {
    kPlot <- kPlot + 1
    mar <- c(3, 5, 1, 5)

    if (log_scale) {
      errorQ <- log(OutputsModel$Qsim) - log(Qobs)
    } else {
      errorQ <- OutputsModel$Qsim - Qobs
    }
    par(new = FALSE, mar = mar)
    ylim1 <- range(errorQ[SelectNotZero], na.rm = TRUE)
    plot(Xaxis, errorQ,
         type = "l", xaxt = "n", yaxt = "n", ylim = ylim1,
         col = par("fg"), lwd = lwd * lwdk,
         xlab = "", ylab = "",
         panel.first = abline(h = 0, col = "royalblue"), ...)
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cex.axis, ...)
    mtext(side = 2, paste("flow error", plotunit), cex = cex.lab, line = line)
    if (!is.null(BasinArea)) {
      Factor <- Factor_UNIT_M3S
      axis(side = 4, at = pretty(ylim1 * Factor)/Factor, labels = pretty(ylim1 * Factor), cex.axis = cex.axis, ...)
      mtext(side = 4, paste("flow error", "[m3/s]"), line = line, cex = cex.lab)
    }
    AxisTS(OutputsModel)
    if (log_scale) {
      legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    }
    box()
  }


  ## Regime
  if (BOOLPLOT_Regime) {
    kPlot <- kPlot + 1
    mar <- c(6, 5, 1, 5)
    plotunitregime <- "[mm/month]"
    par(new = FALSE, mar = mar)
    ## Empty plot
    if ((NameTS == "hour"  & length(IndPeriod_Plot) < 697) |
        (NameTS == "day"   & length(IndPeriod_Plot) <  30) |
        (NameTS == "month" & length(IndPeriod_Plot) <   2) |
        (NameTS == "year"  & length(IndPeriod_Plot) <   2)) {
      plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
      mtext(side = 1, text = "", line = line, cex = cex.lab)
      text(0, 0, labels = "NO ENOUGH VALUES", col = "grey40")
      txtlab <- "flow"
      if (BOOL_Pobs) {
        txtlab <- "precip. & flow"
      }
      mtext(side = 2, paste(txtlab, plotunitregime), line = line, cex = cex.lab)
    } else {
      ## Data_formating_as_table
      DataModel <- as.data.frame(matrix(as.numeric(NA), nrow = length(IndPeriod_Plot), ncol = 5))
      DataModel[, 1] <- as.numeric(format(OutputsModel$DatesR, format = "%Y%m%d%H"))
      if (BOOL_Pobs) {
        DataModel[, 2] <- OutputsModel$Precip
      }
      if (BOOL_Psol) {
        DataModel[, 3] <- PsolLayerMean
      }
      if (BOOL_Qobs) {
        DataModel[, 4] <- Qobs
      }
      if (BOOL_Qsim) {
        DataModel[, 5] <- OutputsModel$Qsim
      }
      colnames(DataModel) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
      TxtDatesDataModel <- formatC(DataModel$Dates, format = "d", width = 8, flag = "0")
      ## Building_of_daily_time_series_if_needed
      if (NameTS == "month") {
        DataDaily <- NULL
      }
      if (NameTS == "day") {
        DataDaily <- DataModel
      }
      if (NameTS == "hour" ) {
        DataDaily <- as.data.frame(aggregate(DataModel[, 2:5], by = list(as.numeric(substr(TxtDatesDataModel, 1, 8))), FUN = sum, na.rm = TRUE))
      }
      if (NameTS %in% c("hour", "day")) {
        colnames(DataDaily) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
        TxtDatesDataDaily <- formatC(DataDaily$Dates, format = "d", width = 8, flag = "0")
      }
      ## Building_of_monthly_time_series_if_needed
      if (NameTS == "month") {
        DataMonthly <- DataModel
      }
      if (NameTS == "day"  ) {
        DataMonthly <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily, 1, 6))), FUN = sum, na.rm = TRUE))
      }
      if (NameTS == "hour" ) {
        DataMonthly <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily, 1, 6))), FUN = sum, na.rm = TRUE))
      }
      colnames(DataMonthly) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
      TxtDatesDataMonthly <- formatC(DataMonthly$Dates, format = "d", width = 6, flag = "0")
      ## Computation_of_interannual_mean_series
      if (!is.null(DataDaily)) {
        SeqY <- data.frame(Dates = as.numeric(format(seq(as.Date("1970-01-01", tz = "UTC"),
                                                         as.Date("1970-12-31", tz = "UTC"), "day"),
                                                     format = "%m%d")))
        DataDailyInterAn <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily, 5, 8))), FUN = mean, na.rm = TRUE))
        colnames(DataDailyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
        DataDailyInterAn <- merge(SeqY, DataDailyInterAn, by = "Dates", all.x = TRUE, all.y = FALSE)
      }
      if (!is.null(DataMonthly)) {
        SeqM <- data.frame(Dates = 1:12)
        DataMonthlyInterAn <- as.data.frame(aggregate(DataMonthly[, 2:5], by = list(as.numeric(substr(TxtDatesDataMonthly, 5, 6))), FUN = mean, na.rm = TRUE))
        colnames(DataMonthlyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
        DataMonthlyInterAn <- merge(SeqM, DataMonthlyInterAn, by = "Dates", all.x = TRUE, all.y = FALSE)
      }
      ## Smoothing_of_daily_series_and_scale_conversion_to_make_them_become_a_monthly_regime
      if (!is.null(DataDaily)) {
        ## Smoothing
        NDaysWindow <- 30
        DataDailyInterAn <- as.data.frame(cbind(DataDailyInterAn$Dates,
                                                MyRollMean3(DataDailyInterAn$Precip, NDaysWindow), MyRollMean3(DataDailyInterAn$Psol, NDaysWindow),
                                                MyRollMean3(DataDailyInterAn$Qobs  , NDaysWindow), MyRollMean3(DataDailyInterAn$Qsim, NDaysWindow)))
        colnames(DataDailyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim")
        ## Scale_conversion_to_make_them_become_a_monthly_regime
        if (plotunitregime != "[mm/month]") {
          stop("incorrect unit for regime plot")
        }
        DataDailyInterAn <- as.data.frame(cbind(DataDailyInterAn[1], DataDailyInterAn[2:5]*30))
      }
      ## Plot_preparation
      DataPlotP <- DataMonthlyInterAn
      if (!is.null(DataDaily)) {
        DataPlotQ <- DataDailyInterAn
        SeqX1 <- c(  1, 32, 61,  92, 122, 153, 183, 214, 245, 275, 306, 336, 366)
        SeqX2 <- c( 15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350)
        labX <- "30-days rolling mean"
      } else {
        DataPlotQ <- DataMonthlyInterAn
        SeqX1 <- seq(from = 0.5, to = 12.5, by = 1)
        SeqX2 <- seq(from = 1  , to = 12  , by = 1)
        labX <- ""
      }
      xLabels1 <- rep("", 13)
      xLabels2 <- month.abb
      ylimQ <- range(c(DataPlotQ$Qobs, DataPlotQ$Qsim), na.rm = TRUE)
      if (BOOL_Pobs) {
        ylimP <- c(max(DataPlotP$Precip, na.rm = TRUE), 0)
      }
      txtleg <- NULL
      colleg <- NULL
      lwdleg <- NULL
      lwdP = 10
      ## Plot_forcings
      if (BOOL_Pobs) {
        plot(SeqX2[DataMonthlyInterAn$Dates], DataPlotP$Precip, type = "h",
             xlim = range(SeqX1), ylim = c(3 * ylimP[1], ylimP[2]), lwd = lwdP, lend = 1, lty = 1, col = "royalblue",
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", yaxs = "i", bty = "n", ...)
        txtleg <- c(txtleg, "Ptot" )
        colleg <- c(colleg, "royalblue")
        lwdleg <- c(lwdleg, lwdP/3)
        axis(side = 2, at = pretty(0.8 * ylimP, n = 3), labels = pretty(0.8 * ylimP, n = 3), col.axis = "royalblue", col.ticks = "royalblue", cex.axis = cex.axis, ...)
        par(new = TRUE)
      }
      if (BOOL_Psol) {
        plot(SeqX2, DataPlotP$Psol[DataMonthlyInterAn$Dates], type = "h", xlim = range(SeqX1),
             ylim = c(3 * ylimP[1], ylimP[2]), lwd = lwdP, lend = 1, lty = 1, col = "lightblue",
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", yaxs = "i", bty = "n", ...)
        txtleg <- c(txtleg, "Psol" )
        colleg <- c(colleg, "lightblue")
        lwdleg <- c(lwdleg, lwdP/3)
        par(new = TRUE)
      }
      ## Plot_flows
      plot(NULL, type = "n", xlim = range(SeqX1), ylim = c(ylimQ[1], 2 * ylimQ[2]), xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
      if (BOOL_Qobs) {
        lines(1:nrow(DataPlotQ), DataPlotQ$Qobs, lwd = lwd * lwdk, lty = 1, col = par("fg")  )
        txtleg <- c(txtleg, "Qobs" )
        colleg <- c(colleg, par("fg") )
        lwdleg <- c(lwdleg, lwd)
      }
      if (BOOL_Qsim) {
        lines(1:nrow(DataPlotQ), DataPlotQ$Qsim, lwd = lwd * lwdk, lty = 1, col = "orangered")
        txtleg <- c(txtleg, "Qsim")
        colleg <- c(colleg, "orangered")
        lwdleg <- c(lwdleg, lwd)
      }
      ## Axis_and_legend
      axis(side = 1, at = SeqX1, tick = TRUE , labels = xLabels1, cex.axis = cex.axis, ...)
      axis(side = 1, at = SeqX2, tick = FALSE, labels = xLabels2, cex.axis = cex.axis, ...)
      axis(side = 2, at = pretty(ylimQ), labels = pretty(ylimQ), cex.axis = cex.axis, ...)
      mtext(side = 1, labX, line = line, cex = cex.lab)
      posleg <- "topright"
      txtlab <- "flow"
      if (BOOL_Pobs) {
        posleg <- "right"
        txtlab <- "precip. & flow"
      }
      mtext(side = 2, paste(txtlab, plotunitregime), line = line, cex = cex.lab)
      if (!is.null(BasinArea)) {
        Factor <- Factor_UNIT_M3S / (365.25 / 12)
        axis(side = 4, at = pretty(ylimQ * Factor)/Factor, labels = pretty(ylimQ * Factor), cex.axis = cex.axis, ...)
        mtext(side = 4, paste("flow", "[m3/s]"), line = line, cex = cex.lab)
      }
      box()
    }
  }


  ## Cumulative_frequency
  if (BOOLPLOT_CumFreq) {
    kPlot <- kPlot + 1
    mar <- c(6, 5, 1, 5)
    par(new = FALSE, mar = mar)
    xlim <- c(0, 1)
    if ( BOOL_Qobs & !BOOL_Qsim) {
      # SelectNotZero <- SelectQobsNotZero
      ylim <- range(log(Qobs[SelectNotZero]), na.rm = TRUE)
    }
    if (!BOOL_Qobs &  BOOL_Qsim) {
      # SelectNotZero <- SelectQsimNotZero
      ylim <- range(log(OutputsModel$Qsim[SelectNotZero]), na.rm = TRUE)
    }
    if ( BOOL_Qobs &  BOOL_Qsim) {
      # SelectNotZero <- SelectQobsNotZero & SelectQsimNotZero
      ylim <- range(log(c(Qobs[SelectNotZero], OutputsModel$Qsim[SelectNotZero])), na.rm = TRUE)
    }
    SelectNotZero <- ifelse(is.na(SelectNotZero), FALSE, SelectNotZero)
    if (any(SelectNotZero)) {
      plot(0, 0, type = "n",
           xlim = xlim, ylim = ylim,
           xaxt = "n", yaxt = "n",
           xlab = "", ylab = "", ...)
      axis(side = 1, at = pretty(xlim), labels = pretty(xlim), cex.axis = cex.axis, ...)
      mtext(side = 1, text = "non-exceedance prob. [-]", line = line, cex = cex.lab)
      axis(side = 2, at = seqDATA1, labels = seqDATA2, cex.axis = cex.axis, ...)
      mtext(side = 2, text = paste("flow", plotunit), line = line, cex = cex.lab)
      txtleg <- NULL
      colleg <- NULL
      if (BOOL_Qobs) {
        DATA2 <- log(Qobs[SelectNotZero])
        Fn <- ecdf(DATA2)
        YY <- DATA2
        YY <- YY[order(Fn(DATA2))]
        XX <- Fn(DATA2)
        XX <- XX[order(Fn(DATA2))]
        lines(XX, YY, lwd = lwd, col = par("fg"))
        txtleg <- c(txtleg, "observed")
        colleg <- c(colleg, par("fg"))
      }
      if (BOOL_Qsim) {
        DATA2 <- log(OutputsModel$Qsim[SelectNotZero])
        Fn <- ecdf(DATA2)
        YY <- DATA2
        YY <- YY[order(Fn(DATA2))]
        XX <- Fn(DATA2)
        XX <- XX[order(Fn(DATA2))]
        lines(XX, YY, lwd = lwd, col = "orangered")
        txtleg <- c(txtleg, "simulated")
        colleg <- c(colleg, "orangered")
      }
      if (!is.null(BasinArea)) {
        Factor <- Factor_UNIT_M3S
        axis(side = 4, at = seqDATA1, labels = round(seqDATA2 * Factor, digits = 2), cex.axis = cex.axis, ...)
        mtext(side = 4, paste("flow", "[m3/s]"), line = line, cex = cex.lab)
      }
      legend("topleft", txtleg, col = colleg, lty = 1, lwd = lwd, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
      legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    } else {
      plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
      mtext(side = 1, text = "non-exceedance prob. [-]", line = line, cex = cex.lab)
      mtext(side = 2, text = paste("flow", plotunit), line = line, cex = cex.lab)
      text(0, 0, labels = "NO COMMON DATA", col = "grey40")
    }
    box()
  }


  ## Correlation_QQ
  if (BOOLPLOT_CorQQ) {
    kPlot <- kPlot + 1
    mar <- c(6, 5, 1, 5)
    par(new = FALSE, mar = mar)
    if (any(SelectNotZero)) {
      ylim <- log(range(c(Qobs[SelectQobsNotZero & SelectQsimNotZero], OutputsModel$Qsim[SelectQobsNotZero & SelectQsimNotZero]), na.rm = TRUE))
      plot(log(Qobs[SelectQobsNotZero & SelectQsimNotZero]),
           log(OutputsModel$Qsim[SelectQobsNotZero & SelectQsimNotZero]),
           type = "p", pch = 1, cex = 0.9, col = par("fg"), lwd = lwd,
           xlim = ylim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
      abline(a = 0, b = 1, col = "royalblue", lwd = lwd)
      axis(side = 1, at = seqDATA1, labels = seqDATA2, cex = cex.leg, cex.axis = cex.axis, ...)
      axis(side = 2, at = seqDATA1, labels = seqDATA2, cex = cex.leg, cex.axis = cex.axis, ...)
      mtext(side = 1, paste("observed flow", plotunit), line = line, cex = cex.lab)
      mtext(side = 2, paste("simulated flow", plotunit), line = line, cex = cex.lab)
      if (!is.null(BasinArea)) {
        Factor <- Factor_UNIT_M3S
        axis(side = 4, at = seqDATA1, labels = round(seqDATA2 * Factor, digits = 2), cex.axis = cex.axis, ...)
        mtext(side = 4, paste("simulated flow", "[m3/s]"), line = line, cex = cex.lab)
      }
      legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bg, box.col = bg, cex = cex.leg)
    } else {
      plot(0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
      mtext(side = 1, paste("observed flow", plotunit), line = line, cex = cex.lab)
      mtext(side = 2, paste("simulated flow", plotunit), line = line, cex = cex.lab)
      text(0, 0, labels = "NO COMMON DATA", col = "grey40")
    }
    box()
  }

  ## Empty_plots
  if (exists("iPlotMax")) {
    while (kPlot < iPlotMax) {
      kPlot <- kPlot + 1
      par(new = FALSE)
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, ...)
    }
  }


}

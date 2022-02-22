CreateInputsCrit <- function(FUN_CRIT,
                             InputsModel,
                             RunOptions,
                             Obs,
                             VarObs = "Q",
                             BoolCrit = NULL,
                             transfo = "",
                             Weights = NULL,
                             epsilon = NULL,
                             warnings = TRUE) {


  ObjectClass <- NULL


  ## ---------- check arguments

  ## check 'InputsModel'
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }


  ## length of index of period to be used for the model run
  LLL <- length(InputsModel$DatesR[RunOptions$IndPeriod_Run])


  ## check 'Obs' and definition of idLayer
  if (!is.numeric(unlist(Obs))) {
    stop("'Obs' must be a (list of) vector(s) of numeric values")
  }
  Obs2 <- Obs
  if ("ParamT" %in% VarObs) {
    if (is.list(Obs2)) {
      Obs2[[which(VarObs == "ParamT")]] <- NULL
    } else {
      Obs2 <- NULL
    }
  }
  if (!is.null(Obs2)) {
    vecObs <- unlist(Obs2)
    if (length(vecObs) %% LLL != 0) {
      stop(sprintf("'Obs' must be a (list of) vector(s) of numeric values of length %i", LLL), call. = FALSE)
    }
  }
  if (!is.list(Obs)) {
    idLayer <- list(1L)
    Obs <- list(Obs)
  } else {
    idLayer <- lapply(Obs, function(i) {
      if (is.list(i)) {
        length(i)
      } else {
        1L
      }
    })
    Obs <- lapply(Obs, function(x) rowMeans(as.data.frame(x)))
  }


  ## create list of arguments
  listArgs <- list(FUN_CRIT   = FUN_CRIT,
                   Obs        = Obs,
                   VarObs     = VarObs,
                   BoolCrit   = BoolCrit,
                   idLayer    = idLayer,
                   transfo    = as.character(transfo),
                   Weights    = Weights,
                   epsilon    = epsilon)


  ## check lists lengths
  for (iArgs in names(listArgs)) {
    if (iArgs %in% c("Weights", "BoolCrit", "epsilon")) {
      if (any(is.null(listArgs[[iArgs]]))) {
        listArgs[[iArgs]] <- lapply(seq_along(listArgs$FUN_CRIT), function(x) NULL)
      }
    }
    if (iArgs %in% c("FUN_CRIT", "VarObs", "transfo", "Weights") & length(listArgs[[iArgs]]) > 1L) {
      listArgs[[iArgs]] <- as.list(listArgs[[iArgs]])
    }
    if (!is.list(listArgs[[iArgs]])) {
      listArgs[[iArgs]] <- list(listArgs[[iArgs]])
    }
  }

  ## check 'FUN_CRIT'
  listArgs$FUN_CRIT <- lapply(listArgs$FUN_CRIT, FUN = match.fun)


  ## check 'VarObs'
  if (missing(VarObs)) {
    listArgs$VarObs <- as.list(rep("Q", times = length(listArgs$Obs)))
    # if (warnings) {
    #   warning("'VarObs' automatically set to \"Q\"")
    # }
  }


  ## check 'VarObs' + 'RunOptions'
  if ("Q" %in% VarObs & !inherits(RunOptions, "GR")) {
    stop("'VarObs' cannot contain Q if a GR rainfall-runoff model is not used")
  }
  if (any(c("SCA", "SWE") %in% VarObs) & !inherits(RunOptions, "CemaNeige")) {
    stop("'VarObs' cannot contain SCA or SWE if CemaNeige is not used")
  }
  if ("SCA" %in% VarObs & inherits(RunOptions, "CemaNeige") & !"Gratio"   %in% RunOptions$Outputs_Sim) {
    stop("'Gratio' is missing in 'Outputs_Sim' of 'RunOptions', which is necessary to output SCA with CemaNeige")
  }
  if ("SWE" %in% VarObs & inherits(RunOptions, "CemaNeige") & !"SnowPack" %in% RunOptions$Outputs_Sim) {
    stop("'SnowPack' is missing in 'Outputs_Sim' of 'RunOptions', which is necessary to output SWE with CemaNeige")
  }


  ## check 'transfo'
  if (missing(transfo)) {
    listArgs$transfo <- as.list(rep("", times = length(listArgs$Obs)))
    # if (warnings) {
    #   warning("'transfo' automatically set to \"\"")
    # }
  }

  ## check length of each args
  if (length(unique(sapply(listArgs, FUN = length))) != 1) {
    stopListArgs <- paste(sapply(names(listArgs), shQuote), collapse = ", ")
    stop(sprintf("arguments %s must have the same length", stopListArgs))
  }


  ## check 'RunOptions'
  if (!inherits(RunOptions , "RunOptions")) {
    stop("'RunOptions' must be of class 'RunOptions'")
  }


  ## check 'Weights'
  if (length(listArgs$Weights) > 1 & sum(unlist(listArgs$Weights)) == 0 & !any(sapply(listArgs$Weights, is.null))) {
    stop("sum of 'Weights' cannot be equal to zero")
  }


  ## ---------- reformat

  ## reformat list of arguments
  listArgs2 <- lapply(seq_along(listArgs$FUN_CRIT), function(i) lapply(listArgs, "[[", i))

  ## preparation of warning messages
  inVarObs  <- c("Q", "SCA", "SWE", "ParamT")
  msgVarObs <- "'VarObs' must be a (list of) character vector(s) and one of %s"
  msgVarObs <- sprintf(msgVarObs, paste(sapply(inVarObs, shQuote), collapse = ", "))
  inTransfo  <- c("", "sqrt", "log", "inv", "sort", "boxcox") # pow is not checked by inTransfo, but appears in the warning message and checkef after (see ## check 'transfo')
  msgTransfo <- "'transfo' must be a (list of) character vector(s) and one of %s, or numeric value for power transformation"
  msgTransfo <- sprintf(msgTransfo, paste(sapply(inTransfo, shQuote), collapse = ", "))


  ## ---------- loop on the list of inputs

  InputsCrit <- lapply(listArgs2, function(iListArgs2) {

    ## define FUN_CRIT as a character string
    iListArgs2$FUN_CRIT <- match.fun(iListArgs2$FUN_CRIT)

    ## check 'FUN_CRIT'
    if (!all(class(iListArgs2$FUN_CRIT) == c("FUN_CRIT", "function"))) {
      stop("incorrect 'FUN_CRIT' for use in 'CreateInputsCrit'", call. = FALSE)
    }
    if (identical(iListArgs2$FUN_CRIT, ErrorCrit_RMSE) & length(listArgs$Weights) > 1 & all(!is.null(unlist(listArgs$Weights)))) {
      stop("calculating a composite criterion with the RMSE is not allowed since RMSE is not a dimensionless metric", call. = FALSE)
    }

    ## check 'Obs'
    if (iListArgs2$VarObs == "ParamT") {
      # Parameter for regularisation
      L2 <- RunOptions$FeatFUN_MOD$NbParam
    } else {
      # Observation time series
      L2 <- LLL
    }
    if (!is.vector(iListArgs2$Obs) | length(iListArgs2$Obs) != L2 | !is.numeric(iListArgs2$Obs)) {
      stop(sprintf("'Obs' must be a (list of) vector(s) of numeric values of length %i", L2), call. = FALSE)
    }

    ## check 'BoolCrit'
    if (is.null(iListArgs2$BoolCrit)) {
      iListArgs2$BoolCrit <- rep(TRUE, length(iListArgs2$Obs))
    }
    if (!is.logical(iListArgs2$BoolCrit)) {
      stop("'BoolCrit' must be a (list of) vector(s) of boolean", call. = FALSE)
    }
    if (length(iListArgs2$BoolCrit) != L2) {
      stop("'BoolCrit' and the period defined in 'RunOptions' must have the same length", call. = FALSE)
    }

    ## check 'VarObs'
    if (!is.vector(iListArgs2$VarObs) | length(iListArgs2$VarObs) != 1 | !is.character(iListArgs2$VarObs) | !all(iListArgs2$VarObs %in% inVarObs)) {
      stop(msgVarObs, call. = FALSE)
    }

    ## check 'VarObs' + 'Obs'
    if (any(iListArgs2$VarObs %in% "SCA")) {
      idSCA <- which(iListArgs2$VarObs == "SCA")
      if (length(idSCA) == 1L) {
        vecSCA <- iListArgs2$Obs
      } else {
        vecSCA <- unlist(iListArgs2$Obs[idSCA])
      }
      if (min(vecSCA, na.rm = TRUE) < 0 | max(vecSCA, na.rm = TRUE) > 1) {
        stop("'Obs' outside [0,1] for \"SCA\"", call. = FALSE)
      }
    }
    inPosVarObs <- c("Q", "SWE")
    if (any(iListArgs2$VarObs %in% inPosVarObs)) {
      idQSS <- which(iListArgs2$VarObs %in% inPosVarObs)
      if (length(idQSS) == 1L) {
        vecQSS <- iListArgs2$Obs
      } else {
        vecQSS <- unlist(iListArgs2$Obs[idQSS])
      }
      if (all(is.na(vecQSS))) {
        stop("'Obs' contains only missing values", call. = FALSE)
      }
      if (min(vecQSS, na.rm = TRUE) < 0) {
        stop(sprintf("'Obs' outside [0,Inf[ for \"%s\"", iListArgs2$VarObs), call. = FALSE)
      }
    }


    ## check 'transfo'
    if (is.null(iListArgs2$transfo) | !is.vector(iListArgs2$transfo) | length(iListArgs2$transfo) != 1 | !is.character(iListArgs2$transfo)) {
      stop(msgTransfo, call. = FALSE)
    }
    isNotInTransfo <- !(iListArgs2$transfo %in% inTransfo)
    if (any(isNotInTransfo)) {
      powTransfo <- iListArgs2$transfo[isNotInTransfo]
      powTransfo <- gsub("\\^|[[:alpha:]]", "", powTransfo)
      numExpTransfo <- suppressWarnings(as.numeric(powTransfo))
      if (any(is.na(numExpTransfo))) {
        stop(msgTransfo, call. = FALSE)
      }
      iListArgs2$transfo <- paste0("^", iListArgs2$transfo)
    }

    ## check 'Weights'
    if (!is.null(iListArgs2$Weights)) {
      if (!is.vector(iListArgs2$Weights) | length(iListArgs2$Weights) != 1 | !is.numeric(iListArgs2$Weights) | any(iListArgs2$Weights < 0)) {
        stop("'Weights' must be a single (list of) positive or equal to zero value(s)", call. = FALSE)
      }
    }

    ## check 'epsilon'
    if (!is.null(iListArgs2$epsilon)) {
      if (!is.vector(iListArgs2$epsilon) | length(iListArgs2$epsilon) != 1 | !is.numeric(iListArgs2$epsilon) | any(iListArgs2$epsilon <= 0)) {
        stop("'epsilon' must be a single (list of) positive value(s)", call. = FALSE)
      }
    } else if (iListArgs2$transfo %in% c("log", "inv") & any(iListArgs2$Obs %in% 0) & warnings) {
      warning("zeroes detected in Obs: the corresponding time-steps will be excluded by the 'ErrorCrit*' functions as the epsilon argument was set to NULL", call. = FALSE)
    }

    ## check 'transfo' + 'FUN_CRIT'
    if (iListArgs2$transfo == "log" & warnings) {
      warn_log_kge <- "we do not advise using the %s with a log transformation on Obs (see the details section in the 'CreateInputsCrit' help)"
      if (identical(iListArgs2$FUN_CRIT, ErrorCrit_KGE)) {
        warning(sprintf(warn_log_kge, "KGE"), call. = FALSE)
      }
      if (identical(iListArgs2$FUN_CRIT, ErrorCrit_KGE2)) {
        warning(sprintf(warn_log_kge, "KGE'"), call. = FALSE)
      }
    }


    ## Create InputsCrit
    iInputsCrit <- list(FUN_CRIT   = iListArgs2$FUN_CRIT,
                        Obs        = iListArgs2$Obs,
                        VarObs     = iListArgs2$VarObs,
                        BoolCrit   = iListArgs2$BoolCrit,
                        idLayer    = iListArgs2$idLayer,
                        transfo    = iListArgs2$transfo,
                        epsilon    = iListArgs2$epsilon,
                        Weights    = iListArgs2$Weights)
    class(iInputsCrit) <- c("Single", "InputsCrit", ObjectClass)
    return(iInputsCrit)

  })
  names(InputsCrit) <- paste0("IC", seq_along(InputsCrit))

  listVarObs <- sapply(InputsCrit, FUN = "[[", "VarObs")
  inCnVarObs <- c("SCA", "SWE")
  if (!"ZLayers" %in% names(InputsModel)) {
    if (any(listVarObs %in% inCnVarObs)) {
      stop(sprintf("'VarObs' can not be equal to %i if CemaNeige is not used",
                   paste(sapply(inCnVarObs, shQuote), collapse = " or ")))
    }
  } else {
    listGroupLayer0 <- sapply(InputsCrit, FUN = "[[", "idLayer")
    listGroupLayer <- rep(listVarObs, times = listGroupLayer0)
    tabGroupLayer  <- as.data.frame(table(listGroupLayer))
    colnames(tabGroupLayer) <- c("VarObs", "freq")
    nLayers <- length(InputsModel$ZLayers)
    for (iInCnVarObs in inCnVarObs) {
      if (any(listVarObs %in% iInCnVarObs)) {
        if (tabGroupLayer[tabGroupLayer$VarObs %in% iInCnVarObs, "freq"] != nLayers) {
          stop(sprintf("'Obs' must contain %i vector(s) about %s", nLayers, iInCnVarObs))
        }
      }
    }
  }

  ## define idLayer as an index of the layer to use
  for (iInCnVarObs in unique(listVarObs)) {
    if (!iInCnVarObs %in% inCnVarObs) {
      for (i in which(listVarObs == iInCnVarObs)) {
        InputsCrit[[i]]$idLayer <- NA
      }
    } else {
      aa <- listGroupLayer0[listVarObs == iInCnVarObs]
      aa <- unname(aa)
      bb <- cumsum(c(0, aa[-length(aa)]))
      cc <- lapply(seq_along(aa), function(x) seq_len(aa[x]) + bb[x])
      k <- 1
      for (i in which(listVarObs == iInCnVarObs)) {
        InputsCrit[[i]]$idLayer <- cc[[k]]
        k <- k + 1
      }
    }
  }


  ## if only one criterion --> not a list of InputsCrit but directly an InputsCrit
  if (length(InputsCrit) < 2) {
    InputsCrit <- InputsCrit[[1L]]
    InputsCrit["Weights"] <- list(Weights = NULL)
  } else {
    if (any(sapply(listArgs$Weights, is.null))) {
      for (iListArgs in InputsCrit) {
        iListArgs$Weights <- NULL
      }
      class(InputsCrit) <- c("Multi", "InputsCrit", ObjectClass)
    } else {
      class(InputsCrit) <- c("Compo", "InputsCrit", ObjectClass)
    }
    combInputsCrit <- combn(x = length(InputsCrit), m = 2)
    apply(combInputsCrit, MARGIN = 2, function(i) {
      equalInputsCrit <- identical(InputsCrit[[i[1]]], InputsCrit[[i[2]]])
      if (equalInputsCrit) {
        warning(sprintf("elements %i and %i of the criteria list are identical. This might not be necessary", i[1], i[2]), call. = FALSE)
      }
    })
  }

  return(InputsCrit)

}

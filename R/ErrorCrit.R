ErrorCrit <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {


  ## ---------- Arguments check

  if (!inherits(InputsCrit, "InputsCrit")) {
    stop("InputsCrit must be of class 'InputsCrit'")
  }
  if (!inherits(OutputsModel, "OutputsModel")) {
    stop("OutputsModel must be of class 'OutputsModel'")
  }


  ## ---------- Criterion computation

  ## ----- Single criterion
  if (inherits(InputsCrit, "Single")) {
    FUN_CRIT <- match.fun(InputsCrit$FUN_CRIT)
    OutputsCrit <- FUN_CRIT(InputsCrit = InputsCrit,
                            OutputsModel = OutputsModel,
                            warnings = warnings,
                            verbose = verbose)
  }


  ## ----- Multiple criteria or Composite criterion

  if (inherits(InputsCrit, "Multi") | inherits(InputsCrit, "Compo")) {
    listOutputsCrit <- lapply(InputsCrit, FUN = function(iInputsCrit) {
      FUN_CRIT <- match.fun(iInputsCrit$FUN_CRIT)
      FUN_CRIT(InputsCrit = iInputsCrit,
               OutputsModel = OutputsModel,
               warnings = warnings,
               verbose = verbose)
    })

    listValCrit  <- sapply(listOutputsCrit, function(x) x[["CritValue"]])
    listNameCrit <- sapply(listOutputsCrit, function(x) x[["CritName"]])
    listweights  <- unlist(lapply(InputsCrit, function(x) x[["Weights"]]))
    listweights  <- listweights / sum(listweights)

    if (inherits(InputsCrit, "Compo")) {
      CritValue <- sum(listValCrit * listweights)
      OutputsCritCompo <- list(MultiCritValues  = listValCrit,
                               MultiCritNames   = listNameCrit,
                               MultiCritWeights = listweights)
      OutputsCrit <- list(CritValue       = CritValue,
                          CritName        = "Composite",
                          CritBestValue   = +1,
                          Multiplier      = -1,
                          Ind_notcomputed = NULL,
                          CritCompo       = OutputsCritCompo,
                          MultiCrit       = listOutputsCrit)
      class(OutputsCrit) <- c("Compo", "ErrorCrit")
      if (verbose) {
        message("------------------------------------\n")
        message("Crit. Composite = ", sprintf("%.4f", CritValue))
        msgForm <- paste(sprintf("%.2f", listweights), listNameCrit, sep = " * ", collapse = ", ")
        msgForm <- unlist(strsplit(msgForm, split = ","))
        msgFormSep <- rep(c(",", ",", ",\n\t\t    "), times = ceiling(length(msgForm)/3))[1:length(msgForm)]
        msgForm <- paste(msgForm, msgFormSep, sep = "", collapse = "")
        msgForm <- gsub("\\,\\\n\\\t\\\t    $|\\,$", "", msgForm)
        message("\tFormula: sum(", msgForm, ")\n")
      }
    } else {
      OutputsCrit <- listOutputsCrit
      class(OutputsCrit) <- c("Multi", "ErrorCrit")
    }

  }

  return(OutputsCrit)

}


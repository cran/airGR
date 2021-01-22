
## =================================================================================
## function to check
## =================================================================================

# .onLoad <- function(libname, pkgname){
#   if (requireNamespace("airGRteaching", quietly = TRUE)) {
#     if (packageVersion("airGRteaching") %in% package_version(c("0.2.0.9", "0.2.2.2", "0.2.3.2"))) {
#       packageStartupMessage("In order to be compatible with the present version of 'airGR', please update your version of the 'airGRteaching' package.")
#     }
#   }
# }



## =================================================================================
## function to manage Fortran outputs
## =================================================================================

.FortranOutputs <- function(GR = NULL, isCN = FALSE) {

  outGR <- NULL
  outCN <- NULL

  if (is.null(GR)) {
    GR <- ""
  }
  if (GR == "GR1A") {
    outGR <- c("PotEvap", "Precip",
               "Qsim")
  } else if (GR == "GR2M") {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn", "Ps",
               "AE",
               "Perc", "PR",
               "Rout",
               "AExch",
               "Qsim")
  } else if (GR == "GR5H") {
    outGR <- c("PotEvap", "Precip", "Interc", "Prod", "Pn", "Ps",
               "AE", "EI", "ES",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch",
               "AExch1", "AExch2",
               "AExch", "QR",
               "QD",
               "Qsim")
  } else if (GR %in% c("GR4J", "GR5J", "GR4H")) {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn", "Ps",
               "AE",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch",
               "AExch1", "AExch2",
               "AExch", "QR",
               "QD",
               "Qsim")
  } else if (GR == "GR6J") {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn", "Ps",
               "AE",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch",
               "AExch1", "AExch2",
               "AExch", "QR",
               "QRExp", "Exp",
               "QD",
               "Qsim")
  }
  if (isCN) {
    outCN <- c("Pliq", "Psol",
               "SnowPack", "ThermalState", "Gratio",
               "PotMelt", "Melt", "PliqAndMelt", "Temp",
               "Gthreshold", "Glocalmax")
  }

  res <- list(GR = outGR, CN = outCN)

}



## =================================================================================
## functions to extract parts of InputsModel or OutputsModel objects
## =================================================================================

## InputsModel

.ExtractInputsModel <- function(x, i) {
  res <- lapply(x, function(x) {
    if (is.matrix(x)) {
      res0 <- x[i, , drop = FALSE]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      if (inherits(x, "OutputsModel")) {
        res0 <- .ExtractOutputsModel(x = x, i = i)
      } else {
        res0 <- .ExtractInputsModel(x = x, i = i)
      }
    }
    return(res0)
  })
  if (!is.null(x$ZLayers)) {
    res$ZLayers <- x$ZLayers
  }
  if (inherits(x, "SD")) {
    res$LengthHydro <- x$LengthHydro
    res$BasinAreas  <- x$BasinAreas
  }
  class(res) <- class(x)
  res
}

'[.InputsModel' <- function(x, i) {
  if (!inherits(x, "InputsModel")) {
    stop("'x' must be of class 'InputsModel'")
  }
  if (is.factor(i)) {
    i <- as.character(i)
  }
  if (is.numeric(i)) {
    .ExtractInputsModel(x, i)
  } else {
    NextMethod()
  }
}


## OutputsModel

.ExtractOutputsModel <- function(x, i) {
  res <- lapply(x, function(x) {
    if (is.matrix(x)  && length(dim(x)) == 2L) {
      res0 <- x[i, ]
    }
    if (is.array(x) && length(dim(x)) == 3L) {
      res0 <- x[i, , ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractOutputsModel(x = x, i = i)
    }
    return(res0)
  })
  if (!is.null(x$StateEnd)) {
    res$StateEnd <- x$StateEnd
  }
  class(res) <- class(x)
  res
}

# '[.OutputsModel' <- function(x, i) {
#   if (!inherits(x, "OutputsModel")) {
#     stop("'x' must be of class 'OutputsModel'")
#   }
#   if (is.factor(i)) {
#     i <- as.character(i)
#   }
#   if (is.numeric(i)) {
#     .ExtractOutputsModel(x, i)
#   } else {
#     NextMethod()
#   }
# }

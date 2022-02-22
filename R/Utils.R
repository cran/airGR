
## =================================================================================
## function to check
## =================================================================================

# .onLoad <- function(libname, pkgname) {
#   if (requireNamespace("airGRteaching", quietly = TRUE)) {
#     if (packageVersion("airGRteaching") %in% package_version(c("0.2.0.9", "0.2.2.2", "0.2.3.2"))) {
#       packageStartupMessage("In order to be compatible with the present version of 'airGR', please update your version of the 'airGRteaching' package.")
#     }
#   }
# }



## =================================================================================
## function to extract model features
## =================================================================================

## table of feature models
.FeatModels <- function() {
  path <- system.file("modelsFeatures/FeatModelsGR.csv", package = "airGR")
  read.table(path, header = TRUE, sep = ";", stringsAsFactors = FALSE)
}


## function to extract model features
.GetFeatModel <- function(FUN_MOD, DatesR = NULL) {
  FeatMod <- .FeatModels()
  NameFunMod <- ifelse(test = FeatMod$Pkg %in% "airGR",
                       yes  = paste("RunModel", FeatMod$NameMod, sep = "_"),
                       no   = FeatMod$NameMod)
  FunMod <- lapply(NameFunMod, FUN = match.fun)
  IdMod <- which(sapply(FunMod, FUN = function(x) identical(FUN_MOD, x)))
  if (length(IdMod) < 1) {
    stop("'FUN_MOD' must be one of ", paste(NameFunMod, collapse = ", "))
  } else {
    res <- as.list(FeatMod[IdMod, ])
    res$NameFunMod <- NameFunMod[IdMod]
    if (!is.null(DatesR)) {
      DiffTimeStep <- as.numeric(difftime(DatesR[length(DatesR)],
                                          DatesR[length(DatesR)-1],
                                          units = "secs"))
      if (is.na(res$TimeUnit)) {
        if (any(DiffTimeStep %in% 3600:3601)) { # 3601: leap second
          res$TimeUnit <- "hourly"
        } else {
          res$TimeUnit <- "daily"
        }
      }
    }
    res$TimeStep <- switch(res$TimeUnit,
                           hourly  =       1,
                           daily   =       1 * 24,
                           monthly =   28:31 * 24,
                           yearly  = 365:366 * 24)
    res$TimeStepMean <- switch(res$TimeUnit,
                               hourly  =           1,
                               daily   =           1 * 24,
                               monthly = 365.25 / 12 * 24,
                               yearly  =      365.25 * 24)
    res$TimeStep     <- res$TimeStep * 3600
    res$TimeStepMean <- as.integer(res$TimeStepMean * 3600)
    res$Class <- c(res$TimeUnit, res$Class)
    res$CodeModHydro <- res$CodeMod
    if (grepl("CemaNeige", res$NameFunMod)) {
      res$Class <- c(res$Class, "CemaNeige")
      res$CodeModHydro <- gsub("CemaNeige", "", res$CodeMod)
    }
    res$Class <- res$Class[!is.na(res$Class)]
    if (!is.null(DatesR)) {
      if (all(DiffTimeStep != res$TimeStep)) {
        stop("the time step of the model inputs must be ", res$TimeUnit)
      }
    }
    return(res)
  }
}



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
  if (!is.null(x$RunOptions)) {
    res$RunOptions <- x$RunOptions
  }
  if (!is.null(x$StateEnd)) {
    res$StateEnd <- x$StateEnd
  }
  class(res) <- class(x)
  res
}

.IndexOutputsModel <- function(x, i) {
  # '[.OutputsModel' <- function(x, i) {
  if (!inherits(x, "OutputsModel")) {
    stop("'x' must be of class 'OutputsModel'")
  }
  if (is.factor(i)) {
    i <- as.character(i)
  }
  if (is.numeric(i)) {
    .ExtractOutputsModel(x, i)
  } else {
    NextMethod()
  }
}



## =================================================================================
## function to try to set local time in English
## =================================================================================

.TrySetLcTimeEN <- function() {
  locale <- list("English_United Kingdom",
                 "en_US",
                 "en_US.UTF-8",
                 "en_US.utf8",
                 "en")
  dateTest <- as.POSIXct("2000-02-15", tz = "UTC", format = "%Y-%m-%d")
  monthTestTarget <- "February"
  monthTest <- function() {
    format(dateTest, format = "%B")
  }
  lapply(locale, function(x) {
    if (monthTest() != monthTestTarget) {
      Sys.setlocale(category = "LC_TIME", locale = x)
    }
  })
}

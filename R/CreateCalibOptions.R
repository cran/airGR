CreateCalibOptions <- function(FUN_MOD,
                               FUN_CALIB = Calibration_Michel,
                               FUN_TRANSFO = NULL,
                               IsHyst = FALSE,
                               IsSD = FALSE,
                               FixedParam = NULL,
                               SearchRanges = NULL,
                               StartParamList = NULL,
                               StartParamDistrib = NULL) {

  ObjectClass <- NULL

  FUN_MOD     <- match.fun(FUN_MOD)
  FUN_CALIB   <- match.fun(FUN_CALIB)
  if(!is.null(FUN_TRANSFO)) {
    FUN_TRANSFO <- match.fun(FUN_TRANSFO)
  }
  if (!is.logical(IsHyst) | length(IsHyst) != 1L) {
    stop("'IsHyst' must be a logical of length 1")
  }
  if (!is.logical(IsSD) | length(IsSD) != 1L) {
    stop("'IsSD' must be a logical of length 1")
  }
  ##check_FUN_MOD
  BOOL <- FALSE

  if (identical(FUN_MOD, RunModel_GR4H)) {
    ObjectClass <- c(ObjectClass, "GR4H")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR5H)) {
    ObjectClass <- c(ObjectClass, "GR5H")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR4J)) {
    ObjectClass <- c(ObjectClass, "GR4J")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR5J)) {
    ObjectClass <- c(ObjectClass, "GR5J")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR6J)) {
    ObjectClass <- c(ObjectClass, "GR6J")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR2M)) {
    ObjectClass <- c(ObjectClass, "GR2M")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_GR1A)) {
    ObjectClass <- c(ObjectClass, "GR1A")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeige)) {
    ObjectClass <- c(ObjectClass, "CemaNeige")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeigeGR4H)) {
    ObjectClass <- c(ObjectClass, "CemaNeigeGR4H")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeigeGR5H)) {
    ObjectClass <- c(ObjectClass, "CemaNeigeGR5H")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeigeGR4J)) {
    ObjectClass <- c(ObjectClass, "CemaNeigeGR4J")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeigeGR5J)) {
    ObjectClass <- c(ObjectClass, "CemaNeigeGR5J")
    BOOL <- TRUE
  }
  if (identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
    ObjectClass <- c(ObjectClass, "CemaNeigeGR6J")
    BOOL <- TRUE
  }
  if (IsHyst) {
    ObjectClass <- c(ObjectClass, "hysteresis")
  }
  if (IsSD) {
    ObjectClass <- c(ObjectClass, "SD")
  }
  if (!BOOL) {
    stop("incorrect 'FUN_MOD' for use in 'CreateCalibOptions'")
    return(NULL)
  }

  ##check_FUN_CALIB
  BOOL <- FALSE

  if (identical(FUN_CALIB, Calibration_Michel)) {
    ObjectClass <- c(ObjectClass, "HBAN")
    BOOL <- TRUE
  }
  if (!BOOL) {
    stop("incorrect 'FUN_CALIB' for use in 'CreateCalibOptions'")
    return(NULL)

  }

  ##check_FUN_TRANSFO
  if (is.null(FUN_TRANSFO)) {
    ##_set_FUN1
    if (identical(FUN_MOD, RunModel_GR4H) |
        identical(FUN_MOD, RunModel_CemaNeigeGR4H)) {
      FUN_GR <- TransfoParam_GR4H
    }
    if (identical(FUN_MOD, RunModel_GR5H) |
        identical(FUN_MOD, RunModel_CemaNeigeGR5H)) {
      FUN_GR <- TransfoParam_GR5H
    }
    if (identical(FUN_MOD, RunModel_GR4J) |
        identical(FUN_MOD, RunModel_CemaNeigeGR4J)) {
      FUN_GR <- TransfoParam_GR4J
    }
    if (identical(FUN_MOD, RunModel_GR5J) |
        identical(FUN_MOD, RunModel_CemaNeigeGR5J)) {
      FUN_GR <- TransfoParam_GR5J
    }
    if (identical(FUN_MOD, RunModel_GR6J) |
        identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
      FUN_GR <- TransfoParam_GR6J
    }
    if (identical(FUN_MOD, RunModel_GR2M)) {
      FUN_GR <- TransfoParam_GR2M
    }
    if (identical(FUN_MOD, RunModel_GR1A)) {
      FUN_GR <- TransfoParam_GR1A
    }
    if (identical(FUN_MOD, RunModel_CemaNeige)) {
      if (IsHyst) {
        FUN_GR <- TransfoParam_CemaNeigeHyst
      } else {
        FUN_GR <- TransfoParam_CemaNeige
      }
    }
    if (is.null(FUN_GR)) {
      stop("'FUN_GR' was not found")
      return(NULL)
    }
    ##_set_FUN2
    if (IsHyst) {
      FUN_SNOW <- TransfoParam_CemaNeigeHyst
    } else {
      FUN_SNOW <- TransfoParam_CemaNeige
    }
    ##_set_FUN_LAG
    if (IsSD) {
      FUN_LAG <- TransfoParam_Lag
    }
    ##_set_FUN_TRANSFO
    if (sum(ObjectClass %in% c("GR4H", "GR5H", "GR4J", "GR5J", "GR6J", "GR2M", "GR1A", "CemaNeige")) > 0) {
      if (!IsSD) {
        FUN_TRANSFO <- FUN_GR
      } else {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (!Bool) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          ParamOut[, 2:NParam] <- FUN_GR(ParamIn[, 2:NParam], Direction)
          ParamOut[, 1       ] <- FUN_LAG(as.matrix(ParamIn[, 1]), Direction)
          if (!Bool) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
    } else {
      if (IsHyst & !IsSD) {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (!Bool) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          ParamOut[, 1:(NParam - 4)     ] <- FUN_GR(ParamIn[, 1:(NParam - 4)     ], Direction)
          ParamOut[, (NParam - 3):NParam] <- FUN_SNOW(ParamIn[, (NParam - 3):NParam], Direction)
          if (!Bool) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
      if (!IsHyst & !IsSD) {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (!Bool) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          if (NParam <= 3) {
            ParamOut[, 1:(NParam - 2)] <- FUN_GR(cbind(ParamIn[, 1:(NParam - 2)]), Direction)
          } else {
            ParamOut[, 1:(NParam - 2)] <- FUN_GR(      ParamIn[, 1:(NParam - 2)],  Direction)
          }
          ParamOut[, (NParam - 1):NParam] <- FUN_SNOW(ParamIn[, (NParam - 1):NParam], Direction)
          if (!Bool) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
      if (IsHyst & IsSD) {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (!Bool) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          ParamOut[, 2:(NParam - 4)     ] <- FUN_GR(   ParamIn[, 2:(NParam - 4)     ], Direction)
          ParamOut[, (NParam - 3):NParam] <- FUN_SNOW(   ParamIn[, (NParam - 3):NParam], Direction)
          ParamOut[, 1                  ] <- FUN_LAG(as.matrix(ParamIn[, 1       ]), Direction)
          if (!Bool) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
      if (!IsHyst & IsSD) {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (!Bool) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          if (NParam <= 3) {
            ParamOut[, 2:(NParam - 2)] <- FUN_GR(cbind(ParamIn[, 2:(NParam - 2)]), Direction)
          } else {
            ParamOut[, 2:(NParam - 2)] <- FUN_GR(      ParamIn[, 2:(NParam - 2)],  Direction)
          }
          ParamOut[, (NParam - 1):NParam] <- FUN_SNOW(   ParamIn[, (NParam - 1):NParam], Direction)
          ParamOut[, 1                  ] <- FUN_LAG(as.matrix(ParamIn[, 1]), Direction)
          if (!Bool) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
    }
  }
  if (is.null(FUN_TRANSFO)) {
    stop("'FUN_TRANSFO' was not found")
    return(NULL)
  }

  ##NParam
  if ("GR4H" %in% ObjectClass) {
    NParam <- 4
  }
  if ("GR5H" %in% ObjectClass) {
    NParam <- 5
  }
  if ("GR4J" %in% ObjectClass) {
    NParam <- 4
  }
  if ("GR5J" %in% ObjectClass) {
    NParam <- 5
  }
  if ("GR6J" %in% ObjectClass) {
    NParam <- 6
  }
  if ("GR2M" %in% ObjectClass) {
    NParam <- 2
  }
  if ("GR1A" %in% ObjectClass) {
    NParam <- 1
  }
  if ("CemaNeige" %in% ObjectClass) {
    NParam <- 2
  }
  if ("CemaNeigeGR4H" %in% ObjectClass) {
    NParam <- 6
  }
  if ("CemaNeigeGR5H" %in% ObjectClass) {
    NParam <- 7
  }
  if ("CemaNeigeGR4J" %in% ObjectClass) {
    NParam <- 6
  }
  if ("CemaNeigeGR5J" %in% ObjectClass) {
    NParam <- 7
  }
  if ("CemaNeigeGR6J" %in% ObjectClass) {
    NParam <- 8
  }
  if (IsHyst) {
    NParam <- NParam + 2
  }
  if (IsSD) {
    NParam <- NParam + 1
  }

  ##check_FixedParam
  if (is.null(FixedParam)) {
    FixedParam <- rep(NA, NParam)
  } else {
    if (!is.vector(FixedParam)) {
      stop("FixedParam must be a vector")
    }
    if (length(FixedParam) != NParam) {
      stop("Incompatibility between 'FixedParam' length and 'FUN_MOD'")
    }
    if (all(!is.na(FixedParam))) {
      stop("At least one parameter must be not set (NA)")
    }
    if (all(is.na(FixedParam))) {
      warning("You have not set any parameter in 'FixedParam'")
    }
  }

  ##check_SearchRanges
  if (is.null(SearchRanges)) {
    ParamT <-  matrix(c(rep(-9.99, NParam), rep(+9.99, NParam)),
                      ncol = NParam, byrow = TRUE)
    SearchRanges <- TransfoParam(ParamIn = ParamT, Direction = "TR", FUN_TRANSFO = FUN_TRANSFO)

  } else {
    if (!is.matrix(SearchRanges)) {
      stop("'SearchRanges' must be a matrix")
    }
    if (!is.numeric(SearchRanges)) {
      stop("'SearchRanges' must be a matrix of numeric values")
    }
    if (sum(is.na(SearchRanges)) != 0) {
      stop("'SearchRanges' must not include NA values")
    }
    if (nrow(SearchRanges) != 2) {
      stop("'SearchRanges' must have 2 rows")
    }
    if (ncol(SearchRanges) != NParam) {
      stop("Incompatibility between 'SearchRanges' ncol and 'FUN_MOD'")
    }
  }

  ##check_StartParamList_and_StartParamDistrib__default_values
  if (("HBAN"  %in% ObjectClass & is.null(StartParamList) & is.null(StartParamDistrib))) {
    if ("GR4H" %in% ObjectClass) {
      ParamT <- matrix(c(+5.12, -1.18, +4.34, -9.69,
                         +5.58, -0.85, +4.74, -9.47,
                         +6.01, -0.50, +5.14, -8.87), ncol = 4, byrow = TRUE)
    }
    if (("GR5H" %in% ObjectClass) & ("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.46, -1.25, +4.04, -9.53, -9.34,
                         +3.74, -0.41, +4.78, -8.94, -3.33,
                         +4.29, +0.16, +5.39, -7.39, +3.33), ncol=5, byrow = TRUE);
    }
    if (("GR5H" %in% ObjectClass) & !("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.28, -0.39, +4.14, -9.54, -7.49,
                         +3.62, -0.19, +4.80, -9.00, -6.31,
                         +4.01, -0.04, +5.43, -7.53, -5.33), ncol=5, byrow = TRUE);
    }
    if ("GR4J" %in% ObjectClass) {
      ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05,
                         +5.51, -0.61, +3.74, -8.51,
                         +6.07, -0.02, +4.42, -8.06), ncol = 4, byrow = TRUE)
    }
    if ("GR5J" %in% ObjectClass) {
      ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45,
                         +5.55, -0.46, +3.75, -9.09, -4.69,
                         +6.10, -0.11, +4.43, -8.60, -0.66), ncol = 5, byrow = TRUE)

    }
    if ("GR6J" %in% ObjectClass) {
      ParamT <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00,
                         +3.90, -0.50, +4.10, -8.70, +0.10, +4.00,
                         +4.50, +0.50, +5.00, -8.10, +1.10, +5.00), ncol = 6, byrow = TRUE)
    }
    if ("GR2M" %in% ObjectClass) {
      ParamT <- matrix(c(+5.03, -7.15,
                         +5.22, -6.74,
                         +5.85, -6.37), ncol = 2, byrow = TRUE)
    }
    if ("GR1A" %in% ObjectClass) {
      ParamT <- matrix(c(-1.69,
                         -0.38,
                         +1.39), ncol = 1, byrow = TRUE)
    }


    if ("CemaNeige" %in% ObjectClass) {
      ParamT <- matrix(c(-9.96, +6.63,
                         -9.14, +6.90,
                         +4.10, +7.21), ncol = 2, byrow = TRUE)
    }
    if ("CemaNeigeGR4H" %in% ObjectClass) {
      ParamT <- matrix(c(+5.12, -1.18, +4.34, -9.69, -9.96, +6.63,
                         +5.58, -0.85, +4.74, -9.47, -9.14, +6.90,
                         +6.01, -0.50, +5.14, -8.87, +4.10, +7.21), ncol = 6, byrow = TRUE)
    }
    if (("CemaNeigeGR5H" %in% ObjectClass) & ("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.46, -1.25, +4.04, -9.53, -9.34, -9.96, +6.63,
                         +3.74, -0.41, +4.78, -8.94, -3.33, -9.14, +6.90,
                         +4.29, +0.16, +5.39, -7.39, +3.33, +4.10, +7.21), ncol = 7, byrow = TRUE);
    }
    if (("CemaNeigeGR5H" %in% ObjectClass) & !("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.28, -0.39, +4.14, -9.54, -7.49, -9.96, +6.63,
                         +3.62, -0.19, +4.80, -9.00, -6.31, -9.14, +6.90,
                         +4.01, -0.04, +5.43, -7.53, -5.33, +4.10, +7.21), ncol = 7, byrow = TRUE);
    }
    if ("CemaNeigeGR4J" %in% ObjectClass) {
      ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05, -9.96, +6.63,
                         +5.51, -0.61, +3.74, -8.51, -9.14, +6.90,
                         +6.07, -0.02, +4.42, -8.06, +4.10, +7.21), ncol = 6, byrow = TRUE)
    }
    if ("CemaNeigeGR5J" %in% ObjectClass) {
      ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45, -9.96, +6.63,
                         +5.55, -0.46, +3.75, -9.09, -4.69, -9.14, +6.90,
                         +6.10, -0.11, +4.43, -8.60, -0.66, +4.10, +7.21), ncol = 7, byrow = TRUE)
    }
    if ("CemaNeigeGR6J" %in% ObjectClass) {
      ParamT <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00, -9.96, +6.63,
                         +3.90, -0.50, +4.10, -8.70, +0.10, +4.00, -9.14, +6.90,
                         +4.50, +0.50, +5.00, -8.10, +1.10, +5.00, +4.10, +7.21), ncol = 8, byrow = TRUE)
    }

    if (IsHyst) {
      ParamTHyst <- matrix(c(-7.00, -7.00,
                             -0.00, -0.00,
                             +7.00, +7.00), ncol = 2, byrow = TRUE)
      ParamT <- cbind(ParamT, ParamTHyst)
    }
    if (IsSD) {
      ParamTSD <- matrix(c(+1.25,
                           +2.50,
                           +5.00), ncol = 1, byrow = TRUE)
      ParamT <- cbind(ParamTSD, ParamT)
    }

    StartParamList    <- NULL
    StartParamDistrib <- TransfoParam(ParamIn = ParamT, Direction = "TR", FUN_TRANSFO = FUN_TRANSFO)

  }

  ##check_StartParamList_and_StartParamDistrib__format
  if ("HBAN" %in% ObjectClass & !is.null(StartParamList)) {
    if (!is.matrix(StartParamList)) {
      stop("'StartParamList' must be a matrix")
    }
    if (!is.numeric(StartParamList)) {
      stop("'StartParamList' must be a matrix of numeric values")
    }
    if (sum(is.na(StartParamList)) != 0) {
      stop("'StartParamList' must not include NA values")
    }
    if (ncol(StartParamList) != NParam) {
      stop("Incompatibility between 'StartParamList' ncol and 'FUN_MOD'")
    }
  }
  if ("HBAN" %in% ObjectClass & !is.null(StartParamDistrib)) {
    if (!is.matrix(StartParamDistrib)) {
      stop("'StartParamDistrib' must be a matrix")
    }
    if (!is.numeric(StartParamDistrib[1, ])) {
      stop("'StartParamDistrib' must be a matrix of numeric values")
    }
    if (sum(is.na(StartParamDistrib[1, ])) != 0) {
      stop("'StartParamDistrib' must not include NA values on the first line")
    }
    if (ncol(StartParamDistrib) != NParam) {
      stop("Incompatibility between 'StartParamDistrib' ncol and 'FUN_MOD'")
    }
  }


  ##Create_CalibOptions
  CalibOptions <- list(FixedParam = FixedParam, SearchRanges = SearchRanges, FUN_TRANSFO = FUN_TRANSFO)

  if (!is.null(StartParamList)) {
    CalibOptions <- c(CalibOptions, list(StartParamList = StartParamList))
  }
  if (!is.null(StartParamDistrib)) {
    CalibOptions <- c(CalibOptions, list(StartParamDistrib = StartParamDistrib))
  }
  class(CalibOptions) <- c("CalibOptions", ObjectClass)

  return(CalibOptions)

}

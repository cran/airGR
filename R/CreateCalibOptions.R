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
  if (!is.null(FUN_TRANSFO)) {
    FUN_TRANSFO <- match.fun(FUN_TRANSFO)
  }
  if (!is.logical(IsHyst) | length(IsHyst) != 1L) {
    stop("'IsHyst' must be a logical of length 1")
  }
  if (!is.logical(IsSD) | length(IsSD) != 1L) {
    stop("'IsSD' must be a logical of length 1")
  }

  ## check FUN_MOD
  FeatFUN_MOD <- .GetFeatModel(FUN_MOD = FUN_MOD)
  FeatFUN_MOD$IsHyst <- IsHyst
  FeatFUN_MOD$IsSD <- IsSD
  ObjectClass <- FeatFUN_MOD$Class

  if (identical(FUN_MOD, RunModel_Lag) && IsSD) {
      stop("RunModel_Lag should not be used with 'isSD=TRUE'")
  }
  if (IsHyst) {
    ObjectClass <- c(ObjectClass, "hysteresis")
  }
  if (IsSD) {
    ObjectClass <- c(ObjectClass, "SD")
  }

  ## check FUN_CALIB
  BOOL <- FALSE

  if (identical(FUN_CALIB, Calibration_Michel)) {
    ObjectClass <- c(ObjectClass, "HBAN")
    BOOL <- TRUE
  }
  if (!BOOL) {
    stop("incorrect 'FUN_CALIB' for use in 'CreateCalibOptions'")
    return(NULL)

  }

  ## check FUN_TRANSFO
  if (is.null(FUN_TRANSFO)) {
    FUN_TRANSFO <- .FunTransfo(FeatFUN_MOD)
  }

  ## NParam
  NParam <- FeatFUN_MOD$NbParam

  if (IsHyst) {
    NParam <- NParam + 2
  }
  if (IsSD) {
    NParam <- NParam + 1
  }

  ## check FixedParam
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

  ## check SearchRanges
  if (is.null(SearchRanges)) {
    ParamT <- matrix(c(rep(-9.99, NParam), rep(+9.99, NParam)),
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

  ## check StartParamList and StartParamDistrib default values
  if (("HBAN"  %in% ObjectClass & is.null(StartParamList) & is.null(StartParamDistrib))) {
    if ("GR4H" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.12, -1.18, +4.34, -9.69,
                         +5.58, -0.85, +4.74, -9.47,
                         +6.01, -0.50, +5.14, -8.87), ncol = 4, byrow = TRUE)
    }
    if (("GR5H" == FeatFUN_MOD$CodeMod) & ("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.46, -1.25, +4.04, -9.53, -9.34,
                         +3.74, -0.41, +4.78, -8.94, -3.33,
                         +4.29, +0.16, +5.39, -7.39, +3.33), ncol = 5, byrow = TRUE)
    }
    if (("GR5H" == FeatFUN_MOD$CodeMod) & !("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.28, -0.39, +4.14, -9.54, -7.49,
                         +3.62, -0.19, +4.80, -9.00, -6.31,
                         +4.01, -0.04, +5.43, -7.53, -5.33), ncol = 5, byrow = TRUE)
    }
    if ("GR4J" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05,
                         +5.51, -0.61, +3.74, -8.51,
                         +6.07, -0.02, +4.42, -8.06), ncol = 4, byrow = TRUE)
    }
    if ("GR5J" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45,
                         +5.55, -0.46, +3.75, -9.09, -4.69,
                         +6.10, -0.11, +4.43, -8.60, -0.66), ncol = 5, byrow = TRUE)

    }
    if ("GR6J" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00,
                         +3.90, -0.50, +4.10, -8.70, +0.10, +4.00,
                         +4.50, +0.50, +5.00, -8.10, +1.10, +5.00), ncol = 6, byrow = TRUE)
    }
    if ("GR2M" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.03, -7.15,
                         +5.22, -6.74,
                         +5.85, -6.37), ncol = 2, byrow = TRUE)
    }
    if ("GR1A" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(-1.69,
                         -0.38,
                         +1.39), ncol = 1, byrow = TRUE)
    }


    if ("CemaNeige" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(-9.96, +6.63,
                         -9.14, +6.90,
                         +4.10, +7.21), ncol = 2, byrow = TRUE)
    }
    if ("CemaNeigeGR4H" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.12, -1.18, +4.34, -9.69, -9.96, +6.63,
                         +5.58, -0.85, +4.74, -9.47, -9.14, +6.90,
                         +6.01, -0.50, +5.14, -8.87, +4.10, +7.21), ncol = 6, byrow = TRUE)
    }
    if (("CemaNeigeGR5H" == FeatFUN_MOD$CodeMod) & ("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.46, -1.25, +4.04, -9.53, -9.34, -9.96, +6.63,
                         +3.74, -0.41, +4.78, -8.94, -3.33, -9.14, +6.90,
                         +4.29, +0.16, +5.39, -7.39, +3.33, +4.10, +7.21), ncol = 7, byrow = TRUE)
    }
    if (("CemaNeigeGR5H" == FeatFUN_MOD$CodeMod) & !("interception" %in% ObjectClass)) {
      ParamT <- matrix(c(+3.28, -0.39, +4.14, -9.54, -7.49, -9.96, +6.63,
                         +3.62, -0.19, +4.80, -9.00, -6.31, -9.14, +6.90,
                         +4.01, -0.04, +5.43, -7.53, -5.33, +4.10, +7.21), ncol = 7, byrow = TRUE)
    }
    if ("CemaNeigeGR4J" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05, -9.96, +6.63,
                         +5.51, -0.61, +3.74, -8.51, -9.14, +6.90,
                         +6.07, -0.02, +4.42, -8.06, +4.10, +7.21), ncol = 6, byrow = TRUE)
    }
    if ("CemaNeigeGR5J" == FeatFUN_MOD$CodeMod) {
      ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45, -9.96, +6.63,
                         +5.55, -0.46, +3.75, -9.09, -4.69, -9.14, +6.90,
                         +6.10, -0.11, +4.43, -8.60, -0.66, +4.10, +7.21), ncol = 7, byrow = TRUE)
    }
    if ("CemaNeigeGR6J" == FeatFUN_MOD$CodeMod) {
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
      ParamTSD <- matrix(c(-8.75,
                           -7.50,
                           -5.00), ncol = 1, byrow = TRUE)
      ParamT <- cbind(ParamTSD, ParamT)
    }

    StartParamList    <- NULL
    StartParamDistrib <- TransfoParam(ParamIn = ParamT, Direction = "TR", FUN_TRANSFO = FUN_TRANSFO)

  }

  ## check StartParamList and StartParamDistrib format
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

CreateCalibOptions <-
  function(FUN_MOD,
           FUN_CALIB = Calibration_Michel,
           FUN_TRANSFO = NULL,
           FixedParam = NULL,
           SearchRanges = NULL,
           StartParamList = NULL,
           StartParamDistrib = NULL) {
    
    ObjectClass <- NULL
    
    
    ##check_FUN_MOD
    BOOL <- FALSE
    
    if (identical(FUN_MOD, RunModel_GR4H)) {
      ObjectClass <- c(ObjectClass, "GR4H")
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
    if (!BOOL) {
      stop("incorrect FUN_MOD for use in CreateCalibOptions \n")
      return(NULL)
    }
    
    ##check_FUN_CALIB
    BOOL <- FALSE
    
    if (identical(FUN_CALIB, Calibration_Michel)) {
      ObjectClass <- c(ObjectClass, "HBAN")
      BOOL <- TRUE
    }
    if (!BOOL) {
      stop("incorrect FUN_CALIB for use in CreateCalibOptions \n")
      return(NULL)
      
    }
    
    ##check_FUN_TRANSFO
    if (is.null(FUN_TRANSFO)) {
      ##_set_FUN1
      if (identical(FUN_MOD, RunModel_GR4H)) {
        FUN1 <- TransfoParam_GR4H
      }
      if (identical(FUN_MOD, RunModel_GR4J) |
          identical(FUN_MOD, RunModel_CemaNeigeGR4J)) {
        FUN1 <- TransfoParam_GR4J
      }
      if (identical(FUN_MOD, RunModel_GR5J) |
          identical(FUN_MOD, RunModel_CemaNeigeGR5J)) {
        FUN1 <- TransfoParam_GR5J
      }
      if (identical(FUN_MOD, RunModel_GR6J) |
          identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
        FUN1 <- TransfoParam_GR6J
      }
      if (identical(FUN_MOD, RunModel_GR2M)) {
        FUN1 <- TransfoParam_GR2M
      }
      if (identical(FUN_MOD, RunModel_GR1A)) {
        FUN1 <- TransfoParam_GR1A
      }
      if (identical(FUN_MOD, RunModel_CemaNeige)) {
        FUN1 <- TransfoParam_CemaNeige
      }
      if (is.null(FUN1)) {
        stop("FUN1 was not found \n")
        return(NULL)
      }
      ##_set_FUN2
      FUN2 <- TransfoParam_CemaNeige
      
      ##_set_FUN_TRANSFO
      if (sum(ObjectClass %in% c("GR4H", "GR4J", "GR5J", "GR6J", "GR2M", "GR1A", "CemaNeige")) > 0) {
        FUN_TRANSFO <- FUN1
      } else {
        FUN_TRANSFO <- function(ParamIn, Direction) {
          Bool <- is.matrix(ParamIn)
          if (Bool == FALSE) {
            ParamIn <- rbind(ParamIn)
          }
          ParamOut <- NA * ParamIn
          NParam   <- ncol(ParamIn)
          if (NParam <= 3) {
            ParamOut[, 1:(NParam - 2)] <- FUN1(cbind(ParamIn[, 1:(NParam - 2)]), Direction)
          } else {
            ParamOut[, 1:(NParam - 2)] <- FUN1(ParamIn[, 1:(NParam - 2)], Direction)
          }
          ParamOut[, (NParam - 1):NParam] <- FUN2(ParamIn[, (NParam - 1):NParam], Direction)
          if (Bool == FALSE) {
            ParamOut <- ParamOut[1, ]
          }
          return(ParamOut)
        }
      }
    }
    if (is.null(FUN_TRANSFO)) {
      stop("FUN_TRANSFO was not found \n")
      return(NULL)
    }
    
    ##NParam
    if ("GR4H" %in% ObjectClass) {
      NParam <- 4
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
    if ("CemaNeigeGR4J" %in% ObjectClass) {
      NParam <- 6
    }
    if ("CemaNeigeGR5J" %in% ObjectClass) {
      NParam <- 7
    }
    if ("CemaNeigeGR6J" %in% ObjectClass) {
      NParam <- 8
    }
    
    ##check_FixedParam
    if (is.null(FixedParam)) {
      FixedParam <- rep(NA, NParam)
    } else {
      if (!is.vector(FixedParam)) {
        stop("FixedParam must be a vector \n")
        return(NULL)
      }
      if (length(FixedParam) != NParam) {
        stop("Incompatibility between FixedParam length and FUN_MOD \n")
        return(NULL)
      }
    }
    
    ##check_SearchRanges
    if (is.null(SearchRanges)) {
      ParamT <-  matrix(c(rep(-9.99, NParam), rep(+9.99, NParam)),
                        ncol = NParam, byrow = TRUE)
      
      SearchRanges <- TransfoParam(ParamIn = ParamT, Direction = "TR", FUN_TRANSFO = FUN_TRANSFO)
      
    } else {
      if (!is.matrix(SearchRanges)) {
        stop("SearchRanges must be a matrix \n")
        return(NULL)
      }
      if (!is.numeric(SearchRanges)) {
        stop("SearchRanges must be a matrix of numeric values \n")
        return(NULL)
      }
      if (sum(is.na(SearchRanges)) != 0) {
        stop("SearchRanges must not include NA values \n")
        return(NULL)
      }
      if (nrow(SearchRanges) != 2) {
        stop("SearchRanges must have 2 rows \n")
        return(NULL)
      }
      if (ncol(SearchRanges) != NParam) {
        stop("Incompatibility between SearchRanges ncol and FUN_MOD \n")
        return(NULL)
      }
    }
    
    ##check_StartParamList_and_StartParamDistrib__default_values
    if (("HBAN"  %in% ObjectClass & is.null(StartParamList) & is.null(StartParamDistrib))) {
      if ("GR4H" %in% ObjectClass) {
        ParamT <- matrix(c(+5.12, -1.18, +4.34, -9.69,
                           +5.58, -0.85, +4.74, -9.47,
                           +6.01, -0.50, +5.14, -8.87), ncol = NParam,  byrow = TRUE)
      }
      if ("GR4J" %in% ObjectClass) {
        ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05,
                           +5.51, -0.61, +3.74, -8.51,
                           +6.07, -0.02, +4.42, -8.06),  ncol = NParam, byrow = TRUE)
      }
      if ("GR5J" %in% ObjectClass) {
        ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45,
                           +5.55, -0.46, +3.75, -9.09, -4.69,
                           +6.10, -0.11, +4.43, -8.60, -0.66), ncol = NParam, byrow = TRUE)
        
      }
      if ("GR6J" %in% ObjectClass) {
        ParamT <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00,
                           +3.90, -0.50, +4.10, -8.70, +0.10, +4.00,
                           +4.50, +0.50, +5.00, -8.10, +1.10, +5.00), ncol = NParam, byrow = TRUE)
      }
      if ("GR2M" %in% ObjectClass) {
        ParamT <- matrix(c(+5.03, -7.15,
                           +5.22, -6.74,
                           +5.85, -6.37), ncol = NParam, byrow = TRUE)
      }
      if ("GR1A" %in% ObjectClass) {
        ParamT <- matrix(c(-1.69,
                           -0.38,
                           +1.39), ncol = NParam, byrow = TRUE)
      }
      if ("CemaNeige" %in% ObjectClass) {
        ParamT <- matrix(c(-9.96, +6.63,
                           -9.14, +6.90,
                           +4.10, +7.21), ncol = NParam, byrow = TRUE)
      }
      if ("CemaNeigeGR4J" %in% ObjectClass) {
        ParamT <- matrix(c(+5.13, -1.60, +3.03, -9.05, -9.96, +6.63,
                           +5.51, -0.61, +3.74, -8.51, -9.14, +6.90,
                           +6.07, -0.02, +4.42, -8.06, +4.10, +7.21), ncol = NParam, byrow = TRUE)
      }
      if ("CemaNeigeGR5J" %in% ObjectClass) {
        ParamT <- matrix(c(+5.17, -1.13, +3.08, -9.37, -7.45, -9.96, +6.63,
                           +5.55, -0.46, +3.75, -9.09, -4.69, -9.14, +6.90,
                           +6.10, -0.11, +4.43, -8.60, -0.66, +4.10, +7.21), ncol = NParam, byrow = TRUE)
      }
      if ("CemaNeigeGR6J" %in% ObjectClass) {
        ParamT <- matrix(c(+3.60, -1.00, +3.30, -9.10, -0.90, +3.00, -9.96, +6.63,
                           +3.90, -0.50, +4.10, -8.70, +0.10, +4.00, -9.14, +6.90,
                           +4.50, +0.50, +5.00, -8.10, +1.10, +5.00, +4.10, +7.21), ncol = NParam, byrow = TRUE)
      }
      
      StartParamList    <- NULL
      StartParamDistrib <- TransfoParam(ParamIn = ParamT, Direction = "TR", FUN_TRANSFO = FUN_TRANSFO)
      
    }
    
    ##check_StartParamList_and_StartParamDistrib__format
    if ("HBAN" %in% ObjectClass & !is.null(StartParamList)) {
      if (!is.matrix(StartParamList)) {
        stop("StartParamList must be a matrix \n")
        return(NULL)
      }
      if (!is.numeric(StartParamList)) {
        stop("StartParamList must be a matrix of numeric values \n")
        return(NULL)
      }
      if (sum(is.na(StartParamList)) != 0) {
        stop("StartParamList must not include NA values \n")
        return(NULL)
      }
      if (ncol(StartParamList) != NParam) {
        stop("Incompatibility between StartParamList ncol and FUN_MOD \n")
        return(NULL)
      }
    }
    if ("HBAN" %in% ObjectClass & !is.null(StartParamDistrib)) {
      if (!is.matrix(StartParamDistrib)) {
        stop("StartParamDistrib must be a matrix \n")
        return(NULL)
      }
      if (!is.numeric(StartParamDistrib[1, ])) {
        stop("StartParamDistrib must be a matrix of numeric values \n")
        return(NULL)
      }
      if (sum(is.na(StartParamDistrib[1, ])) != 0) {
        stop("StartParamDistrib must not include NA values on the first line \n")
        return(NULL)
      }
      if (ncol(StartParamDistrib) != NParam) {
        stop("Incompatibility between StartParamDistrib ncol and FUN_MOD \n")
        return(NULL)
      }
    }
    
    
    ##Create_CalibOptions
    CalibOptions <- list(FixedParam = FixedParam, SearchRanges = SearchRanges)
    
    if (!is.null(StartParamList)) {
      CalibOptions <- c(CalibOptions, list(StartParamList = StartParamList))
    }
    if (!is.null(StartParamDistrib)) {
      CalibOptions <- c(CalibOptions, list(StartParamDistrib = StartParamDistrib))
    }
    class(CalibOptions) <- c("CalibOptions", ObjectClass)
    
    return(CalibOptions)
    
  }

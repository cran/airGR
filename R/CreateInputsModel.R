CreateInputsModel <- function(FUN_MOD,
                              DatesR,
                              Precip, PrecipScale = TRUE,
                              PotEvap = NULL,
                              TempMean = NULL, TempMin = NULL, TempMax = NULL,
                              ZInputs = NULL, HypsoData = NULL, NLayers = 5,
                              verbose = TRUE) {
    
    
    ObjectClass <- NULL
    
    ##check_FUN_MOD
    BOOL <- FALSE
    if (identical(FUN_MOD, RunModel_GR4H)) {
      ObjectClass <- c(ObjectClass, "hourly", "GR")
      
      TimeStep <- as.integer(60 * 60)
      
      BOOL <- TRUE
    }
    if (identical(FUN_MOD, RunModel_GR4J) |
        identical(FUN_MOD, RunModel_GR5J) |
        identical(FUN_MOD, RunModel_GR6J)) {
      ObjectClass <- c(ObjectClass, "daily", "GR")
      
      TimeStep <- as.integer(24 * 60 * 60)
      
      BOOL <- TRUE
    }
    if (identical(FUN_MOD, RunModel_GR2M)) {
      ObjectClass <- c(ObjectClass, "GR", "monthly")
      
      TimeStep <- as.integer(c(28, 29, 30, 31) * 24 * 60 * 60)
      
      BOOL <- TRUE
    }
    if (identical(FUN_MOD, RunModel_GR1A)) {
      ObjectClass <- c(ObjectClass, "GR", "yearly")
      
      TimeStep <- as.integer(c(365, 366) * 24 * 60 * 60)
      
      BOOL <- TRUE
    }
    if (identical(FUN_MOD, RunModel_CemaNeige)) {
      ObjectClass <- c(ObjectClass, "daily", "CemaNeige")
      
      TimeStep <- as.integer(24 * 60 * 60)
      
      BOOL <- TRUE
    }
    if (identical(FUN_MOD, RunModel_CemaNeigeGR4J) |
        identical(FUN_MOD, RunModel_CemaNeigeGR5J) |
        identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
      ObjectClass <- c(ObjectClass, "daily", "GR", "CemaNeige")
      
      TimeStep <- as.integer(24 * 60 * 60)
      
      BOOL <- TRUE
    }
    if (!BOOL) {
      stop("Incorrect FUN_MOD for use in CreateInputsModel \n")
      return(NULL)
    }
    
    ##check_arguments
    if ("GR" %in% ObjectClass | "CemaNeige" %in% ObjectClass) {
      if (is.null(DatesR)) {
        stop("DatesR is missing \n")
        return(NULL)
      }
      if ("POSIXlt" %in% class(DatesR) == FALSE & "POSIXct" %in% class(DatesR) == FALSE) {
        stop("DatesR must be defined as POSIXlt or POSIXct \n")
        return(NULL)
      }
      if ("POSIXlt" %in% class(DatesR) == FALSE) {
        DatesR <- as.POSIXlt(DatesR)
      }
      if (difftime(tail(DatesR, 1), tail(DatesR, 2), units = "secs")[[1]] %in% TimeStep == FALSE) {
        TimeStepName <- grep("hourly|daily|monthly|yearly", ObjectClass, value = TRUE)
        stop(paste0("The time step of the model inputs must be ", TimeStepName, "\n"))
        return(NULL)
      }
      if (any(duplicated(DatesR))) {
        stop("DatesR must not include duplicated values \n")
        return(NULL)
      }
      LLL <- length(DatesR)
    }
    if ("GR" %in% ObjectClass) {
      if (is.null(Precip)) {
        stop("Precip is missing \n")
        return(NULL)
      }
      if (is.null(PotEvap)) {
        stop("PotEvap is missing \n")
        return(NULL)
      }
      if (!is.vector(Precip) | !is.vector(PotEvap)) {
        stop("Precip and PotEvap must be vectors of numeric values \n")
        return(NULL)
      }
      if (!is.numeric(Precip) | !is.numeric(PotEvap)) {
        stop("Precip and PotEvap must be vectors of numeric values \n")
        return(NULL)
      }
      if (length(Precip) != LLL | length(PotEvap) != LLL) {
        stop("Precip, PotEvap and DatesR must have the same length \n")
        return(NULL)
      }
    }
    if ("CemaNeige" %in% ObjectClass) {
      if (is.null(Precip)) {
        stop("Precip is missing \n")
        return(NULL)
      }
      if (is.null(TempMean)) {
        stop("TempMean is missing \n")
        return(NULL)
      }
      if (!is.vector(Precip) | !is.vector(TempMean)) {
        stop("Precip and TempMean must be vectors of numeric values \n")
        return(NULL)
      }
      if (!is.numeric(Precip) | !is.numeric(TempMean)) {
        stop("Precip and TempMean must be vectors of numeric values \n")
        return(NULL)
      }
      if (length(Precip) != LLL | length(TempMean) != LLL) {
        stop("Precip, TempMean and DatesR must have the same length \n")
        return(NULL)
      }
      if (is.null(TempMin) != is.null(TempMax)) {
        stop("TempMin and TempMax must be both defined if not null \n")
        return(NULL)
      }
      if (!is.null(TempMin) & !is.null(TempMax)) {
        if (!is.vector(TempMin) | !is.vector(TempMax)) {
          stop("TempMin and TempMax must be vectors of numeric values \n")
          return(NULL)
        }
        if (!is.numeric(TempMin) | !is.numeric(TempMax)) {
          stop("TempMin and TempMax must be vectors of numeric values \n")
          return(NULL)
        }
        if (length(TempMin) != LLL | length(TempMax) != LLL) {
          stop("TempMin, TempMax and DatesR must have the same length \n")
          return(NULL)
        }
      }
      if (!is.null(HypsoData)) {
        if (!is.vector(HypsoData)) {
          stop("HypsoData must be a vector of numeric values  if not null \n")
          return(NULL)
        }
        if (!is.numeric(HypsoData)) {
          stop("HypsoData must be a vector of numeric values  if not null \n")
          return(NULL)
        }
        if (length(HypsoData) != 101) {
          stop("HypsoData must be of length 101 if not null \n")
          return(NULL)
        }
        if (sum(is.na(HypsoData)) != 0 & sum(is.na(HypsoData)) != 101) {
          stop("HypsoData must not contain any NA if not null \n")
          return(NULL)
        }
      }
      if (!is.null(ZInputs)) {
        if (length(ZInputs) != 1) {
          stop("\t ZInputs must be a single numeric value if not null \n")
          return(NULL)
        }
        if (is.na(ZInputs) | !is.numeric(ZInputs)) {
          stop("\t ZInputs must be a single numeric value if not null \n")
          return(NULL)
        }
      }
      if (is.null(HypsoData)) {
        if (verbose) {
          warning("\t HypsoData is missing => a single layer is used and no extrapolation is made \n")
        }
        HypsoData <- as.numeric(rep(NA, 101))
        ZInputs   <- as.numeric(NA)
        NLayers   <- as.integer(1)
        
      }
      if (is.null(ZInputs)) {
        if (verbose & !identical(HypsoData, as.numeric(rep(NA, 101)))) {
          warning("\t ZInputs is missing => HypsoData[51] is used \n")
        }
        ZInputs <- HypsoData[51L]
      }
      if (NLayers <= 0) {
        stop("NLayers must be a positive integer value \n")
        return(NULL)
      }
      if (NLayers != as.integer(NLayers)) {
        warning("Coerce NLayers to be of integer type (",  NLayers, " => ", as.integer(NLayers), ")")
        NLayers <- as.integer(NLayers)
      }
    }
    
    
    ##check_NA_values
    BOOL_NA <- rep(FALSE, length(DatesR))
    
    if ("GR" %in% ObjectClass) {
      BOOL_NA_TMP <- (Precip  < 0) | is.na(Precip)
      if (sum(BOOL_NA_TMP) != 0) {
        BOOL_NA <- BOOL_NA |  BOOL_NA_TMP
        if (verbose) {
          warning("\t Values < 0 or NA values detected in Precip series  \n")
        }
      }
      BOOL_NA_TMP <- (PotEvap < 0) | is.na(PotEvap)
      if (sum(BOOL_NA_TMP) != 0) {
        BOOL_NA <- BOOL_NA | BOOL_NA_TMP
        if (verbose) {
          warning("\t Values < 0 or NA values detected in PotEvap series \n")
        }
      }
    }
    if ("CemaNeige" %in% ObjectClass) {
      BOOL_NA_TMP <- (Precip  < 0) | is.na(Precip)
      if (sum(BOOL_NA_TMP) != 0) {
        BOOL_NA <- BOOL_NA | BOOL_NA_TMP
        if (verbose) {
          warning("\t Values < 0 or NA values detected in Precip series       \n")
        }
      }
      BOOL_NA_TMP <-  (TempMean < (-150)) | is.na(TempMean)
      if (sum(BOOL_NA_TMP) != 0) {
        BOOL_NA <- BOOL_NA | BOOL_NA_TMP
        if (verbose) {
          warning("\t Values < -150) or NA values detected in TempMean series \n")
        }
      }
      if (!is.null(TempMin) & !is.null(TempMax)) {
        BOOL_NA_TMP <- (TempMin < (-150)) | is.na(TempMin)
        if (sum(BOOL_NA_TMP) != 0) {
          BOOL_NA <- BOOL_NA | BOOL_NA_TMP
          if (verbose) {
            warning("\t Values < -150) or NA values detected in TempMin series \n")
          }
        }
        BOOL_NA_TMP <-  (TempMax < (-150)) | is.na(TempMax)
        if (sum(BOOL_NA_TMP) != 0) {
          BOOL_NA <- BOOL_NA | BOOL_NA_TMP
          if (verbose) {
            warning("\t Values < -150) or NA values detected in TempMax series \n")
          }
        }
      }
    }
    if (sum(BOOL_NA) != 0) {
      WTxt <- NULL
      WTxt <- paste(WTxt, "\t Missing values are not allowed in InputsModel \n", sep = "")
      
      Select <- (max(which(BOOL_NA)) + 1):length(BOOL_NA)
      
      if (Select[1L] > Select[2L]) {
        stop("Time series could not be trunced since missing values were detected at the list time-step")
        return(NULL)
      }
      if ("GR" %in% ObjectClass) {
        Precip  <- Precip[Select]
        PotEvap <- PotEvap[Select]
      }
      if ("CemaNeige" %in% ObjectClass) {
        Precip   <- Precip[Select]
        TempMean <- TempMean[Select]
        if (!is.null(TempMin) & !is.null(TempMax)) {
          TempMin <- TempMin[Select]
          TempMax <- TempMax[Select]
        }
      }
      
      DatesR <- DatesR[Select]
      
      WTxt <- paste(WTxt, "\t -> Data were trunced to keep the most recent available time-steps \n", sep = "")
      WTxt <- paste(WTxt, "\t -> ", length(Select), " time-steps were kept \n", sep = "")
      
      if (!is.null(WTxt) & verbose) {
        warning(WTxt)
      }
    }
    
    
    ##DataAltiExtrapolation_Valery
    if ("CemaNeige" %in% ObjectClass) {
      RESULT <- DataAltiExtrapolation_Valery(DatesR = DatesR,
                                             Precip = Precip, PrecipScale = PrecipScale,
                                             TempMean = TempMean, TempMin = TempMin, TempMax = TempMax,
                                             ZInputs = ZInputs, HypsoData = HypsoData, NLayers = NLayers,
                                             verbose = verbose)
      if (verbose) {
        if (NLayers == 1) {
          message("\t Input series were successfully created on 1 elevation layer for use by CemaNeige")
        } else {
          message( "\t Input series were successfully created on ", NLayers, " elevation layers for use by CemaNeige")
        }
      }
    }
    
    
    ##Create_InputsModel
    InputsModel <- list(DatesR = DatesR)
    if ("GR" %in% ObjectClass) {
      InputsModel <- c(InputsModel, list(Precip = as.double(Precip), PotEvap = as.double(PotEvap)))
    }
    if ("CemaNeige" %in% ObjectClass) {
      InputsModel <- c(InputsModel, list(LayerPrecip          = RESULT$LayerPrecip,
                                         LayerTempMean        = RESULT$LayerTempMean,
                                         LayerFracSolidPrecip = RESULT$LayerFracSolidPrecip,
                                         ZLayers              = RESULT$ZLayers))
    }
    
    class(InputsModel) <- c("InputsModel", ObjectClass)
    
    return(InputsModel)
    
    
    
  }

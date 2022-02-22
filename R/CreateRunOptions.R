CreateRunOptions <- function(FUN_MOD, InputsModel,
                             IndPeriod_WarmUp = NULL, IndPeriod_Run,
                             IniStates = NULL, IniResLevels = NULL, Imax = NULL,
                             Outputs_Cal = NULL, Outputs_Sim = "all",
                             MeanAnSolidPrecip = NULL, IsHyst = FALSE,
                             warnings = TRUE, verbose = TRUE) {

  if (!is.null(Imax)) {
    if (!is.numeric(Imax) | length(Imax) != 1L) {
      stop("'Imax' must be a non negative 'numeric' value of length 1")
    } else {
      if (Imax < 0) {
        stop("'Imax' must be a non negative 'numeric' value of length 1")
      }
    }
    IsIntStore <- TRUE
  } else {
    IsIntStore <- FALSE
  }

  ## check FUN_MOD
  FUN_MOD <- match.fun(FUN_MOD)
  FeatFUN_MOD <- .GetFeatModel(FUN_MOD = FUN_MOD, DatesR = InputsModel$DatesR)
  ObjectClass <- FeatFUN_MOD$Class
  TimeStepMean <- FeatFUN_MOD$TimeStepMean

  ## Model output variable list
  FortranOutputs <- .FortranOutputs(GR = FeatFUN_MOD$CodeModHydro,
                                    isCN = "CemaNeige" %in% FeatFUN_MOD$Class)

  ## manage class
  if (IsIntStore) {
    ObjectClass <- c(ObjectClass, "interception")
  }
  if ("CemaNeige" %in% FeatFUN_MOD$Class) {
    FeatFUN_MOD$IsHyst <- IsHyst
    if (IsHyst) {
      ObjectClass <- c(ObjectClass, "hysteresis")
      FeatFUN_MOD$NbParam <- FeatFUN_MOD$NbParam + 2
    }
  }

  ## SD model
  FeatFUN_MOD$IsSD <- inherits(InputsModel, "SD")
  if (FeatFUN_MOD$IsSD) {
    FeatFUN_MOD$NbParam <- FeatFUN_MOD$NbParam + 1
  }

  if (!"CemaNeige" %in% ObjectClass & "hysteresis" %in% ObjectClass) {
    stop("'IsHyst' cannot be TRUE for the chosen 'FUN_MOD'")
  }
  if (!(identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & "interception" %in% ObjectClass) {
    stop("'IMax' cannot be set for the chosen 'FUN_MOD'")
  }

  ##check_InputsModel
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if ("GR" %in% ObjectClass & !inherits(InputsModel, "GR")) {
    stop("'InputsModel' must be of class 'GR'")
  }
  if ("CemaNeige" %in% ObjectClass &
      !inherits(InputsModel, "CemaNeige")) {
    stop("'InputsModel' must be of class 'CemaNeige'")
  }
  if ("hourly" %in% ObjectClass &
      !inherits(InputsModel, "hourly")) {
    stop("'InputsModel' must be of class 'hourly'")
  }
  if ("daily" %in% ObjectClass & !inherits(InputsModel, "daily")) {
    stop("'InputsModel' must be of class 'daily'")
  }
  if ("monthly" %in% ObjectClass &
      !inherits(InputsModel, "monthly")) {
    stop("'InputsModel' must be of class 'monthly'")
  }
  if ("yearly" %in% ObjectClass &
      !inherits(InputsModel, "yearly")) {
    stop("'InputsModel' must be of class 'yearly'")
  }


  ##check_IndPeriod_Run
  if (!is.vector(IndPeriod_Run)) {
    stop("'IndPeriod_Run' must be a vector of numeric values")
  }
  if (!is.numeric(IndPeriod_Run)) {
    stop("'IndPeriod_Run' must be a vector of numeric values")
  }
  if (!identical(as.integer(IndPeriod_Run), as.integer(seq(from = IndPeriod_Run[1], to = tail(IndPeriod_Run, 1), by = 1)))) {
    stop("'IndPeriod_Run' must be a continuous sequence of integers")
  }
  if (storage.mode(IndPeriod_Run) != "integer") {
    stop("'IndPeriod_Run' should be of type integer")
  }


  ##check_IndPeriod_WarmUp
  WTxt <- NULL
  if (is.null(IndPeriod_WarmUp)) {
    WTxt <- paste(WTxt, "model warm up period not defined: default configuration used", sep = "")
    ##If_the_run_period_starts_at_the_very_beginning_of_the_time_series
    if (IndPeriod_Run[1L] == 1L) {
      IndPeriod_WarmUp <- 0L
      WTxt <- paste0(WTxt,"\n no data were found for model warm up!")
      ##We_look_for_the_longest_period_preceeding_the_run_period_with_a_maximum_of_one_year
    } else {
      TmpDateR0 <- InputsModel$DatesR[IndPeriod_Run[1]]
      TmpDateR  <- TmpDateR0 - 365 * 24 * 60 * 60
      ### minimal date to start the warmup
      if (format(TmpDateR, format = "%d") != format(TmpDateR0, format = "%d")) {
        ### leap year
        TmpDateR <- TmpDateR - 1 * 24 * 60 * 60
      }
      IndPeriod_WarmUp <- which(InputsModel$DatesR == max(InputsModel$DatesR[1], TmpDateR)):(IndPeriod_Run[1] - 1)
      if (length(IndPeriod_WarmUp) * TimeStepMean / (365 * 24 * 60 * 60) >= 1) {
        WTxt <- paste0(WTxt, "\n  the year preceding the run period is used \n")
      } else {
        WTxt <- paste0(WTxt, "\n  less than a year (without missing values) was found for model warm up:")
        WTxt <- paste0(WTxt, "\n  (", length(IndPeriod_WarmUp), " time-steps are used for initialisation)")
      }
    }
  }
  if (!is.null(IndPeriod_WarmUp)) {
    if (!is.vector(IndPeriod_WarmUp)) {
      stop("'IndPeriod_WarmUp' must be a vector of numeric values")
    }
    if (!is.numeric(IndPeriod_WarmUp)) {
      stop("'IndPeriod_WarmUp' must be a vector of numeric values")
    }
    if (storage.mode(IndPeriod_WarmUp) != "integer") {
      stop("'IndPeriod_WarmUp' should be of type integer")
    }
    if (identical(IndPeriod_WarmUp, 0L) & verbose) {
      message(paste0(WTxt, " No warm up period is used"))
    }
    if ((IndPeriod_Run[1] - 1) != tail(IndPeriod_WarmUp, 1) & !identical(IndPeriod_WarmUp, 0L)) {
      WTxt <- paste0(WTxt, " Model warm up period is not directly before the model run period")
    }
  }
  if (!is.null(WTxt) & warnings) {
    warning(WTxt)
  }


  ## check IniResLevels
  if ("GR" %in% ObjectClass & ("monthly" %in% ObjectClass | "daily" %in% ObjectClass | "hourly" %in% ObjectClass)) {
    if (!is.null(IniResLevels)) {
      # if (!is.vector(IniResLevels) | !is.numeric(IniResLevels) | any(is.na(IniResLevels))) {
      if (!is.vector(IniResLevels) | is.character(IniResLevels) | is.factor(IniResLevels) | length(IniResLevels) != 4) {
        stop("'IniResLevels' must be a vector of 4 numeric values")
      }
      # if ((identical(FUN_MOD, RunModel_GR4H) | identical(FUN_MOD, RunModel_CemaNeigeGR4H) |
      #      # (identical(FUN_MOD, RunModel_GR5H) & !IsIntStore) |
      #      identical(FUN_MOD, RunModel_GR5H) |
      #      identical(FUN_MOD, RunModel_GR4J) | identical(FUN_MOD, RunModel_CemaNeigeGR4J) |
      #      identical(FUN_MOD, RunModel_GR5J) | identical(FUN_MOD, RunModel_CemaNeigeGR5J) |
      #      identical(FUN_MOD, RunModel_GR2M)) &
      #     length(IniResLevels) != 2) {
      #   stop("the length of 'IniResLevels' must be 2 for the chosen 'FUN_MOD'")
      # }
      if (any(is.na(IniResLevels[1:2]))) {
        stop("the first 2 values of 'IniResLevels' cannot be missing values for the chosen 'FUN_MOD'")
      }
      # if ((identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J) |
      #      (identical(FUN_MOD, RunModel_GR5H) & IsIntStore)) &
      #     length(IniResLevels) != 3) {
      #   stop("the length of 'IniResLevels' must be 3 for the chosen 'FUN_MOD'")
      # }
      if ((identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J))) {
        if (is.na(IniResLevels[3L])) {
          stop("the third value of 'IniResLevels' cannot be a missing value for the chosen 'FUN_MOD'")
        }
      } else {
        if (!is.na(IniResLevels[3L])) {
          warning("the third value of 'IniResLevels' is set to NA value for the chosen 'FUN_MOD'. Only GR6J presents an exponential store")
          IniResLevels[3L] <- NA
        }
      }
      if (identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) {
        if (IsIntStore & is.na(IniResLevels[4L])) {
          stop("the fourth value of 'IniResLevels' cannot be a missing value for the chosen 'FUN_MOD' (GR5H with an interception store)")
        }
        if (!IsIntStore & !is.na(IniResLevels[4L])) {
          warning("the fourth value of 'IniResLevels' is set to NA value for the chosen 'FUN_MOD'. Only GR5H used with an 'Imax' value presents an interception store")
          IniResLevels[4L] <- NA
        }
      } else {
        if (!is.na(IniResLevels[4L])) {
          warning("the fourth value of 'IniResLevels' is set to NA value for the chosen 'FUN_MOD'. Only GR5H used with an 'Imax' value presents an interception store")
          IniResLevels[4L] <- NA
        }
      }
    } else if (is.null(IniStates)) {
      IniResLevels <- as.double(c(0.3, 0.5, NA, NA))
      if (identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
        IniResLevels <- as.double(c(0.3, 0.5, 0, NA))
      }
      if ((identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & IsIntStore) {
        IniResLevels <- as.double(c(0.3, 0.5, NA, 0))
      }
      # if (!identical(FUN_MOD, RunModel_GR6J) & !identical(FUN_MOD, RunModel_CemaNeigeGR6J) &
      #     !identical(FUN_MOD, RunModel_GR5H) & !identical(FUN_MOD, RunModel_CemaNeigeGR5H)) {
      # if (is.null(IniStates)) {
      #   IniResLevels <- as.double(c(0.3, 0.5, NA, NA))
      # }
    }
  } else {
    if (!is.null(IniResLevels)) {
      stop("'IniResLevels' can only be used with monthly or daily or hourly GR models")
    }
  }
  ## check IniStates
  if (is.null(IniStates) & is.null(IniResLevels) & warnings) {
    warning("model states initialisation not defined: default configuration used")
  }
  if (!is.null(IniStates) & !is.null(IniResLevels) & warnings) {
    warning("'IniStates' and 'IniResLevels' are both defined: store levels are taken from 'IniResLevels'")
  }
  if ("CemaNeige" %in% ObjectClass) {
    NLayers <- length(InputsModel$LayerPrecip)
  } else {
    NLayers <- 0
  }
  NState <- NULL
  if ("GR" %in% ObjectClass | "CemaNeige" %in% ObjectClass) {
    if ("hourly"  %in% ObjectClass) {
      NState <- 7 + 3 * 24 * 20 + 4 * NLayers
    }
    if ("daily"   %in% ObjectClass) {
      NState <- 7 + 3 * 20 + 4 * NLayers
    }
    if ("monthly" %in% ObjectClass) {
      NState <- 2
    }
    if ("yearly"  %in% ObjectClass) {
      NState <- 1
    }
  }
  if (!is.null(IniStates)) {

    if (!inherits(IniStates, "IniStates")) {
      stop("'IniStates' must be an object of class 'IniStates'")
    }
    if (sum(ObjectClass %in% class(IniStates)) < 2) {
      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'"))
    }
    if (identical(FUN_MOD, RunModel_GR1A) & !is.null(IniStates)) { ## GR1A
      stop(paste0("'IniStates' is not available for the chosen 'FUN_MOD'"))
    }
    if ((identical(FUN_MOD, RunModel_GR5J) | identical(FUN_MOD, RunModel_CemaNeigeGR5J) |
         identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) &
        !all(is.na(IniStates$UH$UH1))) { ## GR5J or GR5H
      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'.' In 'IniStates', 'UH1' has to be a vector of NA for GR5J"))
    }
    if ((identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) & is.na(IniStates$Store$Exp)) { ## GR6J
      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'.' GR6J needs an exponential store value in 'IniStates'"))
    }
    if ((identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & is.na(IniStates$Store$Int)) { ## GR5H interception

      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'.' GR5H (with interception store) needs an interception store value in 'IniStates'"))
    }
    if (!(identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) & !is.na(IniStates$Store$Exp)) { ## except GR6J
      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'.' No exponential store value needed in 'IniStates'"))
    }
    if (!(identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & !is.na(IniStates$Store$Int)) { ## except GR5H interception
      stop(paste0("non convenient 'IniStates' for the chosen 'FUN_MOD'.' No interception store value needed in 'IniStates'"))
    }
    # if (length(na.omit(unlist(IniStates))) != NState) {
    #   stop(paste0("the length of IniStates must be ", NState, " for the chosen FUN_MOD"))
    # }
    if ((!"CemaNeige" %in% ObjectClass &  inherits(IniStates, "CemaNeige")) |
        ( "CemaNeige" %in% ObjectClass & !inherits(IniStates, "CemaNeige"))) {
      stop("'FUN_MOD' and 'IniStates' must be both of class 'CemaNeige'")
    }
    if (( "hysteresis" %in% ObjectClass & !inherits(IniStates, "hysteresis")) |
        (!"hysteresis" %in% ObjectClass &  inherits(IniStates, "hysteresis"))) {
      stop("'IsHyst' and 'IniStates' are not consistent on the use of the hysteresis")
    }
    if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$G  ))) {
      IniStates$CemaNeigeLayers$G <- NULL
    }
    if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$eTG))) {
      IniStates$CemaNeigeLayers$eTG <- NULL
    }
    if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$Gthr))) {
      IniStates$CemaNeigeLayers$Gthr <- NULL
    }
    if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$Glocmax))) {
      IniStates$CemaNeigeLayers$Glocmax <- NULL
    }
    IniStates$Store$Rest <- rep(NA, 3)
    IniStates <- unlist(IniStates)
    IniStates[is.na(IniStates)] <- 0
    if ("monthly" %in% ObjectClass) {
      IniStates <- IniStates[seq_len(NState)]
    }
  } else {
    IniStates <- as.double(rep(0.0, NState))
  }


  ##check_Outputs_Cal_and_Sim

  ##Outputs_all
  Outputs_all <- c("DatesR", unlist(FortranOutputs), "WarmUpQsim", "StateEnd", "Param")
  if (FeatFUN_MOD$IsSD) {
    Outputs_all <- c(Outputs_all, "QsimDown", "Qsim_m3")
  }

  ##check_Outputs_Sim
  if (!is.vector(Outputs_Sim)) {
    stop("'Outputs_Sim' must be a vector of characters")
  }
  if (!is.character(Outputs_Sim)) {
    stop("'Outputs_Sim' must be a vector of characters")
  }
  if (sum(is.na(Outputs_Sim)) != 0) {
    stop("'Outputs_Sim' must not contain NA")
  }
  if ("all" %in% Outputs_Sim) {
    Outputs_Sim <- Outputs_all
  }
  Test <- which(!Outputs_Sim %in% Outputs_all)
  if (length(Test) != 0) {
    stop(paste0( "'Outputs_Sim' is incorrectly defined: ",
                 paste(Outputs_Sim[Test], collapse = ", "), " not found"))
  }
  Outputs_Sim <- Outputs_Sim[!duplicated(Outputs_Sim)]


  ##check_Outputs_Cal
  if (is.null(Outputs_Cal)) {
    if ("GR" %in% ObjectClass) {
      Outputs_Cal <- c("Qsim", "Param")
      if ("CemaNeige" %in% ObjectClass) {
        Outputs_Cal <- c("PliqAndMelt", Outputs_Cal)
      }
    } else if ("CemaNeige" %in% ObjectClass) {
      Outputs_Cal <- c("all")
    }
  } else {
    if (!is.vector(Outputs_Cal)) {
      stop("'Outputs_Cal' must be a vector of characters")
    }
    if (!is.character(Outputs_Cal)) {
      stop("'Outputs_Cal' must be a vector of characters")
    }
    if (sum(is.na(Outputs_Cal)) != 0) {
      stop("'Outputs_Cal' must not contain NA")
    }
  }
  if ("all" %in% Outputs_Cal) {
    Outputs_Cal <- Outputs_all
  }
  Test <- which(!Outputs_Cal %in% Outputs_all)
  if (length(Test) != 0) {
    stop(paste0("'Outputs_Cal' is incorrectly defined: ",
                paste(Outputs_Cal[Test], collapse = ", "), " not found"))
  }
  Outputs_Cal <- unique(Outputs_Cal)


  ##check_MeanAnSolidPrecip
  if ("CemaNeige" %in% ObjectClass & is.null(MeanAnSolidPrecip)) {
    NLayers <- length(InputsModel$LayerPrecip)
    SolidPrecip <- NULL
    for (iLayer in 1:NLayers) {
      if (iLayer == 1) {
        SolidPrecip <-
          InputsModel$LayerFracSolidPrecip[[1]] * InputsModel$LayerPrecip[[iLayer]] /
          NLayers
      } else {
        SolidPrecip <- SolidPrecip + InputsModel$LayerFracSolidPrecip[[iLayer]] * InputsModel$LayerPrecip[[iLayer]] / NLayers
      }
    }
    Factor <- NULL
    if (inherits(InputsModel, "hourly")) {
      Factor <- 365.25 * 24
    }
    if (inherits(InputsModel, "daily")) {
      Factor <- 365.25
    }
    if (inherits(InputsModel, "monthly")) {
      Factor <- 12
    }
    if (inherits(InputsModel, "yearly")) {
      Factor <- 1
    }
    if (is.null(Factor)) {
      stop("'InputsModel' must be of class 'hourly', 'daily', 'monthly' or 'yearly'")
    }
    MeanAnSolidPrecip <- rep(mean(SolidPrecip) * Factor, NLayers)
    ### default value: same Gseuil for all layers
    if (warnings) {
      warning("'MeanAnSolidPrecip' not defined: it was automatically set to c(",
              paste(round(MeanAnSolidPrecip), collapse = ","), ")  from the 'InputsModel' given to the function. ",
              "Be careful in case your application is (short-term) forecasting.\n")
    }
  }
  if ("CemaNeige" %in% ObjectClass & !is.null(MeanAnSolidPrecip)) {
    if (!is.vector(MeanAnSolidPrecip)) {
      stop(paste0("'MeanAnSolidPrecip' must be a vector of numeric values"))
    }
    if (!is.numeric(MeanAnSolidPrecip)) {
      stop(paste0("'MeanAnSolidPrecip' must be a vector of numeric values"))
    }
    if (length(MeanAnSolidPrecip) != NLayers) {
      stop(paste0("'MeanAnSolidPrecip' must be a numeric vector of length ", NLayers, ""))
    }
  }


  ##check_PliqAndMelt
  if ("GR" %in% ObjectClass & "CemaNeige" %in% ObjectClass) {
    if (!"PliqAndMelt" %in% Outputs_Cal & !"all" %in% Outputs_Cal) {
      WTxt <- NULL
      WTxt <- paste0(WTxt, "'PliqAndMelt' was not defined in 'Outputs_Cal' but is needed to feed the hydrological model with the snow modele outputs \n")
      WTxt <- paste0(WTxt, ": it was automatically added \n")
      if (!is.null(WTxt) & warnings) {
        warning(WTxt)
      }
      Outputs_Cal <- c(Outputs_Cal, "PliqAndMelt")
    }
    if (!"PliqAndMelt" %in% Outputs_Sim & !"all" %in% Outputs_Sim) {
      WTxt <- NULL
      WTxt <- paste0(WTxt, "'PliqAndMelt' was not defined in 'Outputs_Sim' but is needed to feed the hydrological model with the snow modele outputs \n")
      WTxt <- paste0(WTxt, ": it was automatically added \n")
      if (!is.null(WTxt) & warnings) {
        warning(WTxt)
      }
      Outputs_Sim <- c(Outputs_Sim, "PliqAndMelt")
    }
  }


  ##check_Qsim
  if ("GR" %in% ObjectClass) {
    if (!"Qsim" %in% Outputs_Cal & !"all" %in% Outputs_Cal) {
      WTxt <- NULL
      WTxt <- paste0(WTxt, "'Qsim' was not defined in 'Outputs_Cal' \n")
      WTxt <- paste0(WTxt, ": it was automatically added \n")
      if (!is.null(WTxt) & warnings) {
        warning(WTxt)
      }
      Outputs_Cal <- c(Outputs_Cal, "Qsim")
    }
    if (!"Qsim" %in% Outputs_Sim & !"all" %in% Outputs_Sim) {
      WTxt <- NULL
      WTxt <- paste0(WTxt, "'Qsim' was not defined in 'Outputs_Sim' \n")
      WTxt <- paste0(WTxt, ": it was automatically added \n")
      if (!is.null(WTxt) & warnings) {
        warning(WTxt)
      }
      Outputs_Sim <- c(Outputs_Sim, "Qsim")
    }
  }

  ##Create_RunOptions
  RunOptions <- list(IndPeriod_WarmUp = IndPeriod_WarmUp,
                     IndPeriod_Run = IndPeriod_Run,
                     IniStates = IniStates,
                     IniResLevels = IniResLevels,
                     Outputs_Cal = Outputs_Cal,
                     Outputs_Sim = Outputs_Sim,
                     FortranOutputs = FortranOutputs,
                     FeatFUN_MOD = FeatFUN_MOD)

  if ("CemaNeige" %in% ObjectClass) {
    RunOptions <- c(RunOptions, list(MeanAnSolidPrecip = MeanAnSolidPrecip))
  }
  if ("interception" %in% ObjectClass) {
    RunOptions <- c(RunOptions, list(Imax = Imax))
  }
  class(RunOptions) <- c("RunOptions", ObjectClass)

  return(RunOptions)


}


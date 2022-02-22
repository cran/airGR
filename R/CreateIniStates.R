CreateIniStates <- function(FUN_MOD, InputsModel, IsHyst = FALSE, IsIntStore = FALSE,
                            ProdStore = 350, RoutStore = 90, ExpStore = NULL, IntStore = NULL,
                            UH1 = NULL, UH2 = NULL,
                            GCemaNeigeLayers = NULL, eTGCemaNeigeLayers = NULL,
                            GthrCemaNeigeLayers = NULL, GlocmaxCemaNeigeLayers = NULL,
                            SD = NULL,
                            verbose = TRUE) {


  ObjectClass <- NULL

  UH1n <- 20L
  UH2n <- UH1n * 2L

  FUN_MOD <- match.fun(FUN_MOD)

  FeatFUN_MOD <- .GetFeatModel(FUN_MOD = FUN_MOD, DatesR = InputsModel$DatesR)
  ObjectClass <- FeatFUN_MOD$Class

  if (!"CemaNeige" %in% ObjectClass & IsHyst) {
    stop("'IsHyst' cannot be TRUE if CemaNeige is not used in 'FUN_MOD'")
  }
  if (!(identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & IsIntStore) {
    stop("'IsIntStore' cannot be TRUE if GR5H is not used in 'FUN_MOD'")
  }

  ## check InputsModel
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


  ## check states
  if (any(eTGCemaNeigeLayers > 0)) {
    stop("Positive values are not allowed for 'eTGCemaNeigeLayers'")
  }

  if (identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
    if (is.null(ExpStore)) {
      stop("'RunModel_*GR6J' need an 'ExpStore' value")
    }
  } else if (!is.null(ExpStore)) {
    if (verbose) {
      warning(sprintf("'%s' does not require 'ExpStore'. Value set to NA", FeatFUN_MOD$NameFunMod))
    }
    ExpStore <- Inf
  }

  if (identical(FUN_MOD, RunModel_GR2M)) {
    if (!is.null(UH1)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'UH1'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
      UH1 <- rep(Inf, UH1n)
    }
    if (!is.null(UH2)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'UH2'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
      UH2 <- rep(Inf, UH2n)
    }
  }

  if ((identical(FUN_MOD, RunModel_GR5J) | identical(FUN_MOD, RunModel_CemaNeigeGR5J)) & !is.null(UH1)) {
    if (verbose) {
      warning(sprintf("'%s' does not require 'UH1'. Values set to NA", FeatFUN_MOD$NameFunMod))
    }
    UH1 <- rep(Inf, UH1n)
  }
  if (!(identical(FUN_MOD, RunModel_GR5H) | identical(FUN_MOD, RunModel_CemaNeigeGR5H)) & !is.null(IntStore)) {
    if (verbose) {
      warning(sprintf("'%s' does not require 'IntStore'. Values set to NA", FeatFUN_MOD$NameFunMod))
    }
    IntStore <- Inf
  }

  if ("CemaNeige" %in% ObjectClass & ! "GR" %in% ObjectClass) {
    if (!is.null(ProdStore)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'ProdStore'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    ProdStore <- Inf
    if (!is.null(RoutStore)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'RoutStore'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    RoutStore <- Inf
    if (!is.null(ExpStore)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'ExpStore'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    ExpStore <- Inf
    if (!is.null(IntStore)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'IntStore'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    IntStore <- Inf
    if (!is.null(UH1)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'UH1'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    UH1 <- rep(Inf, UH1n)
    if (!is.null(UH2)) {
      if (verbose) {
        warning(sprintf("'%s' does not require 'UH2'. Values set to NA", FeatFUN_MOD$NameFunMod))
      }
    }
    UH2 <- rep(Inf, UH2n)
  }
  if (IsIntStore & is.null(IntStore)) {
      stop(sprintf("'%s' need values for 'IntStore'", FeatFUN_MOD$NameFunMod))
  }
  if ("CemaNeige" %in% ObjectClass & !IsHyst &
     (is.null(GCemaNeigeLayers) | is.null(eTGCemaNeigeLayers))) {
      stop(sprintf("'%s' need values for 'GCemaNeigeLayers' and 'GCemaNeigeLayers'", FeatFUN_MOD$NameFunMod))
  }
  if ("CemaNeige" %in% ObjectClass & IsHyst &
     (is.null(GCemaNeigeLayers) | is.null(eTGCemaNeigeLayers) |
      is.null(GthrCemaNeigeLayers) | is.null(GlocmaxCemaNeigeLayers))) {
    stop(sprintf("'%s' need values for 'GCemaNeigeLayers', 'GCemaNeigeLayers', 'GthrCemaNeigeLayers' and 'GlocmaxCemaNeigeLayers'", FeatFUN_MOD$NameFunMod))
  }
  if ("CemaNeige" %in% ObjectClass & !IsHyst &
     (!is.null(GthrCemaNeigeLayers) | !is.null(GlocmaxCemaNeigeLayers))) {
    if (verbose) {
      warning(sprintf("'%s' does not require 'GthrCemaNeigeLayers' and 'GlocmaxCemaNeigeLayers'. Values set to NA", FeatFUN_MOD$NameFunMod))
    }
    GthrCemaNeigeLayers    <- Inf
    GlocmaxCemaNeigeLayers <- Inf
  }
  if (!"CemaNeige" %in% ObjectClass &
     (!is.null(GCemaNeigeLayers) | !is.null(eTGCemaNeigeLayers) | !is.null(GthrCemaNeigeLayers) | !is.null(GlocmaxCemaNeigeLayers))) {
    if (verbose) {
      warning(sprintf("'%s' does not require 'GCemaNeigeLayers' 'GCemaNeigeLayers', 'GthrCemaNeigeLayers' and 'GlocmaxCemaNeigeLayers'. Values set to NA", FeatFUN_MOD$NameFunMod))
    }
    GCemaNeigeLayers       <- Inf
    eTGCemaNeigeLayers     <- Inf
    GthrCemaNeigeLayers    <- Inf
    GlocmaxCemaNeigeLayers <- Inf
  }


  ## set states
  if ("CemaNeige" %in% ObjectClass) {
    NLayers <- length(InputsModel$LayerPrecip)
  } else {
    NLayers <- 1
  }


  ## manage NULL values
  if (is.null(ExpStore)) {
    ExpStore <- Inf
  }
  if (is.null(IntStore)) {
    IntStore <- Inf
  }
  if (is.null(UH1)) {
    if ("hourly"  %in% ObjectClass) {
      k <- 24
    } else {
      k <- 1
    }
    UH1 <- rep(Inf, UH1n * k)
  }
  if (is.null(UH2)) {
    if ("hourly"  %in% ObjectClass) {
      k <- 24
    } else {
      k <- 1
    }
    UH2 <- rep(Inf, UH2n * k)
  }
  if (is.null(GCemaNeigeLayers)) {
    GCemaNeigeLayers <- rep(Inf, NLayers)
  }
  if (is.null(eTGCemaNeigeLayers)) {
    eTGCemaNeigeLayers <- rep(Inf, NLayers)
  }
  if (is.null(GthrCemaNeigeLayers)) {
    GthrCemaNeigeLayers <- rep(Inf, NLayers)
  }
  if (any(is.infinite(GthrCemaNeigeLayers))) {
    GthrCemaNeigeLayers <- rep(Inf, NLayers)
  }
  if (is.null(GlocmaxCemaNeigeLayers)) {
    GlocmaxCemaNeigeLayers <- rep(Inf, NLayers)
  }
  if (any(is.infinite(GlocmaxCemaNeigeLayers))) {
    GlocmaxCemaNeigeLayers <- rep(Inf, NLayers)
  }

  # check negative values
  if (any(ProdStore < 0) | any(RoutStore < 0) | any(IntStore < 0) |
      any(UH1 < 0) | any(UH2 < 0) |
      any(GCemaNeigeLayers < 0)) {
    stop("Negative values are not allowed for any of 'ProdStore', 'RoutStore', 'IntStore', 'UH1', 'UH2', 'GCemaNeigeLayers'")
  }


  ## check length
  if (!is.numeric(ProdStore) || length(ProdStore) != 1L) {
    stop("'ProdStore' must be numeric of length one")
  }
  if (!is.numeric(RoutStore) || length(RoutStore) != 1L) {
    stop("'RoutStore' must be numeric of length one")
  }
  if (!is.numeric(ExpStore) || length(ExpStore) != 1L) {
    stop("'ExpStore' must be numeric of length one")
  }
  if (!is.numeric(IntStore) || length(IntStore) != 1L) {
    stop("'IntStore' must be numeric of length one")
  }
  if ( "hourly" %in% ObjectClass & (!is.numeric(UH1) || length(UH1) != UH1n * 24)) {
    stop(sprintf("'UH1' must be numeric of length 480 (%i * 24)", UH1n))
  }
  if (!"hourly" %in% ObjectClass & (!is.numeric(UH1) || length(UH1) != UH1n)) {
    stop(sprintf("'UH1' must be numeric of length %i", UH1n))
  }
  if ( "hourly" %in% ObjectClass & (!is.numeric(UH2) || length(UH2) != UH2n * 24)) {
    stop(sprintf("'UH2' must be numeric of length 960 (%i * 24)", UH2n))
  }
  if (!"hourly" %in% ObjectClass & (!is.numeric(UH2) || length(UH2) != UH2n)) {
    stop(sprintf("'UH2' must be numeric of length %i (2 * %i)", UH2n, UH1n))
  }
  if (!is.numeric(GCemaNeigeLayers) || length(GCemaNeigeLayers) != NLayers) {
    stop(sprintf("'GCemaNeigeLayers' must be numeric of length %i", NLayers))
  }
  if (!is.numeric(eTGCemaNeigeLayers) || length(eTGCemaNeigeLayers) != NLayers) {
    stop(sprintf("'eTGCemaNeigeLayers' must be numeric of length %i", NLayers))
  }
  if (IsHyst) {
    if (!is.numeric(GthrCemaNeigeLayers) || length(GthrCemaNeigeLayers) != NLayers) {
      stop(sprintf("'eTGCemaNeigeLayers' must be numeric of length %i", NLayers))
    }
    if (!is.numeric(GlocmaxCemaNeigeLayers) || length(GlocmaxCemaNeigeLayers) != NLayers) {
      stop(sprintf("'eTGCemaNeigeLayers' must be numeric of length %i", NLayers))
    }
  }

  # SD model state handling
  if (!is.null(SD)) {
    if (!inherits(InputsModel, "SD")) {
      stop("'SD' argument provided and 'InputsModel' is not of class 'SD'")
    }
    if (!is.list(SD)) {
      stop("'SD' argument must be a list")
    }
    lapply(SD, function(x) {
      if (!is.numeric(x)) stop("Each item of 'SD' list argument must be numeric")
    })
    if (length(SD) != length(InputsModel$LengthHydro)) {
      stop("Number of items of 'SD' list argument must be the same as the number of upstream connections",
           sprintf(" (%i required, found %i)", length(InputsModel$LengthHydro), length(SD)))
    }
  }

  ## format output
  IniStates <- list(Store = list(Prod = ProdStore, Rout = RoutStore, Exp = ExpStore, Int = IntStore),
                    UH = list(UH1 = UH1, UH2 = UH2),
                    CemaNeigeLayers = list(G = GCemaNeigeLayers, eTG = eTGCemaNeigeLayers,
                                           Gthr = GthrCemaNeigeLayers, Glocmax = GlocmaxCemaNeigeLayers))
  IniStatesNA <- unlist(IniStates)
  IniStatesNA[is.infinite(IniStatesNA)] <- NA
  IniStatesNA <- relist(IniStatesNA, skeleton = IniStates)

  if (!is.null(SD)) {
    IniStatesNA$SD <- SD
  }

  class(IniStatesNA) <- c("IniStates", ObjectClass)
  if (IsHyst) {
    class(IniStatesNA) <- c(class(IniStatesNA), "hysteresis")
  }
  if (IsIntStore) {
    class(IniStatesNA) <- c(class(IniStatesNA), "interception")
  }

  return(IniStatesNA)


}

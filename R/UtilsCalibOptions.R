.FunTransfo <- function(FeatFUN_MOD) {

  IsHyst <- FeatFUN_MOD$IsHyst
  IsSD <- FeatFUN_MOD$IsSD

  ## set FUN_GR
  if (FeatFUN_MOD$NameFunMod == "Cemaneige") {
    if (IsHyst) {
      FUN_GR <- TransfoParam_CemaNeigeHyst
    } else {
      FUN_GR <- TransfoParam_CemaNeige
    }
  } else {
    # fatal error if the TransfoParam function does not exist
    FUN_GR <- match.fun(sprintf("TransfoParam_%s", FeatFUN_MOD$CodeModHydro))
  }

  ## set FUN_SNOW
  if ("CemaNeige" %in% FeatFUN_MOD$Class) {
    if (IsHyst) {
      FUN_SNOW <- TransfoParam_CemaNeigeHyst
    } else {
      FUN_SNOW <- TransfoParam_CemaNeige
    }
  }

  ## set FUN_LAG
  if (IsSD) {
    FUN_LAG <- TransfoParam_Lag
  }

    ## set FUN_TRANSFO
  if (! "CemaNeige" %in% FeatFUN_MOD$Class) {
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
        ParamOut[, 1:(NParam - 4)     ] <- FUN_GR(ParamIn[, 1:(NParam - 4)], Direction)
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
          ParamOut[, 1:(NParam - 2)] <- FUN_GR(ParamIn[, 1:(NParam - 2)], Direction)
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
        ParamOut[, 2:(NParam - 4)     ] <- FUN_GR(ParamIn[, 2:(NParam - 4)], Direction)
        ParamOut[, (NParam - 3):NParam] <- FUN_SNOW(ParamIn[, (NParam - 3):NParam], Direction)
        ParamOut[, 1                  ] <- FUN_LAG(as.matrix(ParamIn[, 1]), Direction)
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
          ParamOut[, 2:(NParam - 2)] <- FUN_GR(ParamIn[, 2:(NParam - 2)],  Direction)
        }
        ParamOut[, (NParam - 1):NParam] <- FUN_SNOW(ParamIn[, (NParam - 1):NParam], Direction)
        ParamOut[, 1                  ] <- FUN_LAG(as.matrix(ParamIn[, 1]), Direction)
        if (!Bool) {
          ParamOut <- ParamOut[1, ]
        }
        return(ParamOut)
      }
    }
  }
  if (is.null(FUN_TRANSFO)) {
    stop("'FUN_TRANSFO' was not found")
  }
  return(FUN_TRANSFO)
}

CreateInputsCrit_Lavenne <- function(FUN_CRIT = ErrorCrit_KGE,
                                      InputsModel,
                                      RunOptions,
                                      Obs,
                                      VarObs = "Q",
                                      AprParamR,
                                      AprCrit = 1,
                                      k = 0.15,
                                      BoolCrit = NULL,
                                      transfo = "sqrt",
                                      epsilon = NULL) {

  # Check parameters
  if (!is.numeric(AprCrit) || length(AprCrit) != 1 || AprCrit > 1) {
    stop("'AprCrit' must be a numeric of length 1 with a maximum value of 1")
  }
  if (!is.numeric(k) || length(k) != 1 || k < 0 || k > 1) {
    stop("'k' must be a numeric of length 1 with a value between 0 and 1")
  }
  if (!is.null(BoolCrit) && !is.logical(BoolCrit)) {
    stop("'BoolCrit must be logical")
  }
  if (!is.character(transfo)) {
    stop("'transfo' must be character")
  }
  if (!is.null(epsilon) && !is.numeric(epsilon)) {
    stop("'epsilon' must be numeric")
  }
  if (!is.numeric(AprParamR) || length(AprParamR) != RunOptions$FeatFUN_MOD$NbParam) {
    stop("'AprParamR' must be a numeric vector of length ",
         RunOptions$FeatFUN_MOD$NbParam)
  }


  FUN_TRANSFO <- .FunTransfo(RunOptions$FeatFUN_MOD)

  AprParamT <- FUN_TRANSFO(AprParamR, "RT")

  ErrorCrit_GAPX <- CreateErrorCrit_GAPX(FUN_TRANSFO)



  CreateInputsCrit(FUN_CRIT = list(FUN_CRIT, ErrorCrit_GAPX),
                   InputsModel = InputsModel,
                   RunOptions = RunOptions,
                   Obs = list(Obs, AprParamT),
                   VarObs = c("Q", "ParamT"),
                   Weights = c(1 - k, k * max(0, AprCrit)),
                   BoolCrit = list(BoolCrit, NULL),
                   transfo = list(transfo, ""),
                   epsilon = list(epsilon, NULL))
}

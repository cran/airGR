CreateInputsCrit <- function(FUN_CRIT,
                             InputsModel,
                             RunOptions,
                             Qobs,
                             BoolCrit = NULL,
                             transfo = "",
                             Ind_zeroes = NULL,
                             epsilon = NULL,
                             verbose = TRUE) {
  
  
  ObjectClass <- NULL
  
  
  ##check_FUN_CRIT
  BOOL <- FALSE
    
    if (identical(FUN_CRIT, ErrorCrit_NSE) | identical(FUN_CRIT, ErrorCrit_KGE) |
        identical(FUN_CRIT, ErrorCrit_KGE2) | identical(FUN_CRIT, ErrorCrit_RMSE)) {
      BOOL <- TRUE
    }
    if (!BOOL) {
      stop("incorrect FUN_CRIT for use in CreateInputsCrit \n")
      return(NULL)
    }
    
    ##check_arguments
    if (inherits(InputsModel, "InputsModel") == FALSE) {
      stop("InputsModel must be of class 'InputsModel' \n")
      return(NULL)
    }
    if (inherits(RunOptions , "RunOptions") == FALSE) {
      stop("RunOptions must be of class 'RunOptions' \n")
      return(NULL)
    }
    
    LLL <- length(InputsModel$DatesR[RunOptions$IndPeriod_Run])
    
    if (is.null(Qobs)) {
      stop("Qobs is missing \n")
      return(NULL)
    }
    if (!is.vector(Qobs)) {
      stop(paste("Qobs must be a vector of numeric values \n", sep = ""))
      return(NULL)
    }
    if (!is.numeric(Qobs)) {
      stop(paste("Qobs must be a vector of numeric values \n", sep = ""))
      return(NULL)
    }
    if (length(Qobs) != LLL) {
      stop("Qobs and InputsModel series must have the same length \n")
      return(NULL)
    }
    if (is.null(BoolCrit)) {
      BoolCrit <- rep(TRUE, length(Qobs))
    }
    if (!is.logical(BoolCrit)) {
      stop("BoolCrit must be a vector of boolean \n")
      return(NULL)
    }
    if (length(BoolCrit) != LLL) {
      stop("BoolCrit and InputsModel series must have the same length \n")
      return(NULL)
    }
    if (is.null(transfo)) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    if (!is.vector(transfo)) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    if (length(transfo) != 1) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    if (!is.character(transfo)) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    if (! transfo %in% c("", "sqrt", "log", "inv", "sort")) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    
    if (!missing(Ind_zeroes)) {
      warning("Deprecated \"Ind_zeroes\" argument")
    }

    if (!is.null(epsilon)) {
      if (!is.vector(epsilon) | length(epsilon) != 1 | !is.numeric(epsilon) | any(epsilon <= 0)) {
        stop("epsilon must a be single positive value \n")
        return(NULL)
      }
    } else if (transfo %in% c("log", "inv") & any(Qobs %in% 0) & verbose) {
      warning("zeroes detected in Qobs: the corresponding time-steps will be exclude by the 'ErrorCrit*' functions if the epsilon agrument = NULL")
    }
    
    if (transfo == "log" & verbose) {
      warn_log_kge <- "we do not advise using the %s with a log transformation on Qobs (see the details part in the 'CreateInputsCrit' help)"
      if (identical(FUN_CRIT, ErrorCrit_KGE)) {
          warning(sprintf(warn_log_kge, "KGE"))
        }
      if (identical(FUN_CRIT, ErrorCrit_KGE2)) {
          warning(sprintf(warn_log_kge, "KGE'"))
      }
    }
    
    ##Create_InputsCrit
    InputsCrit <- list(BoolCrit   = BoolCrit,
                       Qobs       = Qobs,
                       transfo    = transfo,
                       epsilon    = epsilon)
    
    class(InputsCrit) <- c("InputsCrit", ObjectClass)
    
    return(InputsCrit)
    
    
    
  }

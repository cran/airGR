CreateInputsCrit <-
  function(FUN_CRIT,
           InputsModel,
           RunOptions,
           Qobs,
           BoolCrit = NULL,
           transfo = "",
           Ind_zeroes = NULL,
           epsilon = NULL) {
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
    if (transfo %in% c("", "sqrt", "log", "inv", "sort") == FALSE) {
      stop("transfo must be a chosen among the following: '', 'sqrt', 'log' or 'inv' or 'sort' \n")
      return(NULL)
    }
    
    if (!is.null(Ind_zeroes)) {
      if (!is.vector(Ind_zeroes)) {
        stop("Ind_zeroes must be a vector of integers \n")
        return(NULL)
      }
      if (!is.integer(Ind_zeroes)) {
        stop("Ind_zeroes must be a vector of integers \n")
        return(NULL)
      }
    }
    if (!is.null(epsilon)) {
      if (!is.vector(epsilon) |
          length(epsilon) != 1 | !is.numeric(epsilon)) {
        stop("epsilon must be single numeric value \n")
        return(NULL)
        
      }
      epsilon = as.double(epsilon)
    }
    
    ##Create_InputsCrit
    InputsCrit <- list(BoolCrit = BoolCrit,
                       Qobs     = Qobs,
                       transfo  = transfo,
                       Ind_zeroes = Ind_zeroes,
                       epsilon = epsilon)
    
    class(InputsCrit) <- c("InputsCrit", ObjectClass)
    
    return(InputsCrit)
    
    
    
  }

ErrorCrit <- function(InputsCrit,OutputsModel,FUN_CRIT, warnings = TRUE, verbose = TRUE){
    return( FUN_CRIT(InputsCrit,OutputsModel, warnings = warnings, verbose = verbose) )
}


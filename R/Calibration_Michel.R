Calibration_Michel <- function(InputsModel,
                               RunOptions,
                               InputsCrit,
                               CalibOptions,
                               FUN_MOD,
                               FUN_CRIT,           # deprecated
                               FUN_TRANSFO = NULL,
                               verbose = TRUE,
                               ...) {


  FUN_MOD  <- match.fun(FUN_MOD)

  if (!missing(FUN_CRIT)) {
    FUN_CRIT <- match.fun(FUN_CRIT)
  }

  # Handling 'FUN_TRANSFO' from direct argument or provided by 'CaliOptions'
  if (!is.null(FUN_TRANSFO)) {
    FUN_TRANSFO <- match.fun(FUN_TRANSFO)
  } else if (!is.null(CalibOptions$FUN_TRANSFO)) {
    FUN_TRANSFO <- CalibOptions$FUN_TRANSFO
  } else {
    stop("'FUN_TRANSFO' is not provided neither as 'FUN_TRANSFO' argument or in 'CaliOptions' argument")
  }

  ##_____Arguments_check_____________________________________________________________________
  if (!inherits(InputsModel, "InputsModel")) {
    stop("'InputsModel' must be of class 'InputsModel'")
  }
  if (!inherits(RunOptions, "RunOptions")) {
    stop("'RunOptions' must be of class 'RunOptions'")
  }
  if (!inherits(InputsCrit, "InputsCrit")) {
    stop("'InputsCrit' must be of class 'InputsCrit'")
  }
  if (inherits(InputsCrit, "Multi")) {
    stop("'InputsCrit' must be of class 'Single' or 'Compo'")
  }
  if (inherits(InputsCrit, "Single")) {
    listVarObs <- InputsCrit$VarObs
  }
  if (inherits(InputsCrit, "Compo")) {
    listVarObs <- sapply(InputsCrit, FUN = "[[", "VarObs")
  }
  if ("SCA" %in% listVarObs & !"Gratio" %in% RunOptions$Outputs_Cal) {
    warning("Missing 'Gratio' is automatically added to 'Output_Cal' in 'RunOptions' as it is necessary in the objective function for comparison with SCA")
    RunOptions$Outputs_Cal <- c(RunOptions$Outputs_Cal, "Gratio")
  }
  if ("SWE" %in% listVarObs & !"SnowPack" %in% RunOptions$Outputs_Cal) {
    warning("Missing 'SnowPack' is automatically added to 'Output_Cal' in 'RunOptions' as it is necessary in the objective function for comparison with SWE")
    RunOptions$Outputs_Cal <- c(RunOptions$Outputs_Cal, "SnowPack")
  }
  if (!inherits(CalibOptions, "CalibOptions")) {
    stop("'CalibOptions' must be of class 'CalibOptions'")
  }
  if (!inherits(CalibOptions, "HBAN")) {
    stop("'CalibOptions' must be of class 'HBAN' if 'Calibration_Michel' is used")
  }
  if (!missing(FUN_CRIT)) {
    warning("argument 'FUN_CRIT' is deprecated. The error criterion function is now automatically get from the 'InputsCrit' object")
  }


  ##_variables_initialisation
  ParamFinalR <- NULL
  ParamFinalT <- NULL
  CritFinal   <- NULL
  NRuns <- 0
  NIter <- 0
  if ("StartParamDistrib" %in% names(CalibOptions)) {
    PrefilteringType <- 2
  } else {
    PrefilteringType <- 1
  }
  if (PrefilteringType == 1) {
    NParam <- ncol(CalibOptions$StartParamList)
  }
  if (PrefilteringType == 2) {
    NParam <- ncol(CalibOptions$StartParamDistrib)
  }
  if (NParam > 20) {
    stop("Calibration_Michel can handle a maximum of 20 parameters")
  }
  HistParamR    <- matrix(NA, nrow = 500 * NParam, ncol = NParam)
  HistParamT    <- matrix(NA, nrow = 500 * NParam, ncol = NParam)
  HistCrit      <- matrix(NA, nrow = 500 * NParam, ncol = 1)
  CritName      <- NULL
  CritBestValue <- NULL
  Multiplier    <- NULL
  CritOptim     <- +1e100
  ##_temporary_change_of_Outputs_Sim
  RunOptions$Outputs_Sim <- RunOptions$Outputs_Cal  ### this reduces the size of the matrix exchange with fortran and therefore speeds the calibration



  ##_____Parameter_Grid_Screening____________________________________________________________


  ##Definition_of_the_function_creating_all_possible_parameter_sets_from_different_values_for_each_parameter
  ## use unique() to avoid duplicated values when a parameter is set
  ProposeCandidatesGrid <- function(DistribParam) {
    expand.grid(lapply(seq_len(ncol(DistribParam)), function(x) unique(DistribParam[, x])))
  }


  ##Creation_of_new_candidates_______________________________________________
  OptimParam <- is.na(CalibOptions$FixedParam)
  if (PrefilteringType == 1) {
    CandidatesParamR <- CalibOptions$StartParamList
  }
  if (PrefilteringType == 2) {
    DistribParamR <- CalibOptions$StartParamDistrib
    DistribParamR[, !OptimParam] <- NA
    CandidatesParamR <- ProposeCandidatesGrid(DistribParamR)
  }
  ##Remplacement_of_non_optimised_values_____________________________________
  CandidatesParamR <- apply(CandidatesParamR, 1, function(x) {
    x[!OptimParam] <- CalibOptions$FixedParam[!OptimParam]
    return(x)
  })
  if (NParam > 1) {
    CandidatesParamR <- t(CandidatesParamR)
  } else {
    CandidatesParamR <- cbind(CandidatesParamR)
  }

  ##Loop_to_test_the_various_candidates______________________________________
  iNewOptim <- 0
  Ncandidates <- nrow(CandidatesParamR)
  if (verbose & Ncandidates > 1) {
    if (PrefilteringType == 1) {
      message("List-Screening in progress (", appendLF = FALSE)
    }
    if (PrefilteringType == 2) {
      message("Grid-Screening in progress (", appendLF = FALSE)
    }
    message("0%", appendLF = FALSE)
  }
  for (iNew in 1:nrow(CandidatesParamR)) {
    if (verbose & Ncandidates > 1) {
      for (k in c(2, 4, 6, 8)) {
        if (iNew == round(k / 10 * Ncandidates)) {
          message(" ", 10 * k, "%", appendLF = FALSE)
        }
      }
    }
    ##Model_run
    Param <- CandidatesParamR[iNew, ]
    OutputsModel <- RunModel(InputsModel, RunOptions, Param, FUN_MOD = FUN_MOD, ...)

    ##Calibration_criterion_computation
    OutputsCrit <- ErrorCrit(InputsCrit, OutputsModel, verbose = FALSE)
    if (!is.na(OutputsCrit$CritValue)) {
      if (OutputsCrit$CritValue * OutputsCrit$Multiplier < CritOptim) {
        CritOptim <- OutputsCrit$CritValue * OutputsCrit$Multiplier
        iNewOptim <- iNew
      }
    }
    ##Storage_of_crit_info
    if (is.null(CritName) | is.null(CritBestValue) | is.null(Multiplier)) {
      CritName      <- OutputsCrit$CritName
      CritBestValue <- OutputsCrit$CritBestValue
      Multiplier    <- OutputsCrit$Multiplier
    }
  }
  if (verbose & Ncandidates > 1) {
    message(" 100%)\n", appendLF = FALSE)
  }


  ##End_of_first_step_Parameter_Screening____________________________________
  ParamStartR <- CandidatesParamR[iNewOptim, ]
  if (!is.matrix(ParamStartR)) {
    ParamStartR <- matrix(ParamStartR, nrow = 1)
  }
  ParamStartT <- FUN_TRANSFO(ParamStartR, "RT")
  CritStart   <- CritOptim
  NRuns       <- NRuns + nrow(CandidatesParamR)
  if (verbose) {
    if (Ncandidates > 1) {
      message(sprintf("\t Screening completed (%s runs)", NRuns))
    }
    if (Ncandidates == 1) {
      message("\t Starting point for steepest-descent local search:")
    }
    message("\t     Param = ", paste(sprintf("%8.3f", ParamStartR), collapse = ", "))
    message(sprintf("\t     Crit. %-12s = %.4f", CritName, CritStart * Multiplier))
  }
  ##Results_archiving________________________________________________________
  HistParamR[1, ] <- ParamStartR
  HistParamT[1, ] <- ParamStartT
  HistCrit[1, ]   <- CritStart




  ##_____Steepest_Descent_Local_Search_______________________________________________________


  ##Definition_of_the_function_creating_new_parameter_sets_through_a_step_by_step_progression_procedure
  ProposeCandidatesLoc <- function(NewParamOptimT, OldParamOptimT, RangesT, OptimParam, Pace) {
    ##Format_checking
    if (nrow(NewParamOptimT) != 1 | nrow(OldParamOptimT) != 1) {
      stop("each input set must be a matrix of one single line")
    }
    if (ncol(NewParamOptimT)!=ncol(OldParamOptimT) | ncol(NewParamOptimT) != length(OptimParam)) {
      stop("each input set must have the same number of values")
    }
    ##Proposal_of_new_parameter_sets ###(local search providing 2 * NParam-1 new sets)
    NParam <- ncol(NewParamOptimT)
    VECT <- NULL
    for (I in 1:NParam) {
      ##We_check_that_the_current_parameter_should_indeed_be_optimised
      if (OptimParam[I]) {
        for (J in 1:2) {
          Sign <- 2 * J - 3   #Sign can be equal to -1 or +1
          ##We_define_the_new_potential_candidate
          Add <- TRUE
          PotentialCandidateT <- NewParamOptimT
          PotentialCandidateT[1, I] <- NewParamOptimT[I] + Sign * Pace
          ##If_we_exit_the_range_of_possible_values_we_go_back_on_the_boundary
          if (PotentialCandidateT[1, I] < RangesT[1, I] ) {
            PotentialCandidateT[1, I] <- RangesT[1, I]
          }
          if (PotentialCandidateT[1, I] > RangesT[2, I]) {
            PotentialCandidateT[1, I] <- RangesT[2, I]
          }
          ##We_check_the_set_is_not_outside_the_range_of_possible_values
          if (NewParamOptimT[I] == RangesT[1, I] & Sign < 0) {
            Add <- FALSE
          }
          if (NewParamOptimT[I] == RangesT[2, I] & Sign > 0) {
            Add <- FALSE
          }
          ##We_check_that_this_set_has_not_been_tested_during_the_last_iteration
          if (identical(PotentialCandidateT, OldParamOptimT)) {
            Add <- FALSE
          }
          ##We_add_the_candidate_to_our_list
          if (Add) {
            VECT <- c(VECT, PotentialCandidateT)
          }
        }
      }
    }
    Output <- NULL
    Output$NewCandidatesT <- matrix(VECT, ncol = NParam, byrow = TRUE)
    return(Output)
  }


  ##Initialisation_of_variables
  if (verbose) {
    message("Steepest-descent local search in progress")
  }
  Pace <- 0.64
  PaceDiag <- rep(0, NParam)
  CLG <- 0.7^(1 / NParam)
  Compt <- 0
  CritOptim <- CritStart
  ##Conversion_of_real_parameter_values
  RangesR <- CalibOptions$SearchRanges
  RangesT <- FUN_TRANSFO(RangesR, "RT")
  NewParamOptimT <- ParamStartT
  OldParamOptimT <- ParamStartT


  ##START_LOOP_ITER_________________________________________________________
  for (ITER in 1:(100 * NParam)) {


    ##Exit_loop_when_Pace_becomes_too_small___________________________________
    if (Pace < 0.01) {
      break
    }


    ##Creation_of_new_candidates______________________________________________
    CandidatesParamT <- ProposeCandidatesLoc(NewParamOptimT, OldParamOptimT, RangesT, OptimParam, Pace)$NewCandidatesT
    CandidatesParamR <- FUN_TRANSFO(CandidatesParamT, "TR")
    ##Remplacement_of_non_optimised_values_____________________________________
    CandidatesParamR <- apply(CandidatesParamR, 1, function(x) {
      x[!OptimParam] <- CalibOptions$FixedParam[!OptimParam]
      return(x)
    })
    if (NParam > 1) {
      CandidatesParamR <- t(CandidatesParamR)
    } else {
      CandidatesParamR <- cbind(CandidatesParamR)
    }


    ##Loop_to_test_the_various_candidates_____________________________________
    iNewOptim <- 0
    for (iNew in 1:nrow(CandidatesParamR)) {
      ##Model_run
      Param <- CandidatesParamR[iNew, ]
      OutputsModel <- RunModel(InputsModel, RunOptions, Param, FUN_MOD = FUN_MOD, ...)
      ##Calibration_criterion_computation
      OutputsCrit <- ErrorCrit(InputsCrit, OutputsModel, verbose = FALSE)
      if (!is.na(OutputsCrit$CritValue)) {
        if (OutputsCrit$CritValue * OutputsCrit$Multiplier < CritOptim) {
          CritOptim <- OutputsCrit$CritValue * OutputsCrit$Multiplier
          iNewOptim <- iNew
        }
      }
    }
    NRuns <- NRuns + nrow(CandidatesParamR)


    ##When_a_progress_has_been_achieved_______________________________________
    if (iNewOptim != 0) {
      ##We_store_the_optimal_set
      OldParamOptimT <- NewParamOptimT
      NewParamOptimT <- matrix(CandidatesParamT[iNewOptim, 1:NParam], nrow = 1)
      Compt <- Compt + 1
      ##When_necessary_we_increase_the_pace ### if_successive_progress_occur_in_a_row
      if (Compt > 2 * NParam) {
        Pace <- Pace * 2
        Compt <- 0
      }
      ##We_update_PaceDiag
      VectPace <- NewParamOptimT-OldParamOptimT
      for (iC in 1:NParam) {
        if (OptimParam[iC]) {
          PaceDiag[iC] <- CLG * PaceDiag[iC] + (1-CLG) * VectPace[iC]
        }
      }
    } else {
      ##When_no_progress_has_been_achieved_we_decrease_the_pace_________________
      Pace <- Pace / 2
      Compt <- 0
    }


    ##Test_of_an_additional_candidate_using_diagonal_progress_________________
    if (ITER > 4 * NParam) {
      NRuns <- NRuns + 1
      iNewOptim <- 0
      iNew <- 1
      CandidatesParamT <- NewParamOptimT+PaceDiag
      if (!is.matrix(CandidatesParamT)) {
        CandidatesParamT <- matrix(CandidatesParamT, nrow = 1)
      }
      ##If_we_exit_the_range_of_possible_values_we_go_back_on_the_boundary
      for (iC in 1:NParam) {
        if (OptimParam[iC]) {
          if (CandidatesParamT[iNew, iC] < RangesT[1, iC]) {
            CandidatesParamT[iNew, iC] <- RangesT[1, iC]
          }
          if (CandidatesParamT[iNew, iC] > RangesT[2, iC]) {
            CandidatesParamT[iNew, iC] <- RangesT[2, iC]
          }
        }
      }
      CandidatesParamR <- FUN_TRANSFO(CandidatesParamT, "TR")
      ##Model_run
      Param <- CandidatesParamR[iNew, ]
      OutputsModel <- RunModel(InputsModel, RunOptions, Param, FUN_MOD = FUN_MOD, ...)
      ##Calibration_criterion_computation
      OutputsCrit <- ErrorCrit(InputsCrit, OutputsModel, verbose = FALSE)
      if (OutputsCrit$CritValue * OutputsCrit$Multiplier < CritOptim) {
        CritOptim <- OutputsCrit$CritValue * OutputsCrit$Multiplier
        iNewOptim <- iNew
      }
      ##When_a_progress_has_been_achieved
      if (iNewOptim != 0) {
        OldParamOptimT <- NewParamOptimT
        NewParamOptimT <- matrix(CandidatesParamT[iNewOptim, 1:NParam], nrow = 1)
      }

    }


    ##Results_archiving_______________________________________________________
    NewParamOptimR       <- FUN_TRANSFO(NewParamOptimT, "TR")
    HistParamR[ITER+1, ] <- NewParamOptimR
    HistParamT[ITER+1, ] <- NewParamOptimT
    HistCrit[ITER+1, ]   <- CritOptim
    ### if (verbose) { cat(paste("\t     Iter ",formatC(ITER,format="d",width=3), "    Crit ",formatC(CritOptim,format="f",digits=4), "    Pace ",formatC(Pace,format="f",digits=4), "\n",sep=""))}



  } ##END_LOOP_ITER_________________________________________________________
  ITER <- ITER - 1


  ##Case_when_the_starting_parameter_set_remains_the_best_solution__________
  if (CritOptim == CritStart & verbose) {
    message("\t No progress achieved")
  }

  ##End_of_Steepest_Descent_Local_Search____________________________________
  ParamFinalR <- NewParamOptimR
  ParamFinalT <- NewParamOptimT
  CritFinal   <- CritOptim
  NIter       <- 1 + ITER
  if (verbose) {
    message(sprintf("\t Calibration completed (%s iterations, %s runs)", NIter, NRuns))
    message("\t     Param = ", paste(sprintf("%8.3f", ParamFinalR), collapse = ", "))
    message(sprintf("\t     Crit. %-12s = %.4f", CritName, CritFinal * Multiplier))
    if (inherits(InputsCrit, "Compo")) {
      listweights  <- OutputsCrit$CritCompo$MultiCritWeights
      listNameCrit <- OutputsCrit$CritCompo$MultiCritNames
      msgForm <- paste(sprintf("%.2f", listweights), listNameCrit, sep = " * ", collapse = ", ")
      msgForm <- unlist(strsplit(msgForm, split = ","))
      msgFormSep <- rep(c(",", ",", ",\n\t\t    "), times = ceiling(length(msgForm)/3))[1:length(msgForm)]
      msgForm <- paste(msgForm, msgFormSep, sep = "", collapse = "")
      msgForm <- gsub("\\,\\\n\\\t\\\t    $|\\,$", "", msgForm)
      message("\tFormula: sum(", msgForm, ")")
    }
  }
  ##Results_archiving_______________________________________________________
  HistParamR <- cbind(HistParamR[1:NIter, ])
  colnames(HistParamR) <- paste0("Param", 1:NParam)
  HistParamT <- cbind(HistParamT[1:NIter, ])
  colnames(HistParamT) <- paste0("Param", 1:NParam)
  HistCrit   <- cbind(HistCrit[1:NIter, ])
  ###colnames(HistCrit) <- paste("HistCrit")

  BoolCrit_Actual <- InputsCrit$BoolCrit
  BoolCrit_Actual[OutputsCrit$Ind_notcomputed] <- FALSE
  MatBoolCrit <- cbind(InputsCrit$BoolCrit, BoolCrit_Actual)
  colnames(MatBoolCrit) <- c("BoolCrit_Requested", "BoolCrit_Actual")


  ##_____Output______________________________________________________________________________
  OutputsCalib <- list(ParamFinalR = as.double(ParamFinalR), CritFinal = CritFinal * Multiplier,
                       NIter = NIter, NRuns = NRuns,
                       HistParamR = HistParamR, HistCrit = HistCrit * Multiplier,
                       MatBoolCrit = MatBoolCrit,
                       CritName = CritName, CritBestValue = CritBestValue)
  class(OutputsCalib) <- c("OutputsCalib", "HBAN")
  return(OutputsCalib)



}

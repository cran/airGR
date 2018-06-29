CreateRunOptions <- function(FUN_MOD, InputsModel, IndPeriod_WarmUp = NULL, IndPeriod_Run, IniStates = NULL, IniResLevels = NULL, 
                             Outputs_Cal = NULL, Outputs_Sim = "all", RunSnowModule, MeanAnSolidPrecip = NULL,  verbose = TRUE) {
  
  if (!missing(RunSnowModule)) {
    warning("argument RunSnowModule is deprecated; please adapt FUN_MOD instead.", call. = FALSE)
  }

  ObjectClass <- NULL

  ##check_FUN_MOD
    BOOL <- FALSE;
    if(identical(FUN_MOD,RunModel_GR4H)){
      ObjectClass <- c(ObjectClass,"GR","hourly"); 
      BOOL <- TRUE; 
    }
    if(identical(FUN_MOD,RunModel_GR4J) | identical(FUN_MOD,RunModel_GR5J) | identical(FUN_MOD,RunModel_GR6J)){
      ObjectClass <- c(ObjectClass,"GR","daily"); 
      BOOL <- TRUE; 
    }
    if(identical(FUN_MOD,RunModel_GR2M)){
      ObjectClass <- c(ObjectClass,"GR","monthly"); 
      BOOL <- TRUE; 
    }
    if(identical(FUN_MOD,RunModel_GR1A)){
      ObjectClass <- c(ObjectClass,"GR","yearly"); 
      BOOL <- TRUE; 
    }
    if(identical(FUN_MOD,RunModel_CemaNeige)){
      ObjectClass <- c(ObjectClass,"CemaNeige","daily");
      BOOL <- TRUE; 
    }
    if(identical(FUN_MOD,RunModel_CemaNeigeGR4J) | identical(FUN_MOD,RunModel_CemaNeigeGR5J) | identical(FUN_MOD,RunModel_CemaNeigeGR6J)){
      ObjectClass <- c(ObjectClass,"GR","CemaNeige","daily");
      BOOL <- TRUE; 
    }
    if(!BOOL){ stop("incorrect FUN_MOD for use in CreateRunOptions \n"); return(NULL); } 


  ##check_InputsModel
    if(!inherits(InputsModel,"InputsModel")){
      stop("InputsModel must be of class 'InputsModel' \n"); return(NULL); } 
    if("GR" %in% ObjectClass & !inherits(InputsModel,"GR")){
      stop("InputsModel must be of class 'GR' \n"); return(NULL); } 
    if("CemaNeige" %in% ObjectClass & !inherits(InputsModel,"CemaNeige")){
      stop("InputsModel must be of class 'CemaNeige' \n"); return(NULL); } 
    if("hourly" %in% ObjectClass & !inherits(InputsModel,"hourly")){
      stop("InputsModel must be of class 'hourly' \n"); return(NULL); } 
    if("daily" %in% ObjectClass & !inherits(InputsModel,"daily")){
      stop("InputsModel must be of class 'daily' \n"); return(NULL); } 
    if("monthly" %in% ObjectClass & !inherits(InputsModel,"monthly")){
      stop("InputsModel must be of class 'monthly' \n"); return(NULL); } 
    if("yearly" %in% ObjectClass & !inherits(InputsModel,"yearly")){
      stop("InputsModel must be of class 'yearly' \n"); return(NULL); } 


  ##check_IndPeriod_Run
    if(!is.vector( IndPeriod_Run)){ stop("IndPeriod_Run must be a vector of numeric values \n"); return(NULL); } 
    if(!is.numeric(IndPeriod_Run)){ stop("IndPeriod_Run must be a vector of numeric values \n"); return(NULL); } 
    if(identical(as.integer(IndPeriod_Run),as.integer(seq(from=IndPeriod_Run[1],to=tail(IndPeriod_Run,1),by=1)))==FALSE){
      stop("IndPeriod_Run must be a continuous sequence of integers \n"); return(NULL); } 
    if(storage.mode(IndPeriod_Run)!="integer"){ stop("IndPeriod_Run should be of type integer \n"); return(NULL); } 


  ##check_IndPeriod_WarmUp
    WTxt <- NULL;
    if(is.null(IndPeriod_WarmUp)){
      WTxt <- paste(WTxt,"\t Model warm up period not defined -> default configuration used \n",sep="");
      ##If_the_run_period_starts_at_the_very_beginning_of_the_time_series
      if(IndPeriod_Run[1]==as.integer(1)){
        IndPeriod_WarmUp <- as.integer(0);
        WTxt <- paste(WTxt,"\t    No data were found for model warm up! \n",sep="");
      ##We_look_for_the_longest_period_preceeding_the_run_period_with_a_maximum_of_one_year
      } else {
        TmpDateR <- InputsModel$DatesR[IndPeriod_Run[1]] - 365*24*60*60; ### minimal date to start the warmup        
        IndPeriod_WarmUp <- which(InputsModel$DatesR==max(InputsModel$DatesR[1],TmpDateR)) : (IndPeriod_Run[1]-1); 
        if("hourly"  %in% ObjectClass){ TimeStep <- as.integer(          60*60); }
        if("daily"   %in% ObjectClass){ TimeStep <- as.integer(       24*60*60); }
        if("monthly" %in% ObjectClass){ TimeStep <- as.integer( 30.44*24*60*60); }
        if("yearly"  %in% ObjectClass){ TimeStep <- as.integer(365.25*24*60*60); }
        if(length(IndPeriod_WarmUp)*TimeStep/(365*24*60*60)>=1){ 
        WTxt <- paste(WTxt,"\t    The year preceding the run period is used \n",sep="");
        } else {
        WTxt <- paste(WTxt,"\t    Less than a year (without missing values) was found for model warm up: \n",sep="");
        WTxt <- paste(WTxt,"\t    (",length(IndPeriod_WarmUp)," time-steps are used for initialisation) \n",sep=""); 
        }
      }
    }
    if(!is.null(IndPeriod_WarmUp)){
      if(!is.vector( IndPeriod_WarmUp)){ stop("IndPeriod_Run must be a vector of numeric values \n"); return(NULL); } 
      if(!is.numeric(IndPeriod_WarmUp)){ stop("IndPeriod_Run must be a vector of numeric values \n"); return(NULL); } 
      if(storage.mode(IndPeriod_WarmUp)!="integer"){ stop("IndPeriod_Run should be of type integer \n"); return(NULL); } 
      if(identical(IndPeriod_WarmUp,as.integer(0))){
        WTxt <- paste(WTxt,"\t No warm up period is used! \n",sep=""); }
      if((IndPeriod_Run[1]-1)!=tail(IndPeriod_WarmUp,1) & !identical(IndPeriod_WarmUp,as.integer(0))){ 
        WTxt <- paste(WTxt,"\t Model warm up period is not directly before the model run period \n",sep=""); }
    }
    if(!is.null(WTxt) & verbose){ warning(WTxt); }

    
    ## check IniResLevels    
    if ("GR" %in% ObjectClass & ("monthly" %in% ObjectClass | "daily" %in% ObjectClass | "hourly" %in% ObjectClass)) {
      if (!is.null(IniResLevels)) {
        if (!is.vector(IniResLevels) | !is.numeric(IniResLevels) | any(is.na(IniResLevels))) {
          stop("IniResLevels must be a vector of numeric values \n")
          return(NULL)
        }
        if ((identical(FUN_MOD, RunModel_GR4H) |
             identical(FUN_MOD, RunModel_GR4J) | identical(FUN_MOD, RunModel_CemaNeigeGR4J) |
             identical(FUN_MOD, RunModel_GR5J) | identical(FUN_MOD, RunModel_CemaNeigeGR5J) |
             identical(FUN_MOD, RunModel_GR2M)) &
            length(IniResLevels) != 2) {
          stop("The length of IniResLevels must be 2 for the chosen FUN_MOD \n")
          return(NULL)
        }
        if ((identical(FUN_MOD,RunModel_GR6J) | identical(FUN_MOD,RunModel_CemaNeigeGR6J)) &
            length(IniResLevels) != 3) {
          stop("The length of IniResLevels must be 3 for the chosen FUN_MOD \n")
          return(NULL)
        }
      } else if (is.null(IniStates)) {
        if (identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) {
          IniResLevels <- as.double(c(0.3, 0.5, 0))
        } else {
          IniResLevels <- as.double(c(0.3, 0.5, NA))
        }
      }
    } else {
      if (!is.null(IniResLevels)) {
        stop("IniResLevels can only be used with monthly or daily or hourly GR models \n")
      }
    }
  ## check IniStates
    if (is.null(IniStates) & is.null(IniResLevels) & verbose) {
      warning("\t Model states initialisation not defined -> default configuration used \n")
    }
    if (!is.null(IniStates) & !is.null(IniResLevels) & verbose) {
      warning("\t IniStates and IniResLevels are both defined -> Store levels are taken from IniResLevels \n")
    }
    if("CemaNeige" %in% ObjectClass){ NLayers <- length(InputsModel$LayerPrecip); } else { NLayers <- 0; }
    NState <- NULL;
    if("GR" %in% ObjectClass | "CemaNeige" %in% ObjectClass){
      if("hourly"  %in% ObjectClass){ NState <- 7 + 3*24*20 }
      if("daily"   %in% ObjectClass){ NState <- 7 + 3*20 + 2*NLayers }
      if("monthly" %in% ObjectClass){ NState <- 2; }
      if("yearly"  %in% ObjectClass){ NState <- 1; }
    }
    if (!is.null(IniStates)) {
      if (!inherits(IniStates, "IniStates")) {
        stop("IniStates must be an object of class IniStates\n")
        return(NULL)
      }
      if (sum(ObjectClass %in% class(IniStates)) < 2) {
        stop(paste0("Non convenient IniStates for this FUN_MOD\n"))
        return(NULL)
      }
      if (identical(FUN_MOD, RunModel_GR1A) & !is.null(IniStates)) { ## GR1A
        stop(paste0("IniStates is not available for this FUN_MOD\n"))
        return(NULL)
      }
      if ((identical(FUN_MOD, RunModel_GR5J) | identical(FUN_MOD, RunModel_CemaNeigeGR5J)) & !all(is.na(IniStates$UH$UH1))) { ## GR5J
        stop(paste0("Non convenient IniStates for this FUN_MOD. In IniStates, UH1 has to be a vector of NA for GR5J \n"))
        return(NULL)
      }
      if ((identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) & is.na(IniStates$Store$Exp)) { ## GR6J
        stop(paste0("Non convenient IniStates for this FUN_MOD. GR6J needs an exponential store value in IniStates \n"))
        return(NULL)
      }
      if (!(identical(FUN_MOD, RunModel_GR6J) | identical(FUN_MOD, RunModel_CemaNeigeGR6J)) & !is.na(IniStates$Store$Exp)) { ## except GR6J
        stop(paste0("Non convenient IniStates for this FUN_MOD. No exponential store value needed in IniStates \n"))
        return(NULL)
      }
      # if (length(na.omit(unlist(IniStates))) != NState) {
      #   stop(paste0("The length of IniStates must be ", NState, " for the chosen FUN_MOD \n"))
      #   return(NULL)
      # }
      if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$G  ))) {
        IniStates$CemaNeigeLayers$G   <- NULL
      }
      if (!"CemaNeige" %in% ObjectClass & any(is.na(IniStates$CemaNeigeLayers$eTG))) {
        IniStates$CemaNeigeLayers$eTG <- NULL
      }
      IniStates$Store$Rest <- rep(NA, 4)
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
      Outputs_all <- NULL;
      if(identical(FUN_MOD,RunModel_GR4H)){
        Outputs_all <- c(Outputs_all,"PotEvap","Precip","Prod","AE","Perc","PR","Q9","Q1","Rout","Exch","AExch","QR","QD","Qsim"); }
      if(identical(FUN_MOD,RunModel_GR4J) | identical(FUN_MOD,RunModel_CemaNeigeGR4J)){
        Outputs_all <- c(Outputs_all,"PotEvap", "Precip", "Prod", "Pn", "Ps", "AE", "Perc", "PR", "Q9", "Q1", "Rout", "Exch",
                         "AExch1", "AExch2", "AExch", "QR", "QD", "Qsim"); }
      if(identical(FUN_MOD,RunModel_GR5J) | identical(FUN_MOD,RunModel_CemaNeigeGR5J)){
        Outputs_all <- c(Outputs_all,"PotEvap", "Precip", "Prod", "Pn", "Ps", "AE", "Perc", "PR", "Q9", "Q1", "Rout", "Exch",
                         "AExch1", "AExch2", "AExch", "QR", "QD", "Qsim"); }
      if(identical(FUN_MOD,RunModel_GR6J) | identical(FUN_MOD,RunModel_CemaNeigeGR6J)){
        Outputs_all <- c(Outputs_all,"PotEvap", "Precip", "Prod", "Pn", "Ps", "AE", "Perc", "PR", "Q9", "Q1", "Rout", "Exch",
                         "AExch1", "AExch2", "AExch", "QR", "QRExp", "Exp", "QD", "Qsim"); }
      if(identical(FUN_MOD,RunModel_GR2M)){
        Outputs_all <- c(Outputs_all,"PotEvap", "Precip", "AE", "Pn", "Perc", "PR", "Exch", "Prod", "Rout", "Qsim"); }
      if(identical(FUN_MOD,RunModel_GR1A)){
        Outputs_all <- c(Outputs_all,"PotEvap","Precip","Qsim"); }
      if("CemaNeige" %in% ObjectClass){
        Outputs_all <- c(Outputs_all,"Pliq","Psol","SnowPack","ThermalState","Gratio","PotMelt","Melt","PliqAndMelt", "Temp"); }

    ##check_Outputs_Sim
      if(!is.vector(   Outputs_Sim)){ stop("Outputs_Sim must be a vector of characters \n"); return(NULL);  }
      if(!is.character(Outputs_Sim)){ stop("Outputs_Sim must be a vector of characters \n"); return(NULL);  }
      if(sum(is.na(Outputs_Sim))!=0){ stop("Outputs_Sim must not contain NA \n"); return(NULL);  }
      if("all" %in% Outputs_Sim){ Outputs_Sim <- c("DatesR",Outputs_all,"StateEnd");  }
      Test <- which(Outputs_Sim %in% c("DatesR",Outputs_all,"StateEnd") == FALSE); if(length(Test)!=0){ 
        stop(paste("Outputs_Sim is incorrectly defined: ",paste(Outputs_Sim[Test],collapse=", ")," not found \n",sep="")); return(NULL); } 
      Outputs_Sim <- Outputs_Sim[!duplicated(Outputs_Sim)];

    ##check_Outputs_Cal
      if(is.null(Outputs_Cal)){
        if("GR" %in% ObjectClass                               ){ Outputs_Cal <- c("Qsim"); }
        if("CemaNeige" %in% ObjectClass                        ){ Outputs_Cal <- c("all"); }
        if("GR" %in% ObjectClass & "CemaNeige" %in% ObjectClass){ Outputs_Cal <- c("PliqAndMelt","Qsim"); }
      } else {
        if(!is.vector(   Outputs_Cal)){ stop("Outputs_Cal must be a vector of characters \n"); return(NULL);  }
        if(!is.character(Outputs_Cal)){ stop("Outputs_Cal must be a vector of characters \n"); return(NULL);  }
        if(sum(is.na(Outputs_Cal))!=0){ stop("Outputs_Cal must not contain NA \n"); return(NULL);  }
      }
      if("all" %in% Outputs_Cal){ Outputs_Cal <- c("DatesR",Outputs_all,"StateEnd");  }
      Test <- which(Outputs_Cal %in% c("DatesR",Outputs_all,"StateEnd") == FALSE); if(length(Test)!=0){ 
        stop(paste("Outputs_Cal is incorrectly defined: ",paste(Outputs_Cal[Test],collapse=", ")," not found \n",sep="")); return(NULL); } 
      Outputs_Cal <- Outputs_Cal[!duplicated(Outputs_Cal)];


  ##check_MeanAnSolidPrecip
    if("CemaNeige" %in% ObjectClass & is.null(MeanAnSolidPrecip)){
      NLayers <- length(InputsModel$LayerPrecip);
      SolidPrecip <- NULL; for(iLayer in 1:NLayers){
        if(iLayer==1){ SolidPrecip <- InputsModel$LayerFracSolidPrecip[[1]]*InputsModel$LayerPrecip[[iLayer]]/NLayers;
              } else { SolidPrecip <- SolidPrecip + InputsModel$LayerFracSolidPrecip[[iLayer]]*InputsModel$LayerPrecip[[iLayer]]/NLayers; } }
      Factor <- NULL;
      if(inherits(InputsModel,"hourly" )){ Factor <- 365.25*24; }
      if(inherits(InputsModel,"daily"  )){ Factor <-    365.25; }
      if(inherits(InputsModel,"monthly")){ Factor <-        12; }
      if(inherits(InputsModel,"yearly" )){ Factor <-         1; }
      if(is.null(Factor)){ stop("InputsModel must be of class 'hourly', 'daily', 'monthly' or 'yearly' \n"); return(NULL);  }
      MeanAnSolidPrecip <- rep(mean(SolidPrecip)*Factor,NLayers); ### default value: same Gseuil for all layers
      if(verbose){ warning("\t MeanAnSolidPrecip not defined -> it was automatically set to c(",paste(round(MeanAnSolidPrecip),collapse=","),") \n"); }
    }
    if("CemaNeige" %in% ObjectClass & !is.null(MeanAnSolidPrecip)){
      if(!is.vector( MeanAnSolidPrecip)    ){ stop(paste("MeanAnSolidPrecip must be a vector of numeric values \n",sep="")); return(NULL);  }
      if(!is.numeric(MeanAnSolidPrecip)    ){ stop(paste("MeanAnSolidPrecip must be a vector of numeric values \n",sep="")); return(NULL);  }
      if(length(MeanAnSolidPrecip)!=NLayers){ stop(paste("MeanAnSolidPrecip must be a numeric vector of length ",NLayers," \n",sep="")); return(NULL);  }
    }


  ##check_PliqAndMelt
    if("GR" %in% ObjectClass & "CemaNeige" %in% ObjectClass){
      if("PliqAndMelt" %in% Outputs_Cal == FALSE & "all" %in% Outputs_Cal == FALSE){
        WTxt <- NULL;
        WTxt <- paste(WTxt,"\t PliqAndMelt was not defined in Outputs_Cal but is needed to feed the hydrological model with the snow modele outputs \n",sep="");
        WTxt <- paste(WTxt,"\t -> it was automatically added \n",sep="");
        if(!is.null(WTxt) & verbose){ warning(WTxt); }
        Outputs_Cal <- c(Outputs_Cal,"PliqAndMelt"); }
      if("PliqAndMelt" %in% Outputs_Sim == FALSE & "all" %in% Outputs_Sim == FALSE){
        WTxt <- NULL;
        WTxt <- paste(WTxt,"\t PliqAndMelt was not defined in Outputs_Sim but is needed to feed the hydrological model with the snow modele outputs \n",sep="");
        WTxt <- paste(WTxt,"\t -> it was automatically added \n",sep="");
        if(!is.null(WTxt) & verbose){ warning(WTxt); }
        Outputs_Sim <- c(Outputs_Sim,"PliqAndMelt"); }
    }


  ##Create_RunOptions
    RunOptions <- list(IndPeriod_WarmUp=IndPeriod_WarmUp,IndPeriod_Run=IndPeriod_Run,IniStates=IniStates,IniResLevels=IniResLevels,
                       Outputs_Cal=Outputs_Cal,Outputs_Sim=Outputs_Sim);
    if("CemaNeige" %in% ObjectClass){
        RunOptions <- c(RunOptions,list(MeanAnSolidPrecip=MeanAnSolidPrecip));    }
    class(RunOptions) <- c("RunOptions",ObjectClass);
    return(RunOptions);


}


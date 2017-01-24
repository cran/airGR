plot.OutputsModel <- function(x, Qobs = NULL, IndPeriod_Plot = NULL, BasinArea = NULL, which = "all", log_scale = FALSE, verbose = TRUE, ...){
  
  OutputsModel <- x

  if(!inherits(OutputsModel, "GR") & !inherits(OutputsModel, "CemaNeige")){
    stop(paste("OutputsModel not in the correct format for default plotting \n", sep = ""))
    return(NULL)
  }

  BOOL_Dates <- FALSE; 
      if("DatesR" %in% names(OutputsModel)){ BOOL_Dates <- TRUE; }
  BOOL_Pobs <- FALSE; 
      if("Precip" %in% names(OutputsModel)){ BOOL_Pobs <- TRUE; }
  BOOL_Qsim <- FALSE; 
      if("Qsim"   %in% names(OutputsModel)){ BOOL_Qsim <- TRUE; }
  BOOL_Qobs <- FALSE;
      if(BOOL_Qsim & length(Qobs) == length(OutputsModel$Qsim)){ if(sum(is.na(Qobs)) != length(Qobs)){ BOOL_Qobs <- TRUE; } }
  BOOL_Snow <- FALSE;
      if("CemaNeigeLayers" %in% names(OutputsModel)){ if("SnowPack" %in% names(OutputsModel$CemaNeigeLayers[[1]])){ BOOL_Snow <- TRUE; } }
  BOOL_Psol <- FALSE;
      if("CemaNeigeLayers" %in% names(OutputsModel)){ if("Psol"     %in% names(OutputsModel$CemaNeigeLayers[[1]])){ BOOL_Psol <- TRUE; } }


  if( is.null(     which)){ stop("which must be a vector of character \n"); return(NULL); } 
  if(!is.vector(   which)){ stop("which must be a vector of character \n"); return(NULL); } 
  if(!is.character(which)){ stop("which must be a vector of character \n"); return(NULL); } 
  if (any(!which %in% c("all", "Precip", 'Temp', "SnowPack", "Flows", "Regime", "CumFreq", "CorQQ"))) {
    stop("Incorrect element found in argument which:\nit can only contain 'all', 'Precip', 'Temp', 'SnowPack', 'Flows', 'Regime', 'CumFreq' or 'CorQQ'")
    return(NULL)
  }
  if (all(which %in% c("Temp", "SnowPack")) & !inherits(OutputsModel, "CemaNeige")) {
    stop("Incorrect element found in argument which:\nwithout CemaNeige it can only contain 'all', 'Precip', 'Flows', 'Regime', 'CumFreq' or 'CorQQ'")
    return(NULL)
  }
  if (length(unique(which %in% c("Temp", "SnowPack"))) == 2 & !inherits(OutputsModel, "CemaNeige")) {
    warning("Incorrect element found in argument which:\nit can only contain 'all', 'Precip', 'Flows', 'Regime', 'CumFreq' or 'CorQQ'\nwithout CemaNeige 'Temp' and 'SnowPack' are not available")
  }  
  
  if ("all" %in% which) {
    which <- c("Precip", "Temp", "SnowPack", "Flows", "Regime", "CumFreq", "CorQQ")
  }


  if(!BOOL_Dates){
    stop(paste("OutputsModel must contain at least DatesR to allow plotting \n", sep = "")); return(NULL); }
  if(inherits(OutputsModel, "GR") & !BOOL_Qsim){
    stop(paste("OutputsModel must contain at least Qsim to allow plotting \n", sep = "")); return(NULL); }

  if(BOOL_Dates){
    MyRollMean1 <- function(x, n){
      return(filter(x, rep(1/n, n), sides = 2)); }
    MyRollMean2 <- function(x, n){
      return(filter(c(tail(x, n%/%2), x, x[1:(n%/%2)]), rep(1/n, n), sides = 2)[(n%/%2+1):(length(x)+n%/%2)]); }
    BOOL_TS  <- FALSE;
    TimeStep <- difftime(tail(OutputsModel$DatesR, 1), tail(OutputsModel$DatesR, 2), units = "secs")[[1]];
    if(inherits(OutputsModel, "hourly" ) & TimeStep %in% (                  60*60)){ BOOL_TS <- TRUE; NameTS <- "hour" ; plotunit <- "[mm/h]";     formatAxis <- "%m/%Y"; }
    if(inherits(OutputsModel, "daily"  ) & TimeStep %in% (               24*60*60)){ BOOL_TS <- TRUE; NameTS <- "day"  ; plotunit <- "[mm/d]";     formatAxis <- "%m/%Y"; }
    if(inherits(OutputsModel, "monthly") & TimeStep %in% (c(28, 29, 30, 31)*24*60*60)){ BOOL_TS <- TRUE; NameTS <- "month"; plotunit <- "[mm/month]"; formatAxis <- "%m/%Y"; }
    if(inherits(OutputsModel, "yearly" ) & TimeStep %in% (    c(365, 366)*24*60*60)){ BOOL_TS <- TRUE; NameTS <- "year" ; plotunit <- "[mm/y]";     formatAxis <- "%Y"   ; }
    if(!BOOL_TS){ stop(paste("the time step of the model inputs could not be found \n", sep = "")); return(NULL); } 
  }
  if(length(IndPeriod_Plot) == 0){ IndPeriod_Plot <- 1:length(OutputsModel$DatesR); }
  if(inherits(OutputsModel, "CemaNeige")){ NLayers <- length(OutputsModel$CemaNeigeLayers); }
  PsolLayerMean <- NULL; if(BOOL_Psol){
    for(iLayer in 1:NLayers){
      if(iLayer == 1){ PsolLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$Psol/NLayers; 
            } else { PsolLayerMean <- PsolLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$Psol/NLayers; } } }
  BOOL_QobsZero <- FALSE; if(BOOL_Qobs){ SelectQobsNotZero <- (round(Qobs[IndPeriod_Plot]             , 4) != 0); BOOL_QobsZero <- sum(!SelectQobsNotZero, na.rm = TRUE)>0; }
  BOOL_QsimZero <- FALSE; if(BOOL_Qsim){ SelectQsimNotZero <- (round(OutputsModel$Qsim[IndPeriod_Plot], 4) != 0); BOOL_QsimZero <- sum(!SelectQsimNotZero, na.rm = TRUE)>0; }
  if(BOOL_QobsZero & verbose){ warning("\t zeroes detected in Qobs -> some plots in the log space will not be created using all time-steps \n"); }
  if(BOOL_QsimZero & verbose){ warning("\t zeroes detected in Qsim -> some plots in the log space will not be created using all time-steps \n"); }
  BOOL_FilterZero <- TRUE;

  ##Plots_choices
  BOOLPLOT_Precip   <- ( "Precip"   %in% which & BOOL_Pobs )
  BOOLPLOT_Temp     <- ( "Temp"     %in% which & BOOL_Snow )
  BOOLPLOT_SnowPack <- ( "SnowPack" %in% which & BOOL_Snow )
  BOOLPLOT_Flows    <- ( "Flows"    %in% which & (BOOL_Qsim | BOOL_Qobs) )
  BOOLPLOT_Regime   <- ( "Regime"   %in% which & BOOL_TS & BOOL_Qsim & (NameTS %in% c("hour", "day", "month")) )
  BOOLPLOT_CumFreq  <- ( "CumFreq"  %in% which & (BOOL_Qsim | BOOL_Qobs) & BOOL_FilterZero )
  BOOLPLOT_CorQQ    <- ( "CorQQ"    %in% which & (BOOL_Qsim & BOOL_Qobs) & BOOL_FilterZero )


  ##Options
  BLOC <- TRUE; if(BLOC){
    cexaxis <- 1.0; cexlab <- 0.9; cexleg = 1.0; lwdLine = 1.8; lineX = 2.6; lineY = 2.6; bgleg <- NA

    matlayout <- NULL; iPlot <- 0;
    Sum1 <- sum(c(BOOLPLOT_Precip, BOOLPLOT_SnowPack, BOOLPLOT_Flows))
    Sum2 <- sum(c(BOOLPLOT_Regime, BOOLPLOT_CumFreq, BOOLPLOT_CorQQ))
    if(BOOLPLOT_Precip){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+1, iPlot+1)); iPlot <- iPlot+1; }
    if(BOOLPLOT_Temp){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+1, iPlot+1), c(iPlot+1, iPlot+1, iPlot+1)); iPlot <- iPlot+1; }      
    if(BOOLPLOT_SnowPack){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+1, iPlot+1), c(iPlot+1, iPlot+1, iPlot+1)); iPlot <- iPlot+1; }
    if(BOOLPLOT_Flows){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+1, iPlot+1), c(iPlot+1, iPlot+1, iPlot+1)); iPlot <- iPlot+1; }
    if((Sum1 >= 1 & Sum2 != 0) | (Sum1 == 0 & Sum2 == 3)){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+2, iPlot+3), c(iPlot+1, iPlot+2, iPlot+3)); iPlot <- iPlot+3; }
    if(Sum1 == 0 & Sum2 == 2){ 
      matlayout <- rbind(matlayout, c(iPlot+1, iPlot+2)); iPlot <- iPlot+2; }
    if(Sum1 == 0 & Sum2 == 1){ 
      matlayout <- rbind(matlayout, iPlot+1); iPlot <- iPlot+1; }
    iPlotMax <- iPlot;

    isRStudio <- Sys.getenv("RSTUDIO") == "1";
    if(!isRStudio){ 
      if(Sum1 == 1 & Sum2 == 0){ width = 10; height = 05; }
      if(Sum1 == 1 & Sum2 != 0){ width = 10; height = 07; }
      if(Sum1 == 2 & Sum2 == 0){ width = 10; height = 05; }
      if(Sum1 == 2 & Sum2 != 0){ width = 10; height = 07; }
      if(Sum1 == 3 & Sum2 == 0){ width = 10; height = 07; }
      if(Sum1 == 3 & Sum2 != 0){ width = 10; height = 10; }
      if(Sum1 == 0 & Sum2 == 1){ width = 05; height = 05; }
      if(Sum1 == 0 & Sum2 == 2){ width = 10; height = 04; }
      if(Sum1 == 0 & Sum2 == 3){ width = 10; height = 03; }
      dev.new(width = width, height = height)
    }
    layout(matlayout);

    Xaxis <- 1:length(IndPeriod_Plot);
    if(BOOL_Dates){
      if(NameTS %in% c("hour", "day", "month")){
      Seq1 <- which(OutputsModel$DatesR[IndPeriod_Plot]$mday == 1 & OutputsModel$DatesR[IndPeriod_Plot]$mon %in% c(0, 3, 6, 9));
      Seq2 <- which(OutputsModel$DatesR[IndPeriod_Plot]$mday == 1 & OutputsModel$DatesR[IndPeriod_Plot]$mon == 0);
      Labels2 <- format(OutputsModel$DatesR[IndPeriod_Plot], format = formatAxis)[Seq2];
      }
      if(NameTS %in% c("year")){
      Seq1 <- 1:length(OutputsModel$DatesR[IndPeriod_Plot]);
      Seq2 <- 1:length(OutputsModel$DatesR[IndPeriod_Plot]);
      Labels2 <- format(OutputsModel$DatesR[IndPeriod_Plot], format = formatAxis)[Seq2];
      }
    }

    if(!is.null(BasinArea)){
      Factor_MMH_M3S <- BasinArea/(             60*60/1000); 
      Factor_MMD_M3S <- BasinArea/(          24*60*60/1000); 
      Factor_MMM_M3S <- BasinArea/(365.25/12*24*60*60/1000); 
      Factor_MMY_M3S <- BasinArea/(   365.25*24*60*60/1000); 
      if(NameTS == "hour" ){ Factor_UNIT_M3S <- Factor_MMH_M3S; }
      if(NameTS == "day"  ){ Factor_UNIT_M3S <- Factor_MMD_M3S; }
      if(NameTS == "month"){ Factor_UNIT_M3S <- Factor_MMM_M3S; }
      if(NameTS == "year" ){ Factor_UNIT_M3S <- Factor_MMY_M3S; }
    }
  }

  kPlot <- 0
  
  ## vector of Q values for the y-axis when it is expressed in 
  seqDATA1 <- log(c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000))
  seqDATA2 <- exp(seqDATA1)

  ##Precip
  if(BOOLPLOT_Precip){
    kPlot <- kPlot+1; mar <- c(3, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    ylim1 <- range(OutputsModel$Precip[IndPeriod_Plot], na.rm = TRUE); ylim2 <- ylim1 * c(1.0, 1.1);  ylim2 <- rev(ylim2); 
    lwdP <- 0.7; if(NameTS %in% c("month", "year")){ lwdP <- 2; }
    plot(Xaxis, OutputsModel$Precip[IndPeriod_Plot], type = "h", ylim = ylim2, col = "royalblue", lwd = lwdP, xaxt = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", ...);
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cexaxis)
    par(las = 0); mtext(side = 2, paste("precip.", plotunit, sep = " "), line = lineY, cex = cexlab, adj = 1); par(las = 0); 
    if(BOOL_Psol){
      legend("bottomright", c("solid","liquid"), col = c("lightblue", "royalblue"), lty = c(1, 1), lwd = c(lwdLine, lwdLine), bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
      par(new = TRUE);
      plot(Xaxis, PsolLayerMean[IndPeriod_Plot], type = "h", ylim = ylim2, col = "lightblue", lwd = lwdP, xaxt = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", ...);
    }
    if(BOOL_Dates){
      axis(side = 1, at = Seq1, labels = FALSE, cex.axis = cexaxis);
      axis(side = 1, at = Seq2, labels = Labels2, lwd.ticks = 1.5, cex.axis = cexaxis);
    } else { axis(side = 1, at = pretty(Xaxis), labels = pretty(Xaxis), cex.axis = cexaxis); }
  }
  
  
  ##Temp
  if(BOOLPLOT_Temp){
    kPlot <- kPlot+1; mar <- c(3, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    ylim1 <- c(+99999, -99999)
    for(iLayer in 1:NLayers){
      ylim1[1] <- min(ylim1[1], OutputsModel$CemaNeigeLayers[[iLayer]]$Temp);
      ylim1[2] <- max(ylim1[2], OutputsModel$CemaNeigeLayers[[iLayer]]$Temp);
      if(iLayer == 1){ SnowPackLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$Temp/NLayers;
      } else { SnowPackLayerMean <- SnowPackLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$Temp/NLayers; }
    }
    plot(SnowPackLayerMean[IndPeriod_Plot], type = "n", ylim = ylim1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    for(iLayer in 1:NLayers){ lines(OutputsModel$CemaNeigeLayers[[iLayer]]$Temp[IndPeriod_Plot], lty = 3, col = "orchid", lwd = lwdLine*0.8); }
    abline(h = 0, col = "grey", lty = 2)
    lines(SnowPackLayerMean[IndPeriod_Plot], type = "l", lwd = lwdLine*1.0, col = "darkorchid4")
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cexaxis)
    par(las = 0); mtext(side = 2, expression(paste("temp. [", degree, "C]", sep = "")),  padj = 0.2, line = lineY, cex = cexlab); par(las = 0);
    legend("topright", c("mean", "layers"), col = c("darkorchid4", "orchid"), lty = c(1, 3), lwd = c(lwdLine*1.0, lwdLine*0.8), bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
    if(BOOL_Dates){
      axis(side = 1, at = Seq1, labels = FALSE, cex.axis = cexaxis);
      axis(side = 1, at = Seq2, labels = Labels2, lwd.ticks = 1.5, cex.axis = cexaxis);
    } else { axis(side = 1, at = pretty(Xaxis), labels = pretty(Xaxis), cex.axis = cexaxis); }
  }
  

  ##SnowPack
  if(BOOLPLOT_SnowPack){
    kPlot <- kPlot+1; mar <- c(3, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    ylim1 <- c(+99999, -99999)
    for(iLayer in 1:NLayers){
      ylim1[1] <- min(ylim1[1], OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack);
      ylim1[2] <- max(ylim1[2], OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack);
      if(iLayer == 1){ SnowPackLayerMean <- OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack/NLayers; 
            } else { SnowPackLayerMean <- SnowPackLayerMean + OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack/NLayers; }
    }
    plot(SnowPackLayerMean[IndPeriod_Plot], type = "l", ylim = ylim1, lwd = lwdLine*1.2, col = "royalblue", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    for(iLayer in 1:NLayers){ lines(OutputsModel$CemaNeigeLayers[[iLayer]]$SnowPack[IndPeriod_Plot], lty = 3, col = "royalblue", lwd = lwdLine*0.8); }
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cexaxis)
    par(las = 0); mtext(side = 2, paste("snow pack", "[mm]", sep = " "), line = lineY, cex = cexlab); par(las = 0);
    legend("topright", c("mean", "layers"), col = c("royalblue", "royalblue"), lty = c(1, 3), lwd = c(lwdLine*1.2, lwdLine*0.8), bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
    if(BOOL_Dates){
      axis(side = 1, at = Seq1, labels = FALSE, cex.axis = cexaxis);
      axis(side = 1, at = Seq2, labels = Labels2, lwd.ticks = 1.5, cex.axis = cexaxis);
    } else { axis(side = 1, at = pretty(Xaxis), labels = pretty(Xaxis), cex.axis = cexaxis); }
  }


  ##Flows
  if(BOOLPLOT_Flows & log_scale) {
    kPlot <- kPlot+1; mar <- c(3, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)

    DATA2 <- Qobs
    DATA2[!SelectQobsNotZero] <- mean(Qobs, na.rm = TRUE) / 10000
    DATA2 <- log(DATA2)

    DATA3 <- OutputsModel$Qsim
    DATA2[!SelectQsimNotZero] <- mean(OutputsModel$Qsim, na.rm = TRUE) / 10000
    DATA3 <- log(DATA3)

    ylim1 <- range(DATA3[IndPeriod_Plot], na.rm = TRUE);
    if(BOOL_Qobs){ ylim1 <- range(c(ylim1, DATA2[IndPeriod_Plot]), na.rm = TRUE); }
    ylim2 <- c(ylim1[1], 1.2*ylim1[2]);
    plot(Xaxis, rep(NA, length(Xaxis)), type = "n", ylim = ylim2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...);
    txtleg <- NULL; colleg <- NULL;
    if(BOOL_Qobs){ lines(Xaxis, DATA2[IndPeriod_Plot], lwd = lwdLine, lty = 1, col = "black"); txtleg <- c(txtleg, "observed"); colleg <- c(colleg, "black"); }
    if(BOOL_Qsim){ lines(Xaxis, DATA3[IndPeriod_Plot], lwd = lwdLine, lty = 1, col = "orangered"); txtleg <- c(txtleg, "simulated"); colleg <- c(colleg, "orangered"); }
    axis(side = 2, at = seqDATA1, labels = seqDATA2, cex.axis = cexaxis)
    par(las = 0); mtext(side = 2, paste("flow", plotunit, sep = " "), line = lineY, cex = cexlab); par(las = 0);
    if(!is.null(BasinArea)){
      Factor <- Factor_UNIT_M3S;
      axis(side = 4, at = pretty(ylim1*Factor)/Factor, labels = pretty(ylim1*Factor), cex.axis = cexaxis);
      par(las = 0); mtext(side = 4, paste("flow", "m3/s", sep = " "), line = lineY, cex = cexlab); par(las = 0); }
    if(BOOL_Dates){
      axis(side = 1, at = Seq1, labels = FALSE, cex.axis = cexaxis);
      axis(side = 1, at = Seq2, labels = Labels2, lwd.ticks = 1.5, cex.axis = cexaxis);
    } else { axis(side = 1, at = pretty(Xaxis), labels = pretty(Xaxis), cex.axis = cexaxis); }
    legend("topright", txtleg, col = colleg, lty = 1, lwd = lwdLine, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
  }
  if(BOOLPLOT_Flows & !log_scale){
    kPlot <- kPlot+1; mar <- c(3, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    ylim1 <- range(OutputsModel$Qsim[IndPeriod_Plot], na.rm = TRUE);
    if(BOOL_Qobs){ ylim1 <- range(c(ylim1, Qobs[IndPeriod_Plot]), na.rm = TRUE); }
    ylim2 <- c(ylim1[1], 1.2*ylim1[2]);
    plot(Xaxis, rep(NA, length(Xaxis)), type = "n", ylim = ylim2, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...);
    txtleg <- NULL; colleg <- NULL;
    if(BOOL_Qobs){ lines(Xaxis, Qobs[IndPeriod_Plot], lwd = lwdLine, lty = 1, col = "black"); txtleg <- c(txtleg, "observed"); colleg <- c(colleg, "black"); }
    if(BOOL_Qsim){ lines(Xaxis, OutputsModel$Qsim[IndPeriod_Plot], lwd = lwdLine, lty = 1, col = "orangered"); txtleg <- c(txtleg, "simulated"); colleg <- c(colleg, "orangered"); }
    axis(side = 2, at = pretty(ylim1), labels = pretty(ylim1), cex.axis = cexaxis)
    par(las = 0); mtext(side = 2, paste("flow", plotunit, sep = " "), line = lineY, cex = cexlab); par(las = 0);
    if(!is.null(BasinArea)){
      Factor <- Factor_UNIT_M3S;
      axis(side = 4, at = pretty(ylim1*Factor)/Factor, labels = pretty(ylim1*Factor), cex.axis = cexaxis);
      par(las = 0); mtext(side = 4, paste("flow", "m3/s", sep = " "), line = lineY, cex = cexlab); par(las = 0); }
    if(BOOL_Dates){
      axis(side = 1, at = Seq1, labels = FALSE, cex.axis = cexaxis);
      axis(side = 1, at = Seq2, labels = Labels2, lwd.ticks = 1.5, cex.axis = cexaxis);
    } else { axis(side = 1, at = pretty(Xaxis), labels = pretty(Xaxis), cex.axis = cexaxis); }
    legend("topright", txtleg, col = colleg, lty = 1, lwd = lwdLine, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
  }


  ##Regime
  if(BOOLPLOT_Regime){
    kPlot <- kPlot+1; mar <- c(6, 5, 1, 5); plotunitregime <- "[mm/month]";
    par(new = FALSE, mar = mar, las = 0)
    ##Data_formating_as_table
    DataModel <- as.data.frame(matrix(as.numeric(NA), nrow = length(IndPeriod_Plot), ncol = 5));
                   DataModel[, 1] <- as.numeric(format(OutputsModel$DatesR[IndPeriod_Plot], format = "%Y%m%d%H"));
    if(BOOL_Pobs){ DataModel[, 2] <- OutputsModel$Precip[IndPeriod_Plot]; }
    if(BOOL_Psol){ DataModel[, 3] <- PsolLayerMean[IndPeriod_Plot]; }
    if(BOOL_Qobs){ DataModel[, 4] <- Qobs[IndPeriod_Plot]; }
    if(BOOL_Qsim){ DataModel[, 5] <- OutputsModel$Qsim[IndPeriod_Plot]; }
    colnames(DataModel) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim");
    TxtDatesDataModel <- formatC(DataModel$Dates, format = "d", width = 8, flag = "0");
    ##Building_of_daily_time_series_if_needed
    if(NameTS == "month"){ DataDaily <- NULL; }
    if(NameTS == "day"  ){ DataDaily <- DataModel; }
    if(NameTS == "hour" ){ DataDaily <- as.data.frame(aggregate(DataModel[, 2:5], by = list(as.numeric(substr(TxtDatesDataModel, 1, 8))), FUN = sum, na.rm = T));  }
    if(NameTS %in% c("hour", "day")){
    colnames(DataDaily) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim"); 
    TxtDatesDataDaily <- formatC(DataDaily$Dates, format = "d", width = 8, flag = "0");  }
    ##Building_of_monthly_time_series_if_needed
    if(NameTS == "month"){ DataMonthly <- DataModel; }
    if(NameTS == "day"  ){ DataMonthly <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily, 1, 6))), FUN = sum, na.rm = T)); }
    if(NameTS == "hour" ){ DataMonthly <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily, 1, 6))), FUN = sum, na.rm = T)); }
    colnames(DataMonthly) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim");
    TxtDatesDataMonthly <- formatC(DataMonthly$Dates, format = "d", width = 6, flag = "0");
    ##Computation_of_interannual_mean_series
    if(!is.null(DataDaily)){
    DataDailyInterAn <- as.data.frame(aggregate(DataDaily[, 2:5], by = list(as.numeric(substr(TxtDatesDataDaily , 5, 8))), FUN = mean, na.rm = T));
    colnames(DataDailyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim"); }
    if(!is.null(DataMonthly)){
    DataMonthlyInterAn <- as.data.frame(aggregate(DataMonthly[, 2:5], by = list(as.numeric(substr(TxtDatesDataMonthly, 5, 6))), FUN = mean, na.rm = T));
    colnames(DataMonthlyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim"); }
    ##Smoothing_of_daily_series_and_scale_conversion_to_make_them_become_a_monthly_regime
    if(!is.null(DataDaily)){
      ##Smoothing
      NDaysWindow <- 30; 
      DataDailyInterAn <- as.data.frame(cbind(DataDailyInterAn$Dates, 
                                        MyRollMean2(DataDailyInterAn$Precip, NDaysWindow), MyRollMean2(DataDailyInterAn$Psol, NDaysWindow), 
                                        MyRollMean2(DataDailyInterAn$Qobs  , NDaysWindow), MyRollMean2(DataDailyInterAn$Qsim, NDaysWindow)));
      colnames(DataDailyInterAn) <- c("Dates", "Precip", "Psol", "Qobs", "Qsim");
      ##Scale_conversion_to_make_them_become_a_monthly_regime
      if(plotunitregime != "[mm/month]"){ stop(paste("incorrect unit for regime plot \n", sep = "")); return(NULL); }
      DataDailyInterAn <- as.data.frame(cbind(DataDailyInterAn[1], DataDailyInterAn[2:5]*30));
    }
    ##Plot_preparation
    DataPlotP <- DataMonthlyInterAn;
    if(!is.null(DataDaily)){
    DataPlotQ <- DataDailyInterAn;
    SeqX1 <- c(  1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 366);
    SeqX2 <- c( 15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350);  
    labX <- "30-days rolling mean";
    } else {
    DataPlotQ <- DataMonthlyInterAn; 
    SeqX1 <- seq(from = 0.5, to = 12.5, by = 1);
    SeqX2 <- seq(from = 1  , to = 12  , by = 1);
    labX <- "";
    }
    xLabels1 <- rep("", 13);
    xLabels2 <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec");
    ylimQ <- range(c(DataPlotQ$Qobs, DataPlotQ$Qsim), na.rm = TRUE);
    if(BOOL_Pobs){ ylimP <- c(max(DataPlotP$Precip, na.rm = TRUE), 0);  }
    txtleg <- NULL; colleg <- NULL; lwdleg <- NULL; lwdP = 10;
    ##Plot_forcings
    if(BOOL_Pobs){
    plot(SeqX2, DataPlotP$Precip, type = "h", xlim = range(SeqX1), ylim = c(3*ylimP[1], ylimP[2]), lwd = lwdP, lend = 1, lty = 1, col = "royalblue", xlab = "", ylab = "", xaxt = "n", yaxt = "n", yaxs = "i", bty = "n", ...)
    txtleg <- c(txtleg, "Ptot" ); colleg <- c(colleg, "royalblue"); lwdleg <- c(lwdleg, lwdP/3); 
    axis(side = 2, at = pretty(0.8*ylimP, n = 3), labels = pretty(0.8*ylimP, n = 3), cex.axis = cexaxis, col.axis = "royalblue", col.ticks = "royalblue");
    par(new = TRUE); }
    if(BOOL_Psol){
    plot(SeqX2, DataPlotP$Psol, type = "h", xlim = range(SeqX1), ylim = c(3*ylimP[1], ylimP[2]), lwd = lwdP, lend = 1, lty = 1, col = "lightblue", xlab = "", ylab = "", xaxt = "n", yaxt = "n", yaxs = "i", bty = "n", ...);
    txtleg <- c(txtleg, "Psol" ); colleg <- c(colleg, "lightblue"); lwdleg <- c(lwdleg, lwdP/3); 
    par(new = TRUE); }
    ##Plot_flows
    plot(NULL, type = "n", xlim = range(SeqX1), ylim = c(ylimQ[1], 2*ylimQ[2]), xlab = "", ylab = "", xaxt = "n", yaxt = "n", ...)
    if(BOOL_Qobs){ lines(1:nrow(DataPlotQ), DataPlotQ$Qobs, lwd = lwdLine, lty = 1, col = "black"    ); txtleg <- c(txtleg, "Qobs" ); colleg <- c(colleg, "black"    ); lwdleg <- c(lwdleg, lwdLine); }
    if(BOOL_Qsim){ lines(1:nrow(DataPlotQ), DataPlotQ$Qsim, lwd = lwdLine, lty = 1, col = "orangered"); txtleg <- c(txtleg, "Qsim"); colleg <- c(colleg, "orangered"); lwdleg <- c(lwdleg, lwdLine); }
    ##Axis_and_legend
    axis(side = 1, at = SeqX1, tick = TRUE , labels = xLabels1, cex.axis = cexaxis)
    axis(side = 1, at = SeqX2, tick = FALSE, labels = xLabels2, cex.axis = cexaxis)
    axis(side = 2, at = pretty(ylimQ), labels = pretty(ylimQ), cex.axis = cexaxis)
    par(las = 0); mtext(side = 1, labX, line = lineX, cex = cexlab); par(las = 0);
    posleg <- "topright"; txtlab <- "flow regime";
    if(BOOL_Pobs){ posleg <- "right"; txtlab <- "precip. & flow regime"; }
    par(las = 0); mtext(side = 2, paste(txtlab, "  ", plotunitregime, sep = ""), line = lineY, cex = cexlab); par(las = 0);
    if(!is.null(BasinArea)){
      Factor <- Factor_MMM_M3S;
      axis(side = 4, at = pretty(ylimQ*Factor)/Factor, labels = pretty(ylimQ*Factor), cex.axis = cexaxis);
      par(las = 0); mtext(side = 4, paste("flow  ", "m3/s", sep = ""), line = lineY, cex = cexlab); par(las = 0); }
    ### posleg <- "topright"; if(BOOL_Pobs){ posleg <- "right"; }
    ### legend(posleg, txtleg, col = colleg, lty = 1, lwd = lwdleg, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
  }


  
  ##Cumulative_frequency
  if(BOOLPLOT_CumFreq){
    kPlot <- kPlot+1; mar <- c(6, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    xlim <- c(0, 1);
    if(BOOL_Qobs & !BOOL_Qsim){ SelectNotZero <- SelectQobsNotZero;
                                ylim <- range(log(Qobs[IndPeriod_Plot][SelectNotZero]), na.rm = TRUE); }
    if(BOOL_Qsim & !BOOL_Qobs){ SelectNotZero <- SelectQsimNotZero;
                                ylim <- range(log(OutputsModel$Qsim[IndPeriod_Plot][SelectNotZero]), na.rm = TRUE); }
    if(BOOL_Qobs &  BOOL_Qsim){ SelectNotZero <- SelectQobsNotZero & SelectQsimNotZero;
                                ylim <- range(log(c(Qobs[IndPeriod_Plot][SelectNotZero], OutputsModel$Qsim[IndPeriod_Plot][SelectNotZero])), na.rm = TRUE); }
    plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", ...);
    ### abline(h = 0, lty = 2, col = grey(0.5));
    ### abline(h = 1, lty = 2, col = grey(0.5));
    axis(side = 1, at = pretty(xlim), labels = pretty(xlim), cex.axis = cexaxis);
    par(las = 0); mtext(side = 1, text = "non-exceedance prob. [-]", line = lineY, cex = cexlab); par(las = 0);
    axis(side = 2, at = seqDATA1, labels = seqDATA2, cex.axis = cexaxis) 
    par(las = 0); mtext(side = 2, text = paste("flow  ", plotunit, "", sep = ""), line = lineY, cex = cexlab); par(las = 0);
    txtleg <- NULL; colleg <- NULL;
    if(BOOL_Qobs){
      DATA2 <- log(Qobs[IndPeriod_Plot][SelectNotZero]);
      SeqQuant <- seq(0, 1, by = 1/(length(DATA2))); Quant <- as.numeric(quantile(DATA2, SeqQuant, na.rm = TRUE));
      Fn <- ecdf(DATA2);  YY <- DATA2; YY <- YY[order( Fn(DATA2) )]; XX <- Fn(DATA2); XX <- XX[order( Fn(DATA2) )];
      lines(XX, YY, lwd = 1, col = "black");
      txtleg <- c(txtleg, "observed"); colleg <- c(colleg, "black"); }
    if(BOOL_Qsim){
      DATA2 <- log(OutputsModel$Qsim[IndPeriod_Plot][SelectNotZero]);
      SeqQuant <- seq(0, 1, by = 1/(length(DATA2))); Quant <- as.numeric(quantile(DATA2, SeqQuant, na.rm = TRUE));
      Fn <- ecdf(DATA2);  YY <- DATA2; YY <- YY[order( Fn(DATA2) )]; XX <- Fn(DATA2); XX <- XX[order( Fn(DATA2) )];
      lines(XX, YY, lwd = 1, col = "orangered");
      txtleg <- c(txtleg, "simulated"); colleg <- c(colleg, "orangered"); }
    if(!is.null(BasinArea)){
      Factor <- Factor_UNIT_M3S;
      axis(side = 4, at = seqDATA1, labels = round(seqDATA2*Factor), cex.axis = cexaxis)
      par(las = 0); mtext(side = 4, paste("flow  ", "m3/s", sep = ""), line = lineY, cex = cexlab); par(las = 0); }
    legend("topleft", txtleg, col = colleg, lty = 1, lwd = lwdLine, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
  }


  ##Correlation_QQ
  if(BOOLPLOT_CorQQ){
    kPlot <- kPlot+1; mar <- c(6, 5, 1, 5);
    par(new = FALSE, mar = mar, las = 0)
    ylim <- log(range(c(Qobs[IndPeriod_Plot][SelectQobsNotZero & SelectQsimNotZero], OutputsModel$Qsim[IndPeriod_Plot][SelectQobsNotZero & SelectQsimNotZero]), na.rm = TRUE));
    plot(log(Qobs[IndPeriod_Plot][SelectQobsNotZero & SelectQsimNotZero]), log(OutputsModel$Qsim[IndPeriod_Plot][SelectQobsNotZero & SelectQsimNotZero]), type = "p", pch = 1, cex = 0.9, col = "black", xlim = ylim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
    abline(a = 0, b = 1, col = "royalblue");
    axis(side = 1, at = seqDATA1, labels = seqDATA2, cex = cexaxis);
    axis(side = 2, at = seqDATA1, labels = seqDATA2, cex = cexaxis);
    par(las = 0); mtext(side = 1, paste("observed flow  ", plotunit, "", sep = ""), line = lineX, cex = cexlab); par(las = 0);
    par(las = 0); mtext(side = 2, paste("simulated flow  ", plotunit, "", sep = ""), line = lineY, cex = cexlab); par(las = 0);
    if(!is.null(BasinArea)){
      Factor <- Factor_UNIT_M3S;
      axis(side = 4, at = seqDATA1, labels = round(seqDATA2*Factor), cex.axis = cexaxis);
      par(las = 0); mtext(side = 4, paste("flow  ", "m3/s", sep = ""), line = lineY, cex = cexlab); par(las = 0); }
    legend("bottomright", "log scale", lty = 1, col = NA, bty = "o", bg = bgleg, box.col = bgleg, cex = cexleg)
    box()
  }

  ##Empty_plots
  while(kPlot < iPlotMax){
    kPlot <- kPlot+1;
    par(new = FALSE)
    plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, ...)
  }

  ##Restoring_layout_options
  layout(1);
  

}
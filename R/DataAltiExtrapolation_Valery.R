DataAltiExtrapolation_Valery <- function(DatesR,
                                         Precip,  PrecipScale = TRUE,
                                         TempMean, TempMin = NULL, TempMax = NULL,
                                         ZInputs,  HypsoData, NLayers,
                                         verbose = TRUE) {

  ##Altitudinal_gradient_functions_______________________________________________________________
  ##unique_gradient_for_precipitation
  GradP_Valery2010 <- 0.00041  ### value from Valery PhD thesis page 126



  ##Format_______________________________________________________________________________________
  HypsoData <- as.double(HypsoData)
  ZInputs   <- as.double(ZInputs)



  ##ElevationLayers_Creation_____________________________________________________________________
  ZLayers   <- as.double(rep(NA, NLayers))

  if (!identical(HypsoData, as.double(rep(NA, 101)))) {
    nmoy   <- 100 %/% NLayers
    nreste <- 100 %% NLayers
    ncont  <- 0

    for (iLayer in 1:NLayers) {
      if (nreste > 0) {
        nn <- nmoy + 1
        nreste <- nreste - 1
      } else {
        nn <- nmoy
      }
      if (nn == 1) {
        ZLayers[iLayer] <- HypsoData[ncont + 1]
      }
      if (nn == 2) {
        ZLayers[iLayer] <- 0.5 * (HypsoData[ncont + 1] + HypsoData[ncont + 2])
      }
      if (nn > 2) {
        ZLayers[iLayer] <- HypsoData[ncont + nn / 2 + 1]
      }
      ncont <- ncont + nn
    }
  }


  ##Precipitation_extrapolation__________________________________________________________________
  ##Initialisation
  if (identical(ZInputs, HypsoData[51]) & NLayers == 1) {
    LayerPrecip <- list(as.double(Precip))
  } else {
    ##Elevation_gradients_for_daily_mean_precipitation
    GradP    <- GradP_Valery2010 ### single value
    TabGradP <- rep(GradP, length(Precip))
    ##Extrapolation
    ##Thresold_of_inputs_median_elevation
    Zthreshold <- 4000
    LayerPrecip_mat <- sapply(1:NLayers, function(iLayer) {
      ##If_layer_elevation_smaller_than_Zthreshold
      if (ZLayers[iLayer] <= Zthreshold) {
        prcp <- as.double(Precip * exp(TabGradP * (ZLayers[iLayer] - ZInputs)))
        ##If_layer_elevation_greater_than_Zthreshold
      } else {
        ##If_inputs_median_elevation_smaller_than_Zthreshold
        if (ZInputs <= Zthreshold) {
          prcp <- as.double(Precip * exp(TabGradP * (Zthreshold - ZInputs)))
          ##If_inputs_median_elevation_greater_then_Zthreshold
        } else {
          prcp <- as.double(Precip)
        }
      }
      return(prcp)
    })
    if (PrecipScale) {
      LayerPrecip_mat <- LayerPrecip_mat / rowMeans(LayerPrecip_mat) * Precip
      LayerPrecip_mat[is.nan(LayerPrecip_mat)] <- 0
    }
    LayerPrecip <- as.list(as.data.frame(LayerPrecip_mat))
  }



  ##Temperature_extrapolation____________________________________________________________________
  ##Initialisation
  LayerTempMean <- list()
  LayerTempMin <- list()
  LayerTempMax <- list()

  if (identical(ZInputs, HypsoData[51]) & NLayers == 1) {
    LayerTempMean[[1]] <- as.double(TempMean)

    if (!is.null(TempMin) & !is.null(TempMax)) {
      LayerTempMin[[1]] <- as.double(TempMin)
      LayerTempMax[[1]] <- as.double(TempMax)
    }
  } else {
    ##Elevation_gradients_for_daily_mean_min_and_max_temperature
    GradT <- .GradT_Valery2010
    iday <- match(format(DatesR, format = "%d%m"),
                  sprintf("%02i%02i", GradT[, "day"], GradT[, "month"]))
    TabGradT <- GradT[iday, c("grad_Tmean", "grad_Tmin", "grad_Tmax")]
    ##Extrapolation
    ##On_each_elevation_layer...
    for (iLayer in 1:NLayers) {
      LayerTempMean[[iLayer]] <- as.double(TempMean + (ZInputs - ZLayers[iLayer]) * abs(TabGradT[, "grad_Tmean"]) /  100)
      if (!is.null(TempMin) & !is.null(TempMax)) {
        LayerTempMin[[iLayer]]  <- as.double(TempMin  + (ZInputs - ZLayers[iLayer]) * abs(TabGradT[, "grad_Tmin"]) /  100)
        LayerTempMax[[iLayer]]  <- as.double(TempMax  + (ZInputs - ZLayers[iLayer]) * abs(TabGradT[, "grad_Tmax"]) /  100)
      }
    }
  }



  ##Solid_Fraction_for_each_elevation_layer______________________________________________________
  LayerFracSolidPrecip <- list()

  ##Thresold_of_inputs_median_elevation
  Zthreshold <- 1500

  ##Option
  Option <- "USACE"
  if (!is.na(ZInputs)) {
    if (ZInputs < Zthreshold & !is.null(TempMin) & !is.null(TempMax)) {
      Option <- "Hydrotel"
    }
  }

  ##On_each_elevation_layer...
  for (iLayer in 1:NLayers) {

    ##Turcotte_formula_from_Hydrotel
    if (Option == "Hydrotel") {
      TempMin <- LayerTempMin[[iLayer]]
      TempMax <- LayerTempMax[[iLayer]]
      SolidFraction <- 1 - TempMax / (TempMax - TempMin)
      SolidFraction[TempMin >= 0] <- 0
      SolidFraction[TempMax <= 0] <- 1
    }
    ##USACE_formula
    if (Option == "USACE") {
      USACE_Tmin <- -1.0
      USACE_Tmax <- 3.0
      TempMean <- LayerTempMean[[iLayer]]
      SolidFraction <- 1 - (TempMean - USACE_Tmin) / (USACE_Tmax - USACE_Tmin)
      SolidFraction[TempMean > USACE_Tmax] <- 0
      SolidFraction[TempMean < USACE_Tmin] <- 1
    }
    LayerFracSolidPrecip[[iLayer]] <- as.double(SolidFraction)
  }
  namesLayer <- sprintf("L%i", seq_along(LayerPrecip))
  names(LayerPrecip)   <- namesLayer
  names(LayerTempMean) <- namesLayer
  if (!is.null(TempMin) & !is.null(TempMax)) {
    names(LayerTempMin) <- namesLayer
    names(LayerTempMax) <- namesLayer
  }
  names(LayerFracSolidPrecip) <- namesLayer



  ##END__________________________________________________________________________________________
  return(list(LayerPrecip          = LayerPrecip,
              LayerTempMean        = LayerTempMean,
              LayerTempMin         = LayerTempMin,
              LayerTempMax         = LayerTempMax,
              LayerFracSolidPrecip = LayerFracSolidPrecip,
              ZLayers              = ZLayers))


}


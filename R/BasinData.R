#' @name BasinInfo
#' @docType data
#' @title Data sample: characteristics of a fictional catchment (L0123001, L0123002 or L0123003)
#' @description
#' R-object containing the code, station's name, area and hypsometric curve of the catchment.
#' @encoding UTF-8
#' @format
#' List named 'BasinInfo' containing
#' \itemize{
#' \item two strings: catchment's code and station's name
#' \item one float: catchment's area in km2
#' \item one numeric vector: catchment's hypsometric curve (min, quantiles 01 to 99 and max) in metres
#' }
#' @examples 
#'    require(airGR)
#'    data(L0123001)
#'    str(BasinInfo)

NULL


#' @name BasinObs
#' @docType data
#' @title Data sample: time series of observations of a fictional catchment (L0123001, L0123002 or L0123003)
#' @description
#' R-object containing the times series of precipitation, temperature, potential evapotranspiration and discharges. \cr
#' Times series for L0123001 or L0123002 are at the daily time-step for use with daily models such as GR4J, GR5J, GR6J, CemaNeigeGR4J, CemaNeigeGR5J and CemaNeigeGR6J.
#' Times series for L0123003 are at the hourly time-step for use with hourly models such as GR4H.
#' @encoding UTF-8
#' @format
#' Data frame named 'BasinObs' containing
#' \itemize{
#' \item one POSIXlt vector: time series dates in the POSIXlt format
#' \item five numeric vectors: time series of catchment average precipitation [mm], catchment average air temperature [degC], catchment average potential evapotranspiration [mm], outlet discharge [l/s], outlet discharge [mm]
#' }
#' @examples 
#'    require(airGR)
#'    data(L0123001)
#'    str(BasinObs)

NULL


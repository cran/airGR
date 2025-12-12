SeriesAggreg.InputsModel <- function(x, Format, ...) {
  isCN <- "LayerSolidPrecip" %in% names(x)
  if (isCN) {
    x$LayerSolidPrecip <- as.data.frame(x$LayerPrecip) *
      as.data.frame(x$LayerFracSolidPrecip)
    x$LayerSolidPrecip <- as.list(x$LayerSolidPrecip)
  }
  x <- SeriesAggreg.list(
    x,
    Format,
    ConvertFun = NA,
    except = c("ZLayers", "LengthHydro", "BasinAreas"),
    ...
  )
  if (isCN) {
    x$LayerFracSolidPrecip <- as.data.frame(x$LayerSolidPrecip) /
      as.data.frame(x$LayerPrecip)
    x$LayerFracSolidPrecip <- as.list(x$LayerFracSolidPrecip)
    x$LayerSolidPrecip <- NULL
  }
  x
}

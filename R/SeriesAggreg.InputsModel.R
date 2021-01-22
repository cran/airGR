SeriesAggreg.InputsModel <- function(x, Format, ...) {
  SeriesAggreg.list(x,
                    Format,
                    ConvertFun = NA,
                    except = c("ZLayers", "LengthHydro", "BasinAreas"),
                    ...)
}

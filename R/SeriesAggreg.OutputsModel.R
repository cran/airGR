SeriesAggreg.OutputsModel <- function(x, Format, ...) {
  SeriesAggreg.list(x,
                    Format,
                    ConvertFun = NA,
                    except = c("RunOptions", "StateEnd"),
                    ...)
}

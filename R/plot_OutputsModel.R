plot_OutputsModel <- function(OutputsModel, PlotChoice = "all", ...) {
  .Deprecated(new = "plot_OutputsModel", package = NULL,
              msg = "Deprecated function. Please, use plot.OutputsModel() or plot() on an object of class OutputsModel.",
              old = as.character(sys.call(sys.parent()))[1L])
  if (!missing(OutputsModel)) {
    warning("Deprecated \"OutputsModel\" argument. Please, use \"x\" instead.")
  }
  if (!missing(PlotChoice)) {
    warning("Deprecated \"PlotChoice\" argument. Please, use \"which\" instead.")
  }  
  plot.OutputsModel(x = OutputsModel, which = PlotChoice, ...)
}

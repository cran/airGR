TransfoParam <- function(ParamIn, Direction, FUN_TRANSFO) {
  FUN_TRANSFO <- match.fun(FUN_TRANSFO)
  return(FUN_TRANSFO(ParamIn, Direction))
}

#' Script for running scheduled test
#'
#' All files with the pattern /testthat/tests/scheduled-*.R are tested
#' as testthat does for files /testthat/tests/test-*.R.
#'
#' This script should be started with `source` command from the root of the package.
#' @example
#' source("tests/scheduled.R")

####################
# Helper functions #
####################

#' Wrapper for [quit] which is only applied outside of RStudio
#'
#' @param status See `status` parameter of [quit]. Default `quit = 1`.
#' @param ... Other parameters sent to [quit]
#'
#' @return NULL
#' @export
quit2 <- function(status = 1, ...) {
  if (all(!grepl("rstudio", Sys.getenv(), ignore.case = TRUE))) {
    quit(status, ...)
  }
}

###############
# MAIN SCRIPT #
###############

library(testthat)
library(airGR)

scheduled_tests <- list.files(
  path = "tests/testthat",
  pattern = "^scheduled-.*\\.R$",
  full.names = TRUE
)

lRes <- lapply(scheduled_tests, test_file)
for (res in lRes) {
  dRes <- as.data.frame(res)
  if (any(dRes[, "failed"] > 0) | any(dRes[, "error"])) {
    quit2()
  }
}

# Helper functions for regression

StoreStableExampleResults <- function(
  package = "airGR",
  path = file.path("tests/tmp", Sys.getenv("R_VERSION"), "stable"),
  ...) {
  install.packages(package, repos = "http://cran.r-project.org")
  StoreExampleResults(package = package, path = path, ...)
}

StoreDevExampleResults <- function(
  package = "airGR",
  path = file.path("tests/tmp", Sys.getenv("R_VERSION"), "dev"),
  ...) {
  StoreExampleResults(package = package, path = path, ...)
}

#' Run examples of a package and store the output variables in RDS files for further testing.
#'
#' @param package Name of the package from which examples are tested.
#' @param path Path where to record the files.
#' @param run.dontrun See \code{\link{example}}.
#' @param run.donttest See \code{\link{example}}.
#'
#' @return
#' @export
#'
#' @examples
StoreExampleResults <- function(package, path, run.dontrun = FALSE, run.donttest = TRUE) {

  # Install and load stable version of the package
  library(package, character.only = TRUE)

  # Get the list of documentation pages
  rd <- unique(readRDS(system.file("help", "aliases.rds", package = package)))

  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)

  lapply(
    rd,
    StoreTopicResults,
    package, path, run.dontrun = run.dontrun, run.donttest = run.donttest
  )

}

StoreTopicResults <- function(topic, package, path, run.dontrun = TRUE, run.donttest = TRUE) {

  cat("*******************************\n")
  cat("*", topic, "\n")
  cat("*******************************\n")

  par(ask = FALSE) #https://stackoverflow.com/questions/34756905/how-to-turn-off-the-hit-return-to-see-next-plot-prompt-plot3d

  varBefore <- c()
  varBefore <- ls(envir = globalenv())

  start_time = Sys.time()

  example(
    topic, package = package, character.only = TRUE, echo = FALSE, ask = FALSE, local = FALSE, setRNG = TRUE,
    run.dontrun = run.dontrun, run.donttest = run.donttest
  )

  end_time = Sys.time()
  dev.off()

  write.table(data.frame(topic = topic, time = end_time - start_time),
            file.path(path, "timing.tsv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE,
            sep = "\t", append = TRUE)

  varAfter <- ls(envir = globalenv())

  varToSave <- setdiff(varAfter, varBefore)

  if (length(varToSave) > 0) {
    path <- file.path(path, topic)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    lapply(varToSave, function(x) {
      saveRDS(get(x), file = file.path(path, paste0(x, ".rds")))
    })
  }

  rm(list = varToSave, envir = globalenv())

}

CompareStableDev <- function() {
  res <- testthat::test_file("tests/testthat/regression.R")
  dRes <- as.data.frame(res)
  if (any(dRes[, "failed"] > 0) | any(dRes[, "error"])) {
    quit(status = 1)
  }
}

###############
# MAIN SCRIPT #
###############

# Execute Regression test by comparing RD files stored in folders /tests/tmp/ref and /tests/tmp/test
Args <- commandArgs(trailingOnly = TRUE)

lActions <- list(
  stable = StoreStableExampleResults,
  dev = StoreDevExampleResults,
  compare = CompareStableDev
)

if (length(Args) == 1 && Args %in% names(lActions)) {
  lActions[[Args]]()
} else {
  stop("This script should be run with one argument in the command line:\n",
       "`Rscript tests/regression_tests.R [stable|dev|compare]`.\n",
       "Available arguments are:\n",
       "- stable: install stable version from CRAN, run and store examples\n",
       "- dev: install dev version from current directory, run and store examples\n",
       "- compare: stored results of both versions")
}

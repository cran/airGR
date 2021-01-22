# Execute Regression test by comparing RD files stored in folders /tests/tmp/ref and /tests/tmp/test
Args = commandArgs(trailingOnly=TRUE)

source("tests/testthat/helper_regression.R")

lActions = list(
  stable = StoreStableExampleResults,
  dev = StoreDevExampleResults,
  compare = CompareStableDev
)

if(Args %in% names(lActions)) {
  lActions[[Args]]()
} else {
  stop("This script should be run with one argument in the command line:\n",
       "`Rscript tests/regression_tests.R [stable|dev|compare]`.\n",
       "Available arguments are:\n",
       "- stable: install stable version from CRAN, run and store examples\n", 
       "- dev: install dev version from current directory, run and store examples\n",
       "- compare: stored results of both versions")
}

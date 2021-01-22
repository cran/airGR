context("Compare example outputs with CRAN")

CompareWithStable <- function(refVarFile, testDir, regIgnore) {
  v <- data.frame(topic = basename(dirname(refVarFile)),
                  var = gsub("\\.rds$", "", basename(refVarFile)))
  if (is.null(regIgnore) || all(apply(regIgnore, 1, function(x) !all(x == v)))) {
    test_that(paste("Compare", v$topic, v$var), {
      testVarFile <- paste0(
        file.path(testDir, v$topic, v$var),
        ".rds"
      )
      expect_true(file.exists(testVarFile))
      if (file.exists(testVarFile)) {
        testVar <- readRDS(testVarFile)
        refVar <- readRDS(refVarFile)
        expect_equivalent(testVar, refVar)
      }
    })
  }
}

tmp_path <- file.path("../tmp", Sys.getenv("R_VERSION"));

if (dir.exists(file.path(tmp_path, "stable")) & dir.exists(file.path(tmp_path, "dev"))) {
  refVarFiles <- list.files(file.path(tmp_path, "stable"), recursive = TRUE, full.names = TRUE)
  regIgnoreFile <- "../../.regressionignore"
  if (file.exists(regIgnoreFile)) {
    message("Using .regressionignore file. The following variables are going to be skipped:")
    regIgnore <- read.table(file = regIgnoreFile,
                            sep = " ", header = FALSE, skip = 5,
                            col.names = c("topic", "var"),
                            stringsAsFactors = FALSE)
    apply(regIgnore, 1, function(x) message(x[1], ": ", x[2]))
  } else {
    message("File ", file.path(getwd(), regIgnoreFile), " not found")
    regIgnore <- NULL
  }
  lapply(X = refVarFiles, CompareWithStable, testDir = file.path(tmp_path, "dev"), regIgnore = regIgnore)
} else {
  stop("Regression tests compared to released version needs that you run the following instructions first:\n",
       "Rscript tests/testthat/regression_tests.R stable\n",
       "R CMD INSTALL .\n",
       "Rscript tests/testthat/regression_tests.R dev")
}

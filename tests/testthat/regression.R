context("Compare example outputs with CRAN")

CompareWithStable <- function(refVarFile, testDir, regIgnore) {
  v <- list(topic = basename(dirname(refVarFile)),
                  var = gsub("\\.rds$", "", basename(refVarFile)))
  if (is.null(regIgnore) || !any(apply(regIgnore, 1, function(x) {v$var == x[2] && (x[1] == "*" || x[1] == v$topic)}))) {
    test_that(paste("Compare", v$topic, v$var), {
      testVarFile <- paste0(
        file.path(testDir, v$topic, v$var),
        ".rds"
      )
      expect_true(file.exists(testVarFile))
      if (file.exists(testVarFile)) {
        testVar <- readRDS(testVarFile)
        refVar <- readRDS(refVarFile)
        if (!is.null(regIgnore)) {
          regIgnore$mainVar <- gsub("\\$.*$", "", regIgnore[,2])
          varIgnore <- regIgnore[apply(regIgnore, 1, function(x) {v$var == x[3] && (x[1] == "*" || x[1] == v$topic)}), 2]
          if (length(varIgnore) > 0) {
            itemIgnore <- gsub("^.*\\$", "", varIgnore)
            for(item in itemIgnore) {
              testVar[[item]] <- NULL
              refVar[[item]] <- NULL
            }
          }
        }
        expect_equivalent(testVar, refVar)
      }
    })
  }
}

tmp_path <- file.path("../tmp", Sys.getenv("R_VERSION"));

if (dir.exists(file.path(tmp_path, "stable")) & dir.exists(file.path(tmp_path, "dev"))) {
  refVarFiles <- list.files(path = file.path(tmp_path, "stable"),
                            pattern = "\\.rds$",
                            recursive = TRUE, full.names = TRUE)
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
  lapply(refVarFiles, FUN = CompareWithStable, testDir = file.path(tmp_path, "dev"), regIgnore = regIgnore)
} else {
  stop("Regression tests compared to released version needs that you run the following instructions first:\n",
       "Rscript tests/testthat/regression_tests.R stable\n",
       "R CMD INSTALL .\n",
       "Rscript tests/testthat/regression_tests.R dev")
}

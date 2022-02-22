library(airGR)
source("tests/testthat/helper_scheduled_Calibration.R")


BenchmarkRunModel <- function(model) {

  e <- PrepareCalibration(model)
  for (n in ls(e, all.names = TRUE)) {
    assign(n, value = get(n, e))
  }

  # RunOptions calibration configuration
  RunOptions$Outputs_Sim <- RunOptions$Outputs_Cal
  mbm <- microbenchmark::microbenchmark(
    RunModel = RunModel(InputsModel = InputsModel, RunOptions = RunOptions,
                        Param = as.numeric(strsplit(model$ParamFinalR, ";")[[1]]), FUN_MOD = sModel)
  )
  return(mbm$time)

}


sModelNames <- paste0(dfModels$name,
                      ifelse(as.logical(dfModels$IsHyst), "_Hyst", ""))

dfBM <- as.data.frame(apply(dfModels, MARGIN = 1, FUN = BenchmarkRunModel))
colnames(dfBM) <- sModelNames
dfBM <- cbind(version = as.character(packageVersion('airGR')), dfBM)


file <- "tests/tmp/benchmark.tsv"
dir.create("tests/tmp", showWarnings = FALSE)
write.table(dfBM, file = file,
            row.names = FALSE, col.names = !file.exists(file), quote = FALSE,
            sep = "\t", append = file.exists(file))

df <- read.table(file = file, sep = "\t", header = TRUE)

if (length(unique(df$version)) > 1) {
  lV <- lapply(unique(df$version), function(version) {
    apply(df[df$version == version, -1] / 1e6, MARGIN = 2, FUN = mean)
  })
  names(lV) <- unique(df$version)
  dfMean <- cbind(model = sModelNames, as.data.frame(t(do.call(rbind, lV))))
  dfMean$evolution <- (dfMean[, 3] - dfMean[, 2]) / dfMean[, 2]
  write.table(dfMean, "tests/tmp/mean_execution_time.tsv", row.names = FALSE, quote = FALSE, sep = "\t")
  res <- testthat::test_file("tests/testthat/benchmarkRunModel.R")
  dRes <- as.data.frame(res)
  if (any(dRes[, "failed"] > 0) | any(dRes[, "error"])) {
    quit(status = 1)
  }
}



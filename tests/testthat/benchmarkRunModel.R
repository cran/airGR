df <- read.table("../tmp/mean_execution_time.tsv", sep = "\t", header = T)

lapply(df$model, function(model) {
  test_that(paste(model, ": RunModel should be as fast as CRAN version"), {
    sel <- df$model == model
    # Limit threshold for evolution of execution time (in %) between the 2 versions
    # Negative values of evolution are expected but we apply an error margin depending on execution time
    # decrease from 1.5 at 0.0ms to 0.5 at 10ms with a minimum at 0.5
    threshold <- max(0.5, 2 - 0.1 * df[sel, 2])
    expect_lt(df$evolution[sel], threshold)
  })
})


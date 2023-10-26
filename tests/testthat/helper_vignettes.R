#' Extract chunks from Rmd files (knitr::purl) and source them
#'
#' @param fileRmd Rmd file to
#' @param tmpFolder Folder storing the script containing extracted chunks
#' @param force.eval Force execution of chunks with parameter eval=FALSE
RunRmdChunks <- function(fileRmd,
                         tmpFolder = "../tmp",
                         force.eval = TRUE,
                         chunkIgnore = getChunkIgnore()) {
  dir.create(tmpFolder, showWarnings = FALSE)
  output <- file.path(tmpFolder,
                      gsub("\\.Rmd", "\\.R", basename(fileRmd), ignore.case = TRUE))
  knitr::purl(fileRmd, output = output, quiet = TRUE)
  sTxt <- readLines(output)
  if (force.eval) {
    sectionLines <- grep("^## ----", sTxt)
    chunkIgnore <- chunkIgnore[[basename(fileRmd)]]
    if (!is.null(chunkIgnore)) {
      regexChunk <- sprintf("(?!(%s))", paste(chunkIgnore, collapse = "|"))
    } else {
      regexChunk <- ""
    }
    chunksEvalStart <- grep(paste0("^## ----", regexChunk, ".*eval=F"), sTxt, ignore.case=TRUE, perl = TRUE)
    if (length(chunksEvalStart) > 0) {
      if (sectionLines[length(sectionLines)] == chunksEvalStart[length(chunksEvalStart)]) {
        lastEvalStart <- length(chunksEvalStart) - 1
      } else {
        lastEvalStart <- length(chunksEvalStart)
      }
      # Search for end lines of eval=F chunks
      chunksEvalEnd <- sectionLines[sapply(chunksEvalStart[1:lastEvalStart], function(x) {which(sectionLines == x)}) + 1] - 1
      if (lastEvalStart) {
        # Add last line if last chunk is eval=FALSE
        chunksEvalEnd <- c(chunksEvalEnd, length(sTxt))
      }
      chunksEvalStart <- chunksEvalStart + 1 # Chunks begin one line after the section comment
      for (i in 1:length(chunksEvalStart)) {
        # Remove comments on eval=F chunk lines
        sTxt[chunksEvalStart[i]:chunksEvalEnd[i]] <- gsub(pattern = "^## ",
                                                          replace = "",
                                                          x = sTxt[chunksEvalStart[i]:chunksEvalEnd[i]])
      }
    }

  }
  # Remove line of code displaying data
  removeFromGrep <- function(pattern, x) {
    i <- grep(pattern, x)
    if (length(i) > 0) {
      x <- x[-i]
    }
    return(x)
  }
  sTxt <- removeFromGrep("^summary\\(.*\\)$", sTxt)
  sTxt <- removeFromGrep("^str\\(.*\\)$", sTxt)
  # Switch echo off for some functions
  sTxt <- gsub("trace\\s?=\\s?[0-9]+", "trace = 0", sTxt)
  # Add parameters to example calls
  exLines <- grep("^example\\(.*\\)", sTxt)
  sTxt[exLines] <- paste0(substr(sTxt[exLines], 1, nchar(sTxt[exLines]) - 1), ", echo = FALSE, verbose = FALSE, ask = FALSE)")
  # Remove question "Hit <Return> to see next plot"
  sTxt <- c("par(ask=F)", sTxt)
  # Write the transformed script
  writeLines(sTxt, output)
  # Silently run the chunks
  invisible(capture.output(suppressMessages(suppressWarnings(source(output))), type = "output"))
  return(TRUE)
}

#' Extract chunks from vignette and source them
#'
#' @param vignette Name of the vignette
#' @param tmpFolder Folder storing the script containing extracted chunks
#' @param force.eval Force execution of chunks with parameter eval=FALSE
#'
#' @return TRUE if succeed.
RunVignetteChunks <- function(vignette,
                              tmpFolder = "../tmp",
                              force.eval = TRUE) {
  if (file.exists(sprintf("../../vignettes/%s.Rmd", vignette))) {
    # testthat context in development environnement
    RunRmdChunks(sprintf("../../vignettes/%s.Rmd", vignette),
                 tmpFolder = tmpFolder,
                 force.eval =force.eval,
                 chunkIgnore = getChunkIgnore("../../.vignettechunkignore"))
  } else if (file.exists(sprintf("vignettes/%s.Rmd", vignette))) {
    # context in direct run in development environnement
    RunRmdChunks(sprintf("vignettes/%s.Rmd", vignette),
                 tmpFolder = tmpFolder,
                 force.eval =force.eval,
                 chunkIgnore = getChunkIgnore(".vignettechunkignore"))
  } else {
    # R CMD check context in package environnement
    RunRmdChunks(system.file(sprintf("doc/%s.Rmd", vignette), package = "airGR"),
                 tmpFolder = tmpFolder,
                 force.eval =force.eval,
                 chunkIgnore = getChunkIgnore(".vignettechunkignore"))
  }
  return(TRUE)
}

#' Test if conversion from Q in mm per day into Q in L/s is good in BasinObs
#'
#' @param BasinObs A dataframe containing columns Qmm and Qls
#' @param BasinArea Area of the basin in km2
#' @param tolerance See ?all.equal
#'
#' @return
TestQmmQlsConversion <- function(BasinObs, BasinArea, tolerance = 1E-7) {
  Conversion <- BasinArea * 1000^2 / 1000 * 1000 # km2 -> m2, mm -> m and m3 -> L
  Conversion <- Conversion / 86400 # Day -> seconds
  notNA <- which(!is.na(BasinObs$Qmm))
  expect_equal(BasinObs$Qmm[notNA] * Conversion, BasinObs$Qls[notNA], tolerance = tolerance)
}

#' Read vignettechunkignore file
#'
#' @param chunkIgnoreFile path to the file
#'
#' @return [list] with one item by vignette containing the chunk id to ignore
#'
getChunkIgnore <- function(chunkIgnoreFile = "../../.vignettechunkignore") {
  if (file.exists(chunkIgnoreFile)) {
    message(".vignettechunkignore file found")
    chunkIgnore <- read.table(file = chunkIgnoreFile,
                              sep = " ", header = FALSE,
                              col.names = c("vignette", "chunk"),
                              stringsAsFactors = FALSE)
    chunkIgnore <- lapply(setNames(nm = unique(chunkIgnore$vignette)), function(x) {
      chunkIgnore$chunk[chunkIgnore$vignette == x]
    })
  } else {
    message("No .vignettechunkignore file found")
    chunkIgnore <- list()
  }
  return(chunkIgnore)
}

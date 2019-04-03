
## =================================================================================
## function to check
## =================================================================================

# .onLoad <- function(libname, pkgname){
#   if (requireNamespace("airGRteaching", quietly = TRUE)) {
#     if (packageVersion("airGRteaching") %in% package_version(c("0.2.0.9", "0.2.2.2", "0.2.3.2"))) {
#       packageStartupMessage("In order to be compatible with the present version of 'airGR', please update your version of the 'airGRteaching' package.")
#     }
#   }
# }


## =================================================================================
## function to manage Fortran outputs
## =================================================================================

.FortranOutputs <- function(GR = NULL, isCN = FALSE) {
  
  outGR <- NULL
  outCN <- NULL
  
  if (is.null(GR)) {
    GR <- ""
  }
  if (GR == "GR1A") {
    outGR <- c("PotEvap", "Precip",
               "Qsim")
  } else if (GR == "GR2M") {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn",
               "AE",
               "Perc", "PR",
               "Rout", "Exch",
               "Qsim")
  } else if (GR == "GR4H") {
    outGR <- c("PotEvap", "Precip", "Prod",
               "AE",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch",
               "AExch", "QR",
               "QD",
               "Qsim")
  } else if (GR %in% c("GR4J", "GR5J")) {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn", "Ps",
               "AE",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch", 
               "AExch1", "AExch2",
               "AExch", "QR",
               "QD",
               "Qsim")
  } else if (GR == "GR6J") {
    outGR <- c("PotEvap", "Precip", "Prod", "Pn", "Ps",
               "AE",
               "Perc", "PR",
               "Q9", "Q1",
               "Rout", "Exch",
               "AExch1", "AExch2",
               "AExch", "QR",
               "QRExp", "Exp",
               "QD",
               "Qsim")
  }
  if (isCN) {
    outCN <- c("Pliq", "Psol", 
               "SnowPack", "ThermalState", "Gratio", 
               "PotMelt", "Melt", "PliqAndMelt", "Temp", 
               "Gthreshold", "Glocalmax")
  }
  
  res <- list(GR = outGR, CN = outCN)
  
}











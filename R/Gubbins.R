
#' Refresh package with latest changes
#'
#' Re-document and reinstall a local package
#' @param package String: name of package to be re-documented and reinstalled.
#' @export
#' @examples
#' update.functions("IO.Pixels")
#' 
#' 
update.functions <- function(package = "IO.pixels") {
    require(devtools)
    require(roxygen2)
    
    package <- toString(package)
    
    org.dir <- getwd()
    
    target.dir <- paste0("/home/clair/R/My-packages/", package, "/")
    if (org.dir != target.dir) {setwd(target.dir)}
    
    document()
    setwd("..")
    install(package)
    setwd(org.dir)
}


#' Crop a pdf
#' 
#' Call 'pdfcrop' function without having to switch to terminal
#' @param filenm String giving filename to be cropped (including full path and extension)
#' @export
#' 
crop.pdf <- function(filenm) {
    system2('pdfcrop', c(filenm, filenm))
}


#' Install packages
#' 
#' Download & install packages after upgrading R
#' @export
#' 
install.useful.packages <- function() {
    
    useful.packages <- list("tiff", "spatstat", "polyclip", "XML", "plyr", "raster", 
                     "reshape", "MASS", "SuppDists", "igraph", "mmand", "knitr", "abind")
    lapply(useful.packages, require)
}


#' Replace 0 with "-"
#' 
#' Prepare an object (usually a table or data frame) for output to csv for LaTeX, by rounding to set DP and replacing 0 with "-".
#' @param qq Object to be prepared
#' @param dp Decimal places desired
#' @return Original object, with 0 (and, incidentally, NA) replaced with "-"
#' @export
#' 
prep.csv <- function(qq, dp = 1) {
    qq[qq == 0] <- NA
    qq <- round(qq, dp)
    qq[is.na(qq)] <- "-"
    qq
}
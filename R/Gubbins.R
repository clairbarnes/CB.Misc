
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


#' convert integer date to string, with dashes
#'
#' @param dt Integer date, in format yymmdd
#' @return String date, yy-mm-dd
#' @export
#' 
fancy.date <- function(dt) {
    dt <- toString(dt)
    paste0(substring(dt, 1, 2), "-", substring(dt, 3, 4), "-", substring(dt, 5, 6))
}


#' Merge list of data frames
#' 
#' Recursively merge list of data frames with a 'master data frame'
#' @param head Data frame with which all others are to be merge (for example, a list of coordinates, to which classifications can be added)
#' @param df.list List of additional data frames to be merged with the head
#' @param ... Additional optional parameters to be passed to the \link{\code{merge}} function.
#' @return Data frame containing merged data
#' @export
#' 
merge.df.list <- function(head, df.list, ...) {
    
    # less efficient than lapply, but easier for renaming 
    for (l in 1:length(df.list)) {
        head <- merge(head, df.list[[l]], suffix = c("", paste0(".", names(df.list)[l])), ...)
    }
    return(head)
}
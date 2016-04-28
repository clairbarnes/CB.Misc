
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
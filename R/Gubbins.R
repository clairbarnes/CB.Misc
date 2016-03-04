
#' Refreshes package with latest changes
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
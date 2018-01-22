
#' Quickly refresh & reinstall packages
#' @export
#'
refresh <- function(pkg) {
    require(devtools)
    require(roxygen2)
    org.dir <- getwd()
    target.dir <- paste0("/home/clair/R/My-packages/", pkg)
    
    if (org.dir != target.dir) {setwd(target.dir)}
    document()
    setwd("..")
    install(pkg)
    setwd(org.dir)
}



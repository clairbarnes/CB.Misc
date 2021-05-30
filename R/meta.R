
#' Quickly refresh & reinstall packages
#' @export
#'
refresh <- function(pkg) {
    require(devtools)
    require(roxygen2)
    org.dir <- getwd()
    target.dir <- system(paste("find ~/R -name", pkg), intern = T)
    
    if (org.dir != target.dir) {setwd(target.dir)}
    document()
    setwd("..")
    install(pkg)
    setwd(org.dir)
}



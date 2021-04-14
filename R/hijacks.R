

#' Identify maximum value
#' 
#' Replaces base function with possibility to return indices
#' 
#' @param x Vector or array of values to compare
#' @param ... Additional arguments to pass to which()
#' 
#' @export
#' 
whichmax <- function(x, na.rm = T, arr.ind = T, ...) which(x == max(x, na.rm = na.rm), arr.ind = arr.ind, ...)


#' Identify minimum value
#' 
#' Replaces base function with possibility to return indices
#' 
#' @param x Vector or array of values to compare
#' @param ... Additional arguments to pass to which()
#' 
#' @export
#' 
whichmin <- function(x, na.rm = T, arr.ind = T, ...) which(x == min(x, na.rm = na.rm), arr.ind = arr.ind, ...)



#' Calculate correlation for scatterplot matrix
#'
#' Calculate per-variable correlation & p-value, to be used as panel input in \code{\link{pairs}} etc.
#' @param x First variable to compare
#' @param y Second variable to compare
#' @param digits number of decimal places to display
#' @param cex.cor Scaling factor to apply to text
#' @export
#' @examples
#' pairs(iris[1:4], pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)], upper.panel = panel.cor)
#'
panel.cor <- function(x, y, digits = 2, cex.cor = 1, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    # correlation coefficient
    r <- cor(x, y, use = "complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste("r = ", txt, sep = "")
    text(0.5, 0.6, txt, cex = cex.cor)

    # p-value calculation
    p <- cor.test(x, y)$p.value
    txt2 <- format(c(p, 0.123456789), digits = digits)[1]
    txt2 <- paste("p = ", txt2, sep = "")
    if(p<0.01) txt2 <- "p < 0.01"
    text(0.5, 0.4, txt2, cex = cex.cor)
}



#' Hijack a function to change default parameters
#'
#' Create copy function with defaults set to something more useful. Taken from r-bloggers somewhere
#' @export
#' @examples
#' .data.frame <- hijack(data.frame, stringsAsFactors = FALSE)
#' dat <- .data.frame(x1 = 1:3, x2 = c("a", "b", "c"))
#' str(dat)  # yay! strings are character
#'
hijack <- function (FUN, ...) {
    .FUN <- FUN
    args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
        formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    }))
    .FUN
}



#' Raise a matrix to a given power
#'
#' Use eigen decomposition to raise matrix x to power n
#' @param x Matrix
#' @param n Power
#'
#' @export
#'
"%^%" <- function(x, n) {with(eigen(x), vectors %*% (values^n * t(vectors)))}



#' Randomly generate a single sample from an inverse-Wishart distribution (robust)
#' 
#' @param S Symmetric, positive-semidefinite scale matrix (may be rank deficient)
#' @param nu Scalar degrees of freedom
#' 
#' @export
#' 
r.invwishart <- function(nu, S) {
    return(chol2inv(rwishartc(nu, chol2inv(suppressWarnings(chol(S, pivot = T))))))
}



#' Randomly generate a single sample from a normal-inverse-Wishart distribution (robust)
#' 
#' @param mu0 Mean vector
#' @param lambda Positive scalar used to obtain V(mu) = Sigma/lambda
#' @param S Symmetric, positive-semidefinite scale matrix (may be rank deficient)
#' @param nu Scalar degrees of freedom
#' 
#' @export
#' 
r.norminvwishart <- function(mu0, lambda, S, nu) {
    Sigma <- r.invwishart(nu, S)
    mu <- rmvn(1, mu0, 1/lambda * Sigma)
    return(list(mu = mu, Sigma = Sigma))
}


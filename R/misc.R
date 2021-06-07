
#' Check equivalence of two objects to a certain DP
#'
#' @export
#'
chk <- function(o1, o2, dp = 9) {
    if(class(o1[1]) == "numeric") {
        res <- all(round(c(o1), dp) == round(c(o2), dp), na.rm = T)
        if(res) return(res) else {
            dp.chk <- sapply(1:9, function(i) all(round(c(o1), i) == round(c(o2), i), na.rm = T))
            if(sum(dp.chk) > 0) {
                cat("Matches to", max(which(dp.chk)), "dp", "\n")
                return(T)
            }
            return(res)
        }
    } else {
        all(c(unlist(o1)) == c(unlist(o2)), na.rm = T)
    }
}


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


#' Raise a matrix to a given power
#'
#' Use eigen decomposition to raise matrix x to power n
#' @param x Matrix
#' @param n Power
#'
#' @export
#'
"%^%" <- function(x, n) {with(eigen(x), vectors %*% (values^n * t(vectors)))}


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
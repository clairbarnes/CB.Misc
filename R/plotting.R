
#' Quickly produce a pdf
#'
#' @param fnm String: path & name of pdf to be created
#' @param plotdef Define function to be plotted
#' @param crop Boolean: apply pdfcrop to the final image?
#' @param ... Further arguments to be passed to pdf function
#'
#' @export
#'
#' @example makepdf("./Documents/tmp.pdf", width = 4, {plot(1:100, dnorm(1:100, 50,20))})
#'
makepdf <- function(fnm, plotdef, crop = T, ...) {

    fnm <- paste0(gsub("\\.pdf", "", fnm), ".pdf")

    pdf(fnm, ...)
    plotdef
    dev.off()

    if(crop) system2("pdfcrop", c(fnm, fnm), invisible = T)
}


#' Wrapper function to get vector of transparent colours
#'
#' @param cols Vector of colours to adjust
#' @param alpha Number between 0 and 1 indicating required transparency. Default is 0.4
#'
#' @return Vector of colours, adjusted to desired transparency.
#'
#' @export
transp <- function(cols, alpha = 0.4) {
    mapply(adjustcolor, cols, alpha = alpha)
}


#' Alternative rainbow colour palette
#'
#' @export
#'
rbow <- function(n, bias = 1, rev = F) {

    cols <- c("midnightblue", "dodgerblue", "chartreuse2", "gold", "red3")
    if(rev) cols <- rev(cols)

    crp <- colorRampPalette(cols, bias = bias)
    crp(n)
}


#' Reset parameters to original values
#' @export
#'
reset.par <- function(...) {
    par(mar = c(5,4,2,2) + 0.1, mfrow = c(1,1), ...)
}



#' Draw test square showing colour ramp
#'
#' Test function: plot a test square, shaded according to a given colour ramp.
#' @export
#'
test.colramp <- function(mycolramp, n = 100) {
  z = matrix(1:n, nrow = 1)
  x = 1
  y = seq(3, 2345, len = n) # supposing 3 and 2345 are the range of your data

  image(x, y, z, col = mycolramp(n), axes = FALSE, xlab = "", ylab = "")
}

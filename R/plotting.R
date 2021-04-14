
#' Quickly produce a pdf
#'
#' @param fnm String: path & name of pdf to be created
#' @param plotdef Define function to be plotted
#' @param crop Boolean: apply pdfcrop to the final image?
#' @param ... Further arguments to be passed to pdf function
#'
#' @export
#'
#' @examples makepdf("./Documents/tmp.pdf", width = 4, {
#'      plot(1:100, dnorm(1:100, 50,20))
#' })
#'
makepdf <- function(fnm, plotdef, crop = T, margin = c(0,0,0,0), ...) {

    fnm <- paste0(gsub("\\.pdf", "", fnm), ".pdf")

    pdf(fnm, ...)
    plotdef
    dev.off()

    if(crop) system2("pdfcrop", c(fnm, fnm))
    add.margin(fnm, margin)
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


#' Colour palette of cool colours (designed as companion to heat.colors)
#' 
#' @export
#' 
cool.colors <- function(n, bias = 1, rev = F) {
    
    cols <- c("midnightblue", "dodgerblue", "aquamarine", "honeydew")
    if(rev) cols <- rev(cols)
    
    crp <- colorRampPalette(cols, bias = bias)
    crp(n)
}


#' Quick palette of colours from cool to warm, separated by white
#' 
#' @param ncool Number of colours to generate from cool.colors()
#' @param nwarm Number of colours to generate from heat.colors()
#' 
#' @export
#' 
temp.cols <- function(ncool, nwarm, mid.col = "white") {
    
    cv <- c(cool.colors(ncool), mid.col, rev(heat.colors(nwarm)))
    cv[!is.na(cv)]
}


#' Reset parameters to original values
#' @export
#'
reset.par <- function(...) {
    par(mar = c(5,4,2,2) + 0.1, mfrow = c(1,1), fig = c(0,1,0,1), ...)
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



#' Plot pch symbols
#' 
#' @export
#' 
pch <- function() {
    plot(rep(1:5,5), rep(5:1, each = 5), pch = 1:25, xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n",
         ylim = c(0,5), xlim = c(0,5), bg = "red")
    text(rep(1:5,5), rep(5:1, each = 5), 1:25, pos = 1, cex = 0.8)
    points(0,5,pch = 0); text(0,5,0,pos = 1, cex = 0.8)
}





#' Filled band between two lines
#'
#' @param x Vector of x values for plotting
#' @param y.max Vector of y values for upper line
#' @param y.mix Vector of y values for lower line
#' @param col Colour for shading
#'
#' @export
#'
filled.band <- function(x, y.max, y.min, col, border = NA, ...) {
  polygon(c(x, rev(x)), c(y.max, rev(y.min)), col = col, border = border, ...)
}


#' Add shaded ellipse to plot
#'
#' @param ell Ellipse to be drawn
#' @param ... Additional graphical parameters to be passed to plotting function
#'
#' @export
#'
filled.ellipse <- function(ell, ...) {

  ell.lower <- ell[which.min(ell[,1]):which.max(ell[,1]),]
  ell.upper <- ell[c(which.max(ell[,1]):100, 1:which.min(ell[,1])),]

  polygon(c(ell.upper[,1], ell.lower[,1]), c(ell.upper[,2], ell.lower[,2]), ...)
}



#' Find points on ellipse from matrix of points
#'
#' @export
#'
px2ellipse <- function(px, ...) {
  ellipse(cov(px), centre = apply(px, 2, mean), ...)
}


#' Plot eigenvectors from a given covariance matrix
#'
#' @export
#'
eivecs <- function(cm, centre = c(0,0), ...) {
  ev <- eigen(cm)

  sapply(1:length(ev$values), function(i) {
    v <- ev$values[i] * ev$vectors[,i]
    Arrows(centre[1], centre[2], centre[1] + v[1], centre[2] + v[2],
           arr.adj = 1, arr.length = 0.3, ...)
    v
  })
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


#' Add margins to a pdf
#'
#' @export
#'
add.margin <- function(fnm, margins = c(10,10,10,10)) {

  fnm <- paste0(gsub("\\.pdf", "", fnm), ".pdf")
  margins <- paste(margins, collapse = " ")
  system2("pdfcrop", args = paste0("--margins '", margins, "' ", fnm, " ", fnm))
}


#' Boxplot with less annoying defaults
#' 
#' @export
#' 
.boxplot <- function(x, cex = 0.4, main = "", xlab = "", ylab = "", ...) {
  boxplot(x, pch = 20, cex = cex, lty = 1, xlab = xlab, ylab = ylab, main = main, col = NA, ...)
}

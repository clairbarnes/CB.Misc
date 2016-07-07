
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


#' Recursively merge list of data frames
#' 
#' Recursively merge list of data frames with a 'master data frame'
#' @param head Data frame with which all others are to be merge (for example, a list of coordinates, to which classifications can be added)
#' @param df.list List of additional data frames to be merged with the head
#' @param ... Additional optional parameters to be passed to the \link{\code{merge}} function.
#' @return Data frame containing merged data
#' @export
#' 
rmerge.df.list <- function(head, df.list, ...) {
    
    # less efficient than lapply, but easier for renaming 
    for (l in 1:length(df.list)) {
        head <- merge(head, df.list[[l]], suffix = c("", paste0(".", names(df.list)[l])), ...)
    }
    return(head)
}


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
panel.cor <- function(x, y, digits = 2, cex.cor = 1, ...)
{
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


#' Scale bar for image plot
#' 
#' Draw scale bar for image plot. Taken from http://menugget.blogspot.co.uk/2011/08/adding-scale-to-image-plot.html#more
#' @export
image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
    if(!missing(breaks)){
        if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
    }
    if(missing(breaks) & !missing(zlim)){
        breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
    }
    if(missing(breaks) & missing(zlim)){
        zlim <- range(z, na.rm=TRUE)
        zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
        zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
        breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
    }
    poly <- vector(mode="list", length(col))
    for(i in seq(poly)){
        poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
    }
    xaxt <- ifelse(horiz, "s", "n")
    yaxt <- ifelse(horiz, "n", "s")
    if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
    if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
    if(missing(xlim)) xlim=XLIM
    if(missing(ylim)) ylim=YLIM
    plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
    for(i in seq(poly)){
        if(horiz){
            polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
        }
        if(!horiz){
            polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
        }
    }
}
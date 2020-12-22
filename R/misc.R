
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MISCELLANEOUS USEFUL THINGS                                                                           ####

#' Get word count from pdf
#' 
#' @param fnm Filename of pdf, including path
#' 
#' @export
#' 
pdf.wordcount <- function(fnm) {
    system(paste0("pdftotext ",fnm," - | tr -d '.' | wc -w"))
}



#' Convert a pdf to greyscale
#' 
#' @param fnm.in Filename of pdf to convert to greyscale, including path
#' @param fnm.out Filename of converted pdf, including path. If not supplied, will simply append '-GS' to the input filename.
#' @param open Boolean: open the converted pdf? Default is N.
#' 
#' @export
#' 
greyscale <- function(fnm.in, fnm.out = gsub("\\.pdf", "-GS.pdf", fnm.in), open = F) {
    
    fnm.in <- paste0(gsub("\\.pdf","",gsub("~/", "/home/clair/", fnm.in)), ".pdf")
    fnm.out <- paste0(gsub("\\.pdf","",gsub("~/", "/home/clair/", fnm.out)), ".pdf")
    
    system2("gs", args = c(paste0("-sOutputFile=",fnm.out), 
                           "-sDEVICE=pdfwrite",
                           "-sColorConversionStrategy=Gray",
                           "-dProcessColorModel=/DeviceGray",
                           "-dCompatibiltyLevel=1.4",
                           "-dNOPAUSE",
                           "-dBATCH",
                           fnm.in))
    if(open) system2("evince", args = fnm.out, wait = F)
}



#' Remove LaTeX production files (.aux, .log etc)
#' 
#' @param fpath File path to clear files from 
#' 
#' @export
#' 
clean.tex <- function(fpath) {
    fl <- list.files(fpath, full.names = T)
    
    rm.ext <- c("log", "gz", "bbl", "aux", "blg", "ent", "toc", "concordance.tex")
    
    fl.ext <- rowSums(sapply(rm.ext, function(ext) grepl(ext, fl)))
    to.clear <- fl[fl.ext == 1]
    sapply(to.clear, file.remove)
}



#' Update all old R packages
#' 
#' @export
#' 
update.all <- function() {
    sapply(rownames(old.packages()), install.packages)
}




#' Refresh references list
#' 
#' @export
#' 
ref.list <- function() {
    file.edit("~/PhD/References/.ref-tree/ref-tree.rnw")
}




#' Integer as 2-character string
#'
#' @param i Integer to be converted to 2-character string
#'
#' @return String of length 2
#' @export
#'
ix <- function(i, ...) {formatC(c(i, ...), width = 2, flag = "0")}



#' Identify whether points fall within an ellipse
#'
#' @param px Points to be tested
#' @param mu Centre of ellipse
#' @param Sigma Covariance matrix describing ellipse
#' @param p Define proportion of density to include
#'
#' @export
#'
px.in.ellipse <- function(px, mu, Sigma, p = 0.95) {

    # transform event space onto ellipsoid space
    # from https://github.com/AndrewLJackson/SIBER/blob/master/R/pointsToEllipsoid.R

    eig <- eigen(Sigma)                      # eigen values and vectors of the covariance matrix
    SigSqrt = eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)      # inverse of sigma
    Z <- t(apply(px, 1, function(x) solve(SigSqrt, x - mu)))        # transform the points

    # from https://github.com/AndrewLJackson/SIBER/blob/master/R/ellipseInOut.R
    r <- stats::qchisq(p, df = ncol(Z))         # Define size of ellipse to include
    inside <- rowSums(Z ^ 2) < r
    cbind(px, "inside" = inside)
}



#' Identify nearest city to click
#' 
#' @export
#'
nearest.city <- function(min.size = 0) {
  require(maps); require(raster)
  xy <- raster::click()
  
  wc <- world.cities[world.cities$pop > min.size,]
  wc[which.min(apply(sweep(wc[,c("long", "lat")], 2, xy, "-")^2, 1, sum)),]
}


#' Check matrix equivalence
#'
#' Confirm whether two matrix equations produce identical output
#'
#' @param expr1 First matrix equation; either a string using A,B,C,D or a formula using matrices in the global environment
#' @param expr2 Second matrix equation; either a string using A,B,C,D or a formula using matrices in the global environment
#' @param dp Number of significant figures to check result to; default is 12.
#' @param seed Integer: seed used to generate random matrices
#'
#' @export
#'
matcheck <- function(expr1, expr2, dp = 12, seed = 1) {

  A <- B <- C <- D <- array(NA, dim = c(3,3))
  I <- diag(3)

  set.seed(seed)
  A[upper.tri(A)] <- A[lower.tri(A)] <- runif(3,0,2)
  diag(A) <- rnorm(3,2,1)

  B[upper.tri(B)] <- B[lower.tri(B)] <- runif(3,0,2)
  diag(B) <- rnorm(3,2,1)

  C[upper.tri(C)] <- C[lower.tri(C)] <- runif(3,0,2)
  diag(C) <- rnorm(3,2,1)

  D[upper.tri(D)] <- D[lower.tri(D)] <- runif(3,0,2)
  diag(D) <- rnorm(3,2,1)

  ev1 <- eval(parse(text = expr1))
  ev2 <- eval(parse(text = expr2))

  all(signif(ev1, dp) == signif(ev2, dp))
}



#' Check equivalence of two objects to a certain DP
#'
#' @export
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




#' Update ratty weight graphs with latest data in Google Sheets
#' 
#' @export
#' 
ratty.weights <- function() {
    
    library(googledrive); library(httpuv); library(readxl)
    
    org.dir <- getwd()
    setwd("~/Documents/Ratties")
    
    options("httr_oob_default" = TRUE)
    
    # download data from Google Drive
    drive_download(as_id("1gzPe8RG2-UfNFyzBuK48J-uvB08PEfs78GKTUMT5QEA"), verbose = F, overwrite = T)
    
    rw <- read_xlsx("Ratty-weights.xlsx")
    rw$Date <- as.Date(rw$Date)
    cat("Last update:", toString(max(rw$Date)), "\n")
    
    first.q <- as.Date(cut(min(rw$Date), "quarter"))
    next.q <- as.Date(cut(as.Date(cut(Sys.Date(), "quarter")) + 100, "quarter"))
    
    r.cols <- c("navyblue", "blue2", "tomato", "red3", "chartreuse3", "forestgreen",
                "maroon4", "violetred")
    
    # plot by date
    pdf("./wplot.pdf"); {
        plot(rw$Date, rw$Monty, type = "n", xlab = "Date", ylab = "Weight (g)")
        abline(h = 0:7 * 100, v = seq(first.q, next.q, by = "1 mon"), 
               col = transp("grey"))
        abline(v = seq(first.q, next.q, by = "3 mon"), col = transp("dimgrey"))
        invisible(sapply(2:(ncol(rw) - 1), function(i) {
            ww <- rw[!is.na(rw[,i]),c(1, i)]
            lines(ww, type = "o", pch = 20, col = r.cols[i-1], cex = 0.5)
        }))
        legend("bottomleft", legend = colnames(rw)[2:(ncol(rw) - 1)], col = r.cols, 
               lty = 1, pch = 20, pt.cex = 0.5, 
               bg = "white") 
    }; dev.off()
    
    ra <- sapply(colnames(rw)[2:(ncol(rw) - 1)], function(rn) {
        wa <- rw[!is.na(rw[,rn]),c("Date", rn)]
        wa$age <- wa$Date - min(wa$Date)
        wa[,c(3,2)]
    }, simplify = F)
    
    amx <- which.max(sapply(ra, function(df) max(df$age)))
    
    pdf("./aplot.pdf"); {
    plot(ra[[amx]], ylim = c(0,700), type = "n", xlab = "Age (months)", ylab = "Weight (g)", xaxt = "n")
    invisible(sapply(1:length(ra), function(i) {
        lines(ra[[i]], type = "o", pch = 20, cex = 0.5, col = r.cols[i])
    }))
    abline(h = (0:7) * 100, v = seq(0:36)*30, col = transp("grey"), lty = 1)
    abline(v =  seq(0, 36, 12)*30, col = transp("dimgrey"), lty = 1)
    axis(1, at = seq(0,as.numeric(max(ra[[amx]]$age)) + 90,90),
         label = seq(0,as.numeric(max(ra[[amx]]$age)) + 90,90) / 30)
    legend("bottomright", legend = colnames(rw)[2:(ncol(rw)-1)], col = r.cols, 
           lty = 1, pch = 20, pt.cex = 0.5, bg = "white")
    }; dev.off()
    
    # upload plots to Google drive
    drive_update(as_id("1W_7mzlnYKlNFeEw5PLs0_SB2mJGRGcOC"), media = "aplot.pdf")
    drive_update(as_id("17dR1Ot_n61KwxFr9MKxEr1Ci53ZgY434"), media = "wplot.pdf")
    
    setwd(org.dir)
}



#' Source a block of lines from a named file
#' 
#' @param fnm Filename to source
#' @param from Line to start sourcing from
#' @param to Line to stop sourcing
#' 
#' @export
#' 
source.lines <- function(fnm, from, to) {
    system.time({
        file.lines <- scan(fnm, what = character(), skip = from - 1, nlines = to - from + 1, sep = "\n")
        file.lines.collapsed <- paste(file.lines, collapse = "\n")
        source(textConnection(file.lines.collapsed))
    })["elapsed"]
}



#' Clear environment, except for selected objects
#' 
#' @param keep Vector of names of objects to keep
#' 
#' @export
#' 
tidyup <- function(keep = "") {
    if(class(keep) != "character") {
        cat("Non-string argument \n")
    } else {
        rm(list = ls(envir = .GlobalEnv)[!ls(envir = .GlobalEnv) %in% keep], envir = .GlobalEnv)
    }
}




#' Get country name from country code
#' 
#' @export
#' 
ccd <- function(ccd) {
    library(maps)
    iso3166[iso3166$a2 == ccd,]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MISCELLANEOUS STATS-Y THINGS                                                                          ####


#' Parameters of mixture of Gaussian distributions
#' 
#' @export
#' 
mixt.pars <- function(mu.list, sig.list, weights) {
    
    M <- length(mu.list)
    if(missing(weights)) weights <- rep(1/M, M)
    
    mu.bar <- colSums(sweep(abind(mu.list, along = 0), 1, weights, "*"))
    
    E.var <- apply(sweep(abind(sig.list, along = 0), 1, weights, "*"), 2:3, sum)
    V.exp <- apply(sweep(aaply(sweep(abind(mu.list, along = 0), 2, mu.bar, "-"), 1,
                               function(mu.diff) mu.diff %*% t(mu.diff)), 1, weights, "*"), 2:3, sum)
    return(list("mean" = mu.bar, "var" = E.var + V.exp))
} 




#' Return mode of x
#' 
#' @param x Vector or matrix of values - assumed to be unimodal
#' 
#' @return Value of (single) mode of x
#' 
#' @export
#' 
unimode <- function(x) {
    dns <- density(x)
    dns$x[which.max(dns$y)]
}


#' Gamma parameters given specified mean/mode and variance
#' 
#' @param mean Mean of Gamma distribution. Only used if mode is not provided.
#' @param mode Mode of Gamma distribution.
#' @param var Variance of Gamma distribution.
#' 
#' @return List containing shape and rate parameters.
#' 
#' @export
#' 
gamm.pars <- function(mode, var, mean) {
    
    if(!missing(mode)) {
        r <- (mode + sqrt(mode^2 + 4*var)) / (2 * var)
        return(list("shape" = 1 + (mode * r), "rate" = r))
    } else {
        return(list("shape" = mean^2 / var, "rate" = mean/var))
    }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# BOOTSTRAPPING                                                                                         ####

#' Get bootstrap distribution of a statistic over a set of values
#' 
#' @param x Vector or array of values
#' @param nsamp Number of bootstrap samples to produce. Default is 10000.
#' @param fn Statistic to calculate for each bootstrap sample. Default is mean.
#' 
#' @return nsamp-length vector of bootstrapped statistics 
#' 
#' @export
#'  
bootstrap <- function(x, nsamp = 10000, fn = mean, ...) {
    n <- length(c(x))
    samp <- array(sample(1:n, n * nsamp, replace = T), dim = c(n,nsamp))
    
    apply(samp, 2, function(ind) fn(x[ind], ...))
}



#' Check where target value falls within bootstrap sample
#' 
#' @param boot.samp Vector of bootstrapped statistic, as returned by \link{\code{bootstrap}}
#' @param target Target value. Default is 0.
#' @param q1 Lower probability bound. Default is 0.025.
#' @param q2 Upper probability bound. Default is 0.975.
#' 
#' @return Value: either 0 (target in lower tail), 1 (target not significant), or 2 (target in upper tail)
#' 
#' @export
#' 
boot.sig <- function(boot.samp, target = 0, q1 = 0.025, q2 = 0.975) {
    findInterval(target, quantile(boot.samp, c(q1, q2)))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MISCELLANEOUS THINGS THAT MAY TURN OUT TO BE ONE-OFFS                                                 ####


#' List selector options from bib file, in alphabetical order
#' 
#' @details eg. to extract all keywords stored in the 'manager' menu for easier revision
#' @param fnm Filename of .bib file (including path). Default is ~/PhD/References/Master-refs.bib
#' @param field Field to interrogate: default is 'keywords'
#' 
#' @return Vector of sorted keywords
#' @export
#' 
#' @examples
#' kw <- bib.dropdown("~/PhD/References/Master-refs.bib")
#' paste(kw, collapse = ";")
#' 
bib.dropdown <- function(fnm = "~/texmf/bibtex/bib/master-refs.bib",
                         field = "keywords", add = NA, string.out = F) {
    dd <- sort(strsplit(gsub("}.+", "", 
                   gsub(paste0(".+selector_", field,":"),"", 
                        readr::read_file(fnm)))
              , ";")[[1]])
    
    if(!is.na(add)) dd <- sort(unique(c(dd, add)))
    if(string.out) {
        return(paste(dd, collapse = ";"))
    } else {
        return(dd)
    }
}


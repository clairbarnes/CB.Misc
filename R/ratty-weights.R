
#' Update ratty weight graphs with latest data in Google Sheets
#' 
#' @export
#' 
ratty.weights <- function() {
    
    library(googledrive); library(readxl)
    # library(httpuv); 
    
    org.dir <- getwd()
    setwd("~/Documents")
    
    options("httr_oob_default" = TRUE)
    
    # download data from Google Drive
    drive_download(as_id("1gzPe8RG2-UfNFyzBuK48J-uvB08PEfs78GKTUMT5QEA"), verbose = F, overwrite = T)
    
    rw <- read_xlsx("Ratty-weights.xlsx")
    rw$Date <- as.Date(rw$Date)
    cat("Last update:", toString(max(rw$Date)), "\n")
    
    first.q <- as.Date(cut(min(rw$Date), "quarter"))
    next.q <- as.Date(cut(as.Date(cut(Sys.Date(), "quarter")) + 100, "quarter"))
    
    r.cols <- c("navyblue", "blue2", "tomato", "red3", "chartreuse3", "forestgreen",
                "maroon4", "violetred", "cyan3", "aquamarine3", "turquoise3")
    
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
               bg = "white", ncol = 2, cex = 0.8) 
    }; dev.off()
    
    ra <- sapply(colnames(rw)[2:(ncol(rw) - 1)], function(rn) {
        wa <- rw[!is.na(rw[,rn]),c("Date", rn)]
        wa$age <- wa$Date - min(wa$Date)
        wa[,c(3,2)]
    }, simplify = F)
    
    amx <- which.max(sapply(ra, function(df) max(df$age)))
    
    pdf("./aplot.pdf"); {
        plot(ra[[amx]], ylim = c(0,700), type = "n", xlab = "Age (months)", ylab = "Weight (g)", xaxt = "n")
        abline(h = (0:7) * 100, v = seq(0:36)*30, col = transp("grey"), lty = 1)
        abline(v =  seq(0, 36, 12)*30, col = transp("dimgrey"), lty = 1)
        invisible(sapply(1:length(ra), function(i) {
            lines(ra[[i]], type = "o", pch = 20, cex = 0.5, col = r.cols[i])
        }))
        axis(1, at = seq(0,as.numeric(max(ra[[amx]]$age)) + 90,90),
             label = seq(0,as.numeric(max(ra[[amx]]$age)) + 90,90) / 30)
        legend("bottomright", legend = colnames(rw)[2:(ncol(rw)-1)], col = r.cols, 
               lty = 1, pch = 20, pt.cex = 0.5, bg = "white", ncol = 2, cex = 0.8)
    }; dev.off()
    
    # upload plots to Google drive
    drive_update(as_id("1W_7mzlnYKlNFeEw5PLs0_SB2mJGRGcOC"), media = "aplot.pdf")
    drive_update(as_id("17dR1Ot_n61KwxFr9MKxEr1Ci53ZgY434"), media = "wplot.pdf")
    
    file.remove("Ratty-weights.xlsx", "aplot.pdf", "wplot.pdf")
    setwd(org.dir)
}

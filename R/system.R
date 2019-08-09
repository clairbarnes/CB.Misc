

#' Change desktop wallpaper
#'
#' @export
#'
change.wallpaper <- function() {
    fnm <- system("gsettings get org.gnome.desktop.background picture-uri", intern = T)
    fnm <- gsub("'", "", gsub("file:///home/clair/Pictures/Wallpapers/", "", fnm))
    fl <- list.files("/home/clair/Pictures/Wallpapers/")
    fl <- fl[-which(fl == fnm)]

    new.fnm <- sample(list.files("/home/clair/Pictures/Wallpapers/"), 1)
    system(paste0("gsettings set org.gnome.desktop.background picture-uri file:///home/clair/Pictures/Wallpapers/",
                  new.fnm))
}



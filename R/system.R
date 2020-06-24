

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



#' Compare music on laptop with music on phone, list any missing tracks
#' 
#' @export
#' 
new.songs.to.add.to.phone <- function() {
    
    mpath <- "/run/user/1000/gvfs/'mtp:host=%5Busb%3A001%2C006%5D/Internal shared storage'/Music"
    spath <- "/run/user/1000/gvfs/'mtp:host=%5Busb%3A001%2C006%5D/Internal shared storage'/syncr"
    
    # list recursively, append / to folders then exclude
    on.phone <- system(paste("ls -R -p ", mpath, " | grep -v /"), intern = T)
    from.syncr <- system(paste("ls -R -p ", spath, " | grep -v /"), intern = T)
    
    on.laptop <- list.files("~/Music", recursive = T)
    on.laptop <- on.laptop[-grep("\\.zip", on.laptop)]
    
    files.on.laptop <- basename(on.laptop)
    
    on.laptop[!files.on.laptop %in% c(on.phone, from.syncr)]
    
}
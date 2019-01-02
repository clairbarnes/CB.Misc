
# BACKUP & MOTHBALLING FUNCTIONS

# backup all vital admin files to central repository (with location)
# add 'getting things gnome' to-do list backup?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Managing git repositories                                                                             ####

#' List all git repositories
#' 
#' @param fpath Filepath to begin searching for git repositories. Default is /home/.
#' 
#' @return Vector of paths to git repositories
#' 
#' @export
#' @examples
#' gl <- list.repos()
#' 
list.repos <- function(fpath = "/home/") {
    gsub("/\\.git", "", gsub("/home/clair/", "~/", system(paste0("find ", fpath, " -name '.git'"), intern = T)))
}


#' Check status of git repository
#' 
#' @param repo.nm File path to git repository
#' 
#' @return Vector of status checks
#' 
#' @export
#' 
#' @examples 
#' t(sapply(gl, repo.status))
#' 
repo.status <- function(repo.nm) {
    
    repo.nm <- gsub("\\.git", "", repo.nm)
    org.wd <- getwd()
    setwd(repo.nm)
    
    gs <- system("git status", intern = T)
    
    setwd(org.wd)
    c("Up to date" = "nothing to commit, working directory clean" %in% gs,
      "Changes to commit" = "Changes to be committed:" %in% gs,
      "Changes not staged" = "Changes not staged for commit:" %in% gs,
      "Untracked files" = "Untracked files:" %in% gs)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Copy key files to 'admin' repo                                                                        ####

# List of key files to back up

key.files <- function() {
    
    # create thesis repository
    
    list("R profile" = "~/.Rprofile",
         "ECMWF API" = "~/.ecmwfapirc",
         "~/Documents/Snippets.R",
         "~/Documents/docstyle.sty",
         "~/Documents/00_setup/Setup.txt",
         "~/.rednotebook/data/",
         "/usr/share/hamster-applet/hamster.db")
    
    # list all rednotebook entries
    # list all knitting/sewing patterns
    # list all source data (.grib, .ncdf)
    # list all music?
    # list all ratty pdfs
    # list all R packages?
    
}
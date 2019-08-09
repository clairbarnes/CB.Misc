
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
    
    # remove blank lines & system messages
    gs <- gs[-grep("\\(use", gs)]
    gs <- gs[!gs == ""]
    
    # count occurrences of key phrases
    to.commit <- grep("Changes to be committed:", gs)
    unstaged <- grep("Changes not staged for commit:", gs)
    untracked <- grep("Untracked files:", gs)
    
    # count lines between key phrases
    n.commits <- gsub(" commit.+", "", gsub(".+by ", "", 
                                            gs[grep("Your branch is ahead of 'origin/master' by ", gs)]))
    n.to.commit <- min(unstaged, untracked, length(gs)+1) - to.commit - 1
    n.unstaged <- min(untracked, length(gs)+1) - unstaged - 1
    n.untracked <- length(gs) - untracked
    
    # return counts
    return(c("Pending commits" =  max(as.integer(n.commits), 0),
             "Changes to commit" = max(n.to.commit, 0),
             "Unstaged changes" = max(n.unstaged, 0),
             "Untracked files" = max(n.untracked, 0)))
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
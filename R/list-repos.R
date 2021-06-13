

#' Check status of git repository
#'
#' @param repo.nm File path to git repository
#' @return Vector of status checks
#'
#' @export
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

    if(n.unstaged == 1 & sum(grepl("modified.+gitignore",gs))) { n.unstaged <- 0 }

    # return counts
    return(c("Pending commits" =  max(as.integer(n.commits), 0),
             "Changes to commit" = max(n.to.commit, 0),
             "Unstaged changes" = max(n.unstaged, 0),
             "Untracked files" = max(n.untracked, 0)))
}

#' List all git repositories
#' @export
#'
list.repos <- function(all = F) {

    repos <- system("find ~ -name '.git'", intern = T)
    repos <- setNames(gsub("/\\.git", "", gsub("/home/clair/", "~/", repos)), NULL)
    repos <- repos[!grepl("Trash", repos)]

    repo.details <- t(sapply(repos, repo.status))

    if(all) {
        return(repo.details)
    } else {
        if(sum(rowSums(repo.details) > 0) == 0) {
            cat("No repositories pending update \n")
        } else {
            return(repo.details[rowSums(repo.details) > 0,,drop = F])
        }
    }
}

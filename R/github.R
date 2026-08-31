#' Fetch GitHub Repository Creation & Last Updated Date
#'
#' @description
#' The GitHub REST API is a powerful tool that allows developers
#' to interact with GitHub programmatically. It provides a set of
#' endpoints that allows a user to create integration, retrieve data,
#' and automate workflows related to GitHub repositories. It is a means
#' by which users can interact with GitHub without directly using a web interface.
#'
#' @details
#' The two functions utilize the GitHub REST API to extract important temporal information about a GitHub repository. \cr\cr
#'  - the getGitRepoStart function is used to retrieve the date a GitHub repository was first created.\cr\cr
#'  - the getGitRepoChange function retrieves the date a GitHub repository was last updated.
#'
#' @rdname github-tweaks
#' @param repo_name full path of the repository eg. "cran/quickcode"
#' @param out.format date output format eg. "%m %d %y"
#' @return date of creation of repository as a character
#'
#' @examples
#' \donttest{
#' # Use default date format
#' getGitRepoStart(repo_name = "oobianom/quickcode")
#'
#' # Specify date format
#' getGitRepoStart(repo_name = "oobianom/quickcode", out.format = "%j|%Y")
#' getGitRepoStart(repo_name = "oobianom/quickcode", out.format = "%D|%j")
#' }
#'
#' @export
getGitRepoStart = function(repo_name,out.format = "%Y-%m-%d"){
  if(!grepl("/",repo_name))
    stop("The format of the repo_name is incorrect. It should include user name and repository eg. 'oobianom/r2social'")
  str = paste0(git.api, repo_name)
  read = readLines(str, warn = FALSE)
  pat = unlist(gregexpr("created_at", read)) + 13
  format(as.POSIXct(substr(read, start = pat, stop = pat + 18), format = "%Y-%m-%dT%H:%M:%S"),out.format)
}



#' @rdname github-tweaks
#' @param repo_name name of the repository
#' @param out.format date output format
#' @return date of the last update of repository as a character
#'
#' @examples
#' \donttest{
#' getGitRepoChange(repo_name = "oobianom/shinyStorePlus", out.format = "%d-%b-%Y")
#' getGitRepoChange(repo_name = "oobianom/r2social", out.format = "%Y/%m/%d")
#' }
#'
#' @export
getGitRepoChange = function(repo_name,out.format = "%Y-%m-%d"){
  if(!grepl("/",repo_name))
    stop("The format of the repo_name is incorrect. It should include user name and repository eg. 'oobianom/r2social'")
  str = paste0(git.api, repo_name)
  read = readLines(str, warn = FALSE)
  pat = unlist(gregexpr("updated_at", read)) + 13
  format(as.POSIXct(substr(read, start = pat, stop = pat + 18), format = "%Y-%m-%dT%H:%M:%S"),out.format)
}



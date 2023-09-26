#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages and their archive date
#'
#' @param startsWith one letter that the package name starts with eg. a, e, f
#' @param after packages archived after a specific date eg. 05-2011
#' @param inc.date should archive date be included in the result
#' @param as return result as data frame or as vector
#'
#' @return a data frame or vector containing listing of all archived R packages
#'
#' @note
#' The startsWith argument should be one letter and should be in lowercase
#'
#' @examples
#' # Task 1: get archived R packages with names beginning with A
#' head(archivedPkg(startsWith = "a"))
#'
#'
#' # Task 2: return the packages from Task 1 without including latest archive date
#' res.dt2 <- archivedPkg(startsWith = "b", inc.date = FALSE)
#' res.dt2[1:10,]
#'
#' # Task 3: return the results from Task 2 as a vector
#' res.dt3 <- archivedPkg(startsWith = "c", inc.date = FALSE, as = "vector")
#' res.dt3[1:10,]
#'
#' # Task 4: return the archived packages beginning with Y
#' # Note that startsWith should be lowercase
#' head(archivedPkg(startsWith = "y"))
#'
#'
#' @export
#'

archivedPkg <- function(startsWith = c("all",letters), after="2011-05", inc.date =TRUE, as =c("data.frame","vector")) {
  startsWith <- match.arg(startsWith)
  if(startsWith == "all") startsWith <- letters
  as <- match.arg(as)
  res <- data.frame()

  #fetch data and update res
  for(i in startsWith)
    data_push(res,utils::read.csv(
    file = paste0(
      "https://quickcode.obi.obianom.com/CRAN/archiveddata_",tolower(i),".txt?count=0&auth=1"),
    header = TRUE, quote = "'",
    skip = 1),which = "rows")

  if(not.null(after)){
    # filter by time
    res$difftime <- res$latest.archive - as.POSIXct(after)
    res <- res[res$difftime >= 0,]
    res$difftime <- NULL
  }


  if(!inc.date) res$latest.archive = NULL
  if(as == "vector") res <- as.vector(res)
  res
}


#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages and their archive date
#'
#' @param startsWith one letter that the package name starts with eg. a, e, f
#' @param after packages archived after a specific date eg. 2011-05-10
#' @param inc.date should archive date be included in the result
#' @param as return result as data frame or as list
#'
#' @return a data frame or list containing listing of all archived R packages
#'
#' @section Use case:
#' This function allows the retrieval of various R packages archived by CRAN along with
#' the respective latest archive date. The packages retrieved include both active and
#' inactive R projects submitted to CRAN. When a new version of an active R package is published,
#' the older versions of the package gets archived. In the same way, when a package is
#' decommissioned from CRAN active projects for one reason or another, it gets archived.
#'
#' @note
#' * The "startsWith" argument should be one letter and should be in lowercase \cr
#' * If no argument is provided for "startsWith", all the packages will be retrieved\cr
#' * The format of the "after" argument must be YYYY-MM-DD e.g. 2022-04-11
#'
#'
#' @examples
#' \donttest{
#' # Task 1: get archived R packages with names beginning with C or All
#' head(archivedPkg(startsWith = "all"), n= 10) #retrieves all packages
#' head(archivedPkg(startsWith = "c"), n= 10) #retrieves only packages beginning with a
#'
#' # Task 2: return the packages from Task 1 without including latest archive date
#' res.dt2 <- archivedPkg(startsWith = "b", inc.date = FALSE)
#' res.dt2[1:10,]
#'
#' # Task 3: return the results from Task 2 as a list
#' res.dt3 <- archivedPkg(startsWith = "c", inc.date = FALSE, as = "list")
#' res.dt3$name[1:10]
#'
#' res.dt3 <- archivedPkg(startsWith = "e", as = "list")
#' res.dt3$name[1:10]
#'
#' # Task 4: return the archived packages beginning with Y archived after 2022-08-12
#' # Note that startsWith should be lowercase
#'
#' #without archive date
#' yRPkg <- archivedPkg(startsWith = "y", after= NULL)
#' nrow(yRPkg) #number of rows returned
#' head(yRPkg, n = 15) #show first 15 rows
#'
#' #with archive date
#' yRPkg2 <- archivedPkg(startsWith = "y", after= "2022-08-12")
#' nrow(yRPkg2) #number of rows returned
#' head(yRPkg2, n = 15) #show first 15 rows, notice no archive date before 2022-08-12
#' }
#' @export
#'

archivedPkg <- function(startsWith = c("all",letters), after = NULL, inc.date = TRUE, as =c("data.frame","list")) {
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
    if(is.na(as.Date(after, "%Y-%m-%d")))
      stop("Date format must be YYYY-MM-DD e.g 2022-05-15")
    # filter by time
    res$difftime <- as.numeric(as.POSIXct(res$latest.archive) - as.POSIXct(after))
    res <- res[res$difftime >= 0,]
    res$difftime <- NULL
  }


  if(!inc.date) res$latest.archive <- NULL
  else res$latest.archive <- as.Date(res$latest.archive)
  if(as == "list") res <- as.list(res)
  res
}


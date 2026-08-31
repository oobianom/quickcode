#' Check if a call or expression produces errors
#'
#' Whether a function or series of calls results in error
#'
#' @param ... the expression or function calls
#' @note
#' More information, check: https://rpkg.net/package/quickcode
#'
#' @examples
#' # this should not produce error
#' # so the function result should be FALSE
#' has.error({
#'   x = 8
#'   y = number(10)
#'   res = x + y
#' })
#'
#' # this should produce the following error
#' # Error in x + y : non-numeric argument to binary operator
#' # so the function result should be TRUE
#' has.error({
#'   x = 8
#'   y = "random"
#'   res = x + y
#' })
#'
#' # this should result in error because
#' # the dataset does not contain a "rpkg.net" column
#' # the result should be TRUE
#' df1 = mtcars
#' has.error(df1[,"rpkg.net"])
#'
#' @return boolean value to indicate if the expression produces errors
#' @export

has.error <- function(...) {
  .error <- FALSE
  tryCatch(...,
           error = function(e) {
             .error <<- TRUE
           }
  )
  .error
}

#' Nullish coalescing operator
#'
#' Alternative return for empty, null or na statements.Return alternative if the value of expression is empty or NA or NULL
#'
#' @rdname orsign
#' @param test an object to return
#' @param alternative alternative object to return
#' @return value of test if not null or empty, else return value of alternative
#' @note
#' Equivalent to Nullish coalescing operator ?? in javascript or PHP like $Var = $operand1 ?? $operand2;
#'
#' @examples
#'
#' test1 <- c(4,NA,5,2,0,21)
#'
#' test2 <- data.frame(ID = 1:10,ED = LETTERS[10:1])
#'
#' # One may also choose to use
#'
#' test2[,1] %or% "A"
#'
#' test2[which(test2$ID == 323),2] %or% "CCCCC"
#'
#' number(1) %or% "Placeholder"
#'
#' number(10) %or% "Placeholder"
#'
#' NA %or% "Random"
#'
#' NULL %or% "Random"
#'
#' "" %or% "Random"
#'
#' or(test1[which(test1==4)],100)
#'
#'
#' or(test1[which(test1==43)],100)
#'
#' or(test2[which(test2$ID == 10),2],"BBBBB")
#'
#' or(test2[which(test2$ID == 323),2],"CCCCC")
#'

#'
#' @export
or <- function(test,alternative){
  res <- test
  if(!length(res)) return(alternative)
  if(all(is.null(res)|is.na(res) | is.nan(res) | !not.empty(res)))
    return(alternative)
  res

}

#' @rdname orsign
#' @export
`%or%` <- or

#' Error coalescing operator
#'
#' Alternative return for error statements.Return alternative if the value of expression is erroneous
#'
#' @param test an object to return
#' @param alternative alternative object to return
#' @return value of test if error, else return value of alternative
#' @rdname errorout
#'
#' @examples
#' # The following statement would produce
#' # error because 'stat1' does not exist
#'
#' # stat1 + 1
#'
#' # To prevent the statement from
#' # stopping the process, when can have alternative out
#' alt = 0
#' error.out(stats1 + 1, alt)
#'
#' @export
error.out <- function(test,alternative = ""){
  tryCatch({
    test
  },error = function(e){
    alternative
  })
}

#' @rdname errorout
#' @export
`%eo%` <- error.out


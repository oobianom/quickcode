#' Not inherit from any of the classes specified
#'
#' Opposite of base::inherits().
#' Indicates whether its first argument inherits from any of the classes specified in the what argument. If which is TRUE then an integer vector of the same length as what is returned. Each element indicates the position in the class(x) matched by the element of what; zero indicates no match. If which is FALSE then TRUE is returned by inherits if any of the names in what match with any class.
#'
#' @inheritParams base::inherits
#' @param what a character vector naming classes.
#' @return a boolean value to indicate if !inherits
#' @examples
#' keep.cols = "a character"
#' class(keep.cols) # class is character
#' not.inherits(keep.cols,"character")
#'
#' num.var = 1L
#' class(num.var) # class is integer
#' not.inherits(num.var,"double")
#'
#' @export

not.inherits <- function(x, what, which = FALSE)!{
  inherits(x = x, what = what, which = which)
  }


#' Shuffle a list object just like shuffle in PHP
#'
#' Shorthand to shuffle a list and save
#'
#' @param . list to shuffle
#' @param seed apply seed if indicated for reproducibility
#' @return shuffled list of items store to the list name
#'
#' @examples
#' list001 <- list("a" = 1:5,
#'            "b" = letters[1:5],
#'            c = LETTERS[1:10],
#'            "2" = number(5,5),
#'            "e" = randString(5,5))
#' list001 #show initial list
#'
#' #illustrate basic functionality
#' list_shuffle(list001)
#' list001 #shuffle and resaved to variable
#'
#' list.f2<-list001
#' list_shuffle(list.f2)
#' list.f2 #first output
#'
#' list.f2<-list001
#' list_shuffle(list.f2)
#' list.f2 # different output from first output top
#'
#' list.f2<-list001
#' list_shuffle(list.f2,seed = 344L)
#' list.f2 #second output
#'
#' list.f2<-list001
#' list_shuffle(list.f2,seed = 344L)
#' list.f2 #the same output as second output top
#'
#' @export
#'

list_shuffle <- function(., seed = NULL) {
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  list1 <- get(as.character(..), envir = parent.frame())

  if(not.null(seed))set.seed(seed)

  assign(as.character(..), sample(names(list1)), envir = parent.frame())
}

#' Shuffle a vector just like shuffle in PHP
#'
#' Shorthand to shuffle a vector and save
#'
#' @param . vector to shuffle
#' @param replace replace selected value
#' @param prob probability of occurrence
#' @param seed apply seed if indicated for reproducibility
#' @return shuffled vector of items store to the vector name
#'
#' @examples
#' v1<-c(3,45,23,3,2,4,1)
#'
#'
#' #demonstrate vector_shuffle
#' vector_shuffle(v1)
#' v1 # show outputs
#'
#' #demonstrate reproducibility in shuffle with seed
#' v0<-v1
#' vector_shuffle(v0)
#' v0 #first output
#'
#' v0<-v1
#' vector_shuffle(v0)
#' v0 # different output from first output top
#'
#' v0<-v1
#' vector_shuffle(v0,seed = 232L)
#' v0 #second output
#'
#' v0<-v1
#' vector_shuffle(v0,seed = 232L)
#' v0 #the same output as second output top
#' @export
#'

vector_shuffle <- function(., replace = FALSE, prob = NULL, seed = NULL) {
  if(not.vector(.)) stop("The first element must be a vector")
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  val <- get(as.character(..), envir = parent.frame())

  if(not.null(seed))set.seed(seed)

  assign(as.character(..), sample(val, length(val), replace = replace, prob = prob), envir = parent.frame())
}

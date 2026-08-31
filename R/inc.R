#' Increment vector by value
#'
#' Increment the content of a vector and re-save as the vector
#'
#' @param . vector of number(s)
#' @param add number to add
#' @return a vector incremented by a number
#' @details
#' This function is very useful when writing complex codes involving loops.
#' Apart from the for loop, this can be useful to quickly increment a variable located outside the loop
#' by simply incrementing the variable by 1 or other numbers. Check in the example section for a specific use.
#' Nonetheless, one may also choose to use this function in any other instance, as it's simple purpose is
#' to increase the value of a variable by a number and then re-save the new value to that variable.
#'
#' @examples
#' num1 <- sample(330:400,10)
#' num1#before increment
#'
#' # increment num1 by 1
#' inc(num1)
#' num1 #after increment
#'
#' # increment num1 by 5
#' num1 #before increment
#' inc(num1, add= 10)
#' num1 #after increment
#'
#' #when used in loops
#'
#' #add and compare directly
#' rnum = 10
#' inc(rnum) == 11 #returns TRUE
#' rnum #the variable was also updated
#'
#' # use in a for loop
#' ynum = 1
#' for( i in c("scientist","dancer","handyman","pharmacist")){
#' message("This is the item number ")
#' message(ynum)
#' message(". For this item, I am a ")
#' message(i)
#'
#' #decrement easily at each turn
#' plus(ynum)
#' }
#'
#'
#' #use in a repeat loop
#' xnum = 1
#' repeat{ #repeat until xnum is 15
#' message(xnum)
#' if(inc(xnum) == 15) break
#' }
#' @export
#'
inc <- function(., add = 1L) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), (get(as.character(..), envir = parent.frame()) + add), envir = parent.frame())
}

#' @inherit inc
#' @export
plus <- inc


#' Decrease vector by value
#'
#' decrease the content of a vector and re-save as the vector
#'
#' @param . vector of number(s)
#' @param minus number to minus
#' @return a vector decreased by a number
#' @details
#' Similar to the inc and plus functions, the minus function is very useful when writing complex codes involving loops.
#' Apart from the for loop, minus can be useful to quickly decrease the value of a variable located outside the loop
#' by simply decreement the variable by 1 or other numbers. Check in the example section for a specific use.
#' Given the scope, one may also choose to use this function in any other instances, as it's simple purpose is
#' to decrease the value of a variable by a number and then re-save the new value to that variable.
#' @examples
#' num1 <- sample(5:150,10)
#' num1
#'
#' # decrease num1 by 1
#' num1 #before decrease
#' minus(num1)
#' num1 #after decrease
#'
#' # decrease num1 by 5
#' num1 #before decrease
#' minus(num1, minus = 5)
#' num1 #after decrease
#'
#'
#'
#' #when used in loops
#'
#' #add and compare directly
#' rnum = 23
#' minus(rnum) == 220 #returns FALSE
#' rnum #the variable was also updated
#'
#'
#' # use in a for loop
#' ynum = 100
#'
#' for( i in c("teacher","student","lawyer","pharmacist")){
#' message("This is the item number ")
#' message(ynum)
#' message(". For this item, I am a ")
#' message(i)
#'
#' #decrement easily at each turn
#' minus(ynum,3)
#' }
#'
#' #use in a repeat loop
#' xnum = 100
#' repeat{ #repeat until xnum is 85
#' message(xnum)
#' if(minus(xnum) == 85) break
#' }
#' @export
#'
minus <- function(., minus = 1L) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), (get(as.character(..), envir = parent.frame()) - minus), envir = parent.frame())
}

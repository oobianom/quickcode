% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supervariable.R
\name{newSuperVar}
\alias{newSuperVar}
\title{Create and use a super variable with unique capabilities}
\usage{
newSuperVar(variable, value = 0L, lock = FALSE, editn = NULL)
}
\arguments{
\item{variable}{variable name for super variable}

\item{value}{value of the variable}

\item{lock}{lock variable to change}

\item{editn}{number of times the super variable may be set to a new value using .set(). \cr
- Set to NULL to allow unlimited value change \cr
- Set to 0 to prevent editing the super variable \cr}
}
\value{
no visible return, but variable is created and stored with various functionalities
}
\description{
Create a variable that supersedes other variables and has various functionalities
}
\note{
\strong{What you should know about the functionality:} \cr\cr
This function ensures that a variable is created and may not easily be altered.
It helps preserve the original variable by providing only limited access to the variable.\cr\cr
Creation of this super variable automatically attached some key functions to it,
such that the user is able to call the function like \code{.set()}, \code{.rm()}.\cr\cr
Super variable value may be set from any scope using the \code{.set()} function, which
means that it is granted global variable features without being present within the
global environment of the current section.\cr\cr The variable name of the super variable may
be overwritten in the local environment, but this would not alter the super variable.
It means that once the local variable is removed, the super variable remains available
for use.\cr\cr

\strong{Use cases:} \cr\cr
 - Preserve originality of variable within an R session. Avoid inadvertent deletion.\cr\cr
 - Widely accessible from any scope e.g functions, lapply, loops, local environment etc\cr\cr
 - Restricted mutability of variable using set function e.g varname\code{.set()}\cr\cr
 - Variable with easy function calls by attaching '.'\cr\cr
 - Variable with un-mutable class when changing its value \cr\cr
 - Variable with restricted number of times it can be changed \cr\cr
}
\examples{
# Task: create a super variable to
# store dataset that should not be altered
newSuperVar(mtdf, value = austres) # create a super variable
head(mtdf) # view it
mtdf.class # view the store class of the variable, it cannot be changed
# it means that when the super variable is edited, the new value MUST have the same class "ts"

# create and lock super variable by default
# extra security to prevent changing
newSuperVar(mtdf3, value = beaver1, lock = TRUE)
head(mtdf3) # view
mtdf3.round(1) # round to 1 decimal places
head(mtdf3) # view
mtdf3.signif(2) # round to 2 significant digits
head(mtdf3) # view

# Task: create a new super variable to store numbers
# edit the numbers from various scopes
newSuperVar(edtvec, value = number(5))
edtvec # view content of the vector

# edtvec.set(letters) #ERROR: Cannot set to value with different class than initial value

edtvec.set(number(20)) # set to new numbers
edtvec # view output

for (pu in 1:8) {
  print(edtvec) # view output within loop
  edtvec.set(number(pu)) # set to new numbers within for loop
}

lc <- lapply(1:8, function(pu) {
  print(edtvec) # view output within loop
  edtvec.set(number(pu)) # set to new numbers within lapply loop
})

# see that the above changed the super variable easily.
# local variable will not be altered by the loop
# example
bim <- 198
lc <- lapply(1:8, function(j) {
  print(bim)
  bim <- j # will not alter the value of bim in next round
})


# Task: create and search data.frame
# create a new super variable with value as mtcars
# search if it contains the numeric value 21
newSuperVar(lon2, value = mtcars) # declares lon2
lon2 # view content of lon2
lon2.contains("21.0") # WRONG - since df.col is not specified,
# only the first column is search for the character "21.0"
lon2.contains("21.0", df.col = "mpg") # WRONG - searches mpg column
# for the character "21.0"
lon2.contains(21.0, df.col = "mpg") # CORRECT - search mpg column for the
# numeric value 21.0

# remove lon2 as a super variable
exists("lon2") # before removal
lon2.rm()
exists("lon2") # after removal

# Task: create and search vector
# create a new super variable with value as 10 random numbers
# search if it contains the numeric value 72
newSuperVar(lon3, value = number(10, seed = 12)) # declares lon3
lon3 # view content of lon3
lon3.contains(72) # should give TRUE or false if the vector contains the value 45
lon3.contains(72, fixed = TRUE) # should give TRUE or false if the vector contains the value 45

# remove lon3 as a super variable
lon3.rm()


#Task: create a super variable that can only be edited 3 times
newSuperVar(man1, value = number(5), editn = 3)
man1 # view value

man1.set(number(10)) # change value first time
man1 # view value

man1.set(number(2)) # change value second time
man1 # view value

man1.set(number(1)) # change value third time
man1 # view value

man1.set(number(5)) # change value forth time,
# should not change because max change times exceeded
man1 # view value

}

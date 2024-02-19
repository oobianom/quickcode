#' Create and Use a super variable
#'
#' Create a variable that supercedes other variables and has various functionalities
#'
#' @param ... variable name super variable
#' @param value value of the variable
#' @param lock lock variable to change
#'
#' @note
#' This function ensures that a variable is created and may not easily be altered.
#' It helps preserve the original variable by providing only limited access to the variable.
#' Creation of this super variable automatically attached some key functions to it,
#' such that the user is able to call the function like var.set(), var.rm().
#' Super variable value may be set from any scope using the .set() function, which
#' means that it is granted global variable features without being present within the
#' global environment of the current section. The variable name of the super variable may
#' be overwritten in the local environment, but this would not alter the super variable.
#' It means that once the local variable is removed, the super variable remains the available
#' for use.
#'
#' @details
#' USE CASE: \cr
#'  - Preserve originality of variable within an R session. Avoid indavertent deletion.
#'  - Widely accessible from any scope e.g functions, lapply, loops, local environment etc
#'  - Restricted mutability of variable using set function e.g varname.set()
#'  - Variable with easy function calls by attaching '.'
#'
#'
#' @return no visible return, but variable is created and stored with various functionalities
#'
#' @examples
#' # Task: create a super variable to
#' # store dataset that should not be altered
#' newSuperVar(mtdf, value = mtcars)
#'
#' # Task: create a new super variable to store numbers
#' # edit the numbers from various scopes
#' newSuperVar(edtvec, value = number(5))
#' edtvec # view content of the vector
#'
#' edtvec.set(number(20)) #set to new numbers locally
#' edtvec # view output
#'
#' for(pu in 1:8){
#' edtvec.set(number(pu)) #set to new numbers within for loop
#' edtvec # view output within loop
#' }
#'
#' lapply(1:8, function(pu){
#' edtvec.set(number(pu)) #set to new numbers within lapply loop
#' edtvec # view output within loop
#' })
#'
#' # see that the above changed the super variable easily.
#' # local variable will not be altered by the loop
#' # example
#' bim = 198
#' lc = lapply(1:8, function(j){ print(bim);bim = j; })
#'
#'
#' # Task: create and search data.frame
#' # create a new super variable with value as mtcars
#' # search if it contains the numeric value 21
#' newSuperVar(lon2, value = mtcars) # declares lon2
#' lon2 # view content of lon2
#' lon2.contains("21.0") # WRONG - since df.col is not specific,
#' only the first column is search for the character "21.0"
#' lon2.contains("21.0",df.col = "mpg") # WRONG - searches mpg column
#' for the character "21.0"
#' lon2.contains(21.0,df.col = "mpg") # CORRECT - search mpg column for the
#' numeric value 21.0
#'
#' # remove lon2 as a super variable
#' lon2.rm()
#'
#'
#' # Task: create and search vector
#' # create a new super variable with value as 10 random numbers
#' # search if it contains the numeric value 72
#' newSuperVar(lon3, value = number(10, seed=12)) # declares lon3
#' lon3 # view content of lon3
#' lon3.contains(72) # should give TRUE or false if the vector contains the value 45
#' lon3.contains(72, fixed = TRUE) # should give TRUE or false if the vector contains the value 45
#'
#' # remove lon3 as a super variable
#' lon3.rm()
#' @export

newSuperVar <- function(variable, value = 1, lock = TRUE) {
  .v <- as.list(substitute(args(variable))[-1L])
  .spkg <- new.env()
  classi <- class(value)
  if (!is.attached(super.)) {
    attach(.spkg, name = super.)
  }
  if (not.exists("super.env")) {
    super.env <- as.environment(super.)
    assign("super.env", super.env, envir = super.env)
    lockBinding("super.env", env = super.env)
  }
  # sub functions
  .join <- function(ij4) {
    eval(parse(text = deparse(ij4)))
  }

  # FUN
  els <- c("", ".rm", ".set", ".contains", ".round", ".signif", ".class")

  # remove
  rmv <- function() {
    rm(list = paste0(i, els), envir = super.env)
  }
  # set
  setv <- function(value) {
    ioo <- as.character(i)
    if (classi != class(value)) stop("Class of new value must be ", classi, ", the same as the original value of ", i)
    unlockBinding(ioo, env = super.env)
    assign(ioo, value, envir = super.env)
    lockBinding(ioo, env = super.env)
  }
  setl <- function(value) {
    if (classi != class(value)) stop("Class of new value must be ", classi, ", the same as the original value of ", i)
    assign(as.character(i), value, envir = super.env)
  }

  # round
  rldd <- function(digits = 0) {
    if(lock)unlockBinding(ioo, env = super.env)
    bv <- round(get(as.character(i), envir = super.env),digits)
    assign(ioo, bv, envir = super.env)
    if(lock)lockBinding(ioo, env = super.env)
  }

  # significant figures
  sgif <- function(digits = 0) {
    if(lock)unlockBinding(ioo, env = super.env)
    bv <- signif(get(as.character(i), envir = super.env),digits)
    assign(ioo, bv, envir = super.env)
    if(lock)lockBinding(ioo, env = super.env)
  }

  # contains
  cntsa <- function(pattern, ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE, invert = FALSE, df.col) {
    if(classi[1] %in% c("matrix", "array","data.frame")){
        z <- data.frame(get(as.character(i), envir = super.env))
        res <- z[grep(pattern = pattern, x = z[,df.col], ignore.case = ignore.case, perl = perl, value = FALSE,
               fixed = fixed, useBytes = useBytes, invert = invert), ]
    }else{
    res <- grep(pattern = pattern, x = get(as.character(i), envir = super.env), ignore.case = ignore.case, perl = perl, value = FALSE,
         fixed = fixed, useBytes = useBytes, invert = invert)
    }

    as.logical(length(unlist(res)))
  }

  # assign to variable
  for (i in .v) {
    if(exists(as.character(i), envir = super.env))
      stop("The variable '",i,"' already exists as a superVar. In order to redeclare it,
           you need to remove the existing object by using '",i,".rm()'")
    assign(as.character(i), value, envir = super.env)
    if (lock) lockBinding(i, env = super.env)
    assign(paste0(i, ".class"), classi, envir = super.env)
    assign(paste0(i, ".rm"), .join(rmv), envir = super.env)
    assign(paste0(i, ".set"), .join(ifelse(lock, setv, setl)), envir = super.env)
    assign(paste0(i, ".contains"), .join(cntsa), envir = super.env)
    assign(paste0(i, ".round"), .join(rldd), envir = super.env)
    assign(paste0(i, ".signif"), .join(sgif), envir = super.env)
    lockBinding(paste0(i, ".class"), env = super.env)
    lockBinding(paste0(i, ".rm"), env = super.env)
    lockBinding(paste0(i, ".set"), env = super.env)
    lockBinding(paste0(i, ".contains"), env = super.env)
    lockBinding(paste0(i, ".round"), env = super.env)
    lockBinding(paste0(i, ".signif"), env = super.env)
  }
  rm(rmv,setv,setl,rldd,cntsa,sgif)
}





# to-do v0.9
# function to track function usage
# type3 <-function(x)type1(x)
# type1 <- function(x){
#   mean(x)
#   sd(x)
#   tracker()
# }
#
# tracker <- function(apiId){
#   getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
#   getFuncName <- strsplit(getCall,"\\(")[[1]][1]
#   print(getFuncName)
#   ls("package:quickcode")
# }
# type1(number(10))
# type3(number(12))


# to-do
# function to track function usage
# type3 <-function(x)type1(x)
# type1 <- function(x){
#   mean(x)
#   sd(x)
#   tracker()
# }
#
# tracker <- function(apiId){
#   getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
#   getFuncName <- strsplit(getCall,"\\(")[[1]][1]
#   print(getFuncName)
#   ls("package:quickcode")
# }


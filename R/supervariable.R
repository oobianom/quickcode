#' Create and use a super variable with unique capabilities
#'
#' Create a variable that supersedes other variables and has various functionalities
#'
#' @param variable variable name for super variable
#' @param value value of the variable
#' @param lock lock variable to change
#' @param editn number of times the super variable may be set to a new value using .set(). \cr
#'  - Set to NULL to allow unlimited value change \cr
#'  - Set to 0 to prevent editing the super variable \cr
#'
#' @note
#' \strong{What you should know about the functionality:} \cr\cr
#' This function ensures that a variable is created and may not easily be altered.
#' It helps preserve the original variable by providing only limited access to the variable.\cr\cr
#' Creation of this super variable automatically attached some key functions to it,
#' such that the user is able to call the function like \code{.set()}, \code{.rm()}.\cr\cr
#' Super variable value may be set from any scope using the \code{.set()} function, which
#' means that it is granted global variable features without being present within the
#' global environment of the current section.\cr\cr The variable name of the super variable may
#' be overwritten in the local environment, but this would not alter the super variable.
#' It means that once the local variable is removed, the super variable remains available
#' for use.\cr\cr
#'
#' \strong{Use cases:} \cr\cr
#'  - Preserve originality of variable within an R session. Avoid inadvertent deletion.\cr\cr
#'  - Widely accessible from any scope e.g functions, lapply, loops, local environment etc\cr\cr
#'  - Restricted mutability of variable using set function e.g varname\code{.set()}\cr\cr
#'  - Variable with easy function calls by attaching '.'\cr\cr
#'  - Variable with un-mutable class when changing its value \cr\cr
#'  - Variable with restricted number of times it can be changed \cr\cr
#'
#'
#' @return no visible return, but variable is created and stored with various functionalities
#'
#' @examples
#' # Task: create a super variable to
#' # store dataset that should not be altered
#' newSuperVar(mtdf, value = austres) # create a super variable
#' head(mtdf) # view it
#' mtdf.class # view the store class of the variable, it cannot be changed
#' # it means that when the super variable is edited, the new value MUST have the same class "ts"
#'
#' # create and lock super variable by default
#' # extra security to prevent changing
#' newSuperVar(mtdf3, value = beaver1, lock = TRUE)
#' head(mtdf3) # view
#' mtdf3.round(1) # round to 1 decimal places
#' head(mtdf3) # view
#' mtdf3.signif(2) # round to 2 significant digits
#' head(mtdf3) # view
#'
#' # Task: create a new super variable to store numbers
#' # edit the numbers from various scopes
#' newSuperVar(edtvec, value = number(5))
#' edtvec # view content of the vector
#'
#' # edtvec.set(letters) #ERROR: Cannot set to value with different class than initial value
#'
#' edtvec.set(number(20)) # set to new numbers
#' edtvec # view output
#'
#' for (pu in 1:8) {
#'   print(edtvec) # view output within loop
#'   edtvec.set(number(pu)) # set to new numbers within for loop
#' }
#'
#' lc <- lapply(1:8, function(pu) {
#'   print(edtvec) # view output within loop
#'   edtvec.set(number(pu)) # set to new numbers within lapply loop
#' })
#'
#' # see that the above changed the super variable easily.
#' # local variable will not be altered by the loop
#' # example
#' bim <- 198
#' lc <- lapply(1:8, function(j) {
#'   print(bim)
#'   bim <- j # will not alter the value of bim in next round
#' })
#'
#'
#' # Task: create and search data.frame
#' # create a new super variable with value as mtcars
#' # search if it contains the numeric value 21
#' newSuperVar(lon2, value = mtcars) # declares lon2
#' lon2 # view content of lon2
#' lon2.contains("21.0") # WRONG - since df.col is not specified,
#' # only the first column is search for the character "21.0"
#' lon2.contains("21.0", df.col = "mpg") # WRONG - searches mpg column
#' # for the character "21.0"
#' lon2.contains(21.0, df.col = "mpg") # CORRECT - search mpg column for the
#' # numeric value 21.0
#'
#' # remove lon2 as a super variable
#' exists("lon2") # before removal
#' lon2.rm()
#' exists("lon2") # after removal
#'
#' # Task: create and search vector
#' # create a new super variable with value as 10 random numbers
#' # search if it contains the numeric value 72
#' newSuperVar(lon3, value = number(10, seed = 12)) # declares lon3
#' lon3 # view content of lon3
#' lon3.contains(72) # should give TRUE or false if the vector contains the value 45
#' lon3.contains(72, fixed = TRUE) # should give TRUE or false if the vector contains the value 45
#'
#' # remove lon3 as a super variable
#' lon3.rm()
#'
#'
#' #Task: create a super variable that can only be edited 3 times
#' newSuperVar(man1, value = number(5), editn = 3)
#' man1 # view value
#'
#' man1.set(number(10)) # change value first time
#' man1 # view value
#'
#' man1.set(number(2)) # change value second time
#' man1 # view value
#'
#' man1.set(number(1)) # change value third time
#' man1 # view value
#'
#' man1.set(number(5)) # change value forth time,
#' # should not change because max change times exceeded
#' man1 # view value
#'
#' @export

newSuperVar <- function(variable, value = 0L, lock = FALSE, editn = NULL) {
  .v <- as.list(substitute(args(variable))[-1L])
  .spkg <- new.env()
  .r231 = paste0("att",frt6,"(.spkg, name = super.)")
  .r232 = paste0("unl",frt5,"ing(ioo, env = .pos80cbca8022ece6174797e10bb8aebf18)")
  classi <- class(value)
  if (!is.attached(super.)) {
    eval(parse(text = .r231))
  }
  if (not.exists(".pos80cbca8022ece6174797e10bb8aebf18")) {
    .pos80cbca8022ece6174797e10bb8aebf18 <- as.environment(super.)
  }
  # sub functions
  .join <- function(ij4) {
    eval(parse(text = deparse(ij4)))
  }

  # FUN
  els <- c("", ".rm", ".set", ".contains", ".round", ".signif", ".class",".head",".tail",".push",".pop")

  # remove
  rmv <- function() {
    rm(list = paste0(i, els), envir = .pos80cbca8022ece6174797e10bb8aebf18)
  }
  # set
  setv <- function(value) {
    continue = TRUE
    if(!is.null(editn)){
      if(editn)
        editn <<- editn - 1
      else continue = FALSE
    }
    if(continue){
    ioo <- as.character(i)
    if (not.inherits(value,classi)) stop("Class of new value must be ", classi, ", the same as the original value of ", i)
    eval(parse(text = as.character(.r232)))
    assign(ioo, value, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    lockBinding(ioo, env = .pos80cbca8022ece6174797e10bb8aebf18)
    }
  }
  setl <- function(value) {
    continue = TRUE
    if(!is.null(editn)){
      if(editn)
        editn <<- editn - 1
      else continue = FALSE
    }
    if(continue){
    if (not.inherits(value,classi)) stop("Class of new value must be ", classi, ", the same as the original value of ", i)
    assign(as.character(i), value, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    }
  }

  # round
  rldd <- function(digits = 0) {
    ioo <- as.character(i)
    if (lock){
      eval(parse(text = as.character(.r232)))
    }
    bv <- round(get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18), digits)
    assign(ioo, bv, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    if (lock) lockBinding(ioo, env = .pos80cbca8022ece6174797e10bb8aebf18)
  }

  # head
  headd <- function(n = 10) {
    .l = {get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18)}
    stopifnot(inherits(.l,"data.frame")) # data must be a data frame
    print(.l[1:n,])
  }
  # tail
  taill <- function(n = 10) {
    .l = {get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18)}
    stopifnot(inherits(.l,"data.frame")) # data must be a data frame
    print(.l[(nrow(.l)-n+1):nrow(.l),])
  }

  # push
  pushl <- function(add, .df = c("row", "col")) {
    .df <- match.arg(.df)
    .l = {get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18)}
    if(inherits(.l,"data.frame")){
      if(.df == "row").l <- rbind(.l, add) else .l <- cbind(.l, add)
    }
    else .l <- c(.l, add)
    .l
  }

  # pop
  popl <- function(n = 10, .df = c("row", "col")) {
    .df <- match.arg(.df)
    .l = {get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18)}
    if(inherits(.l,"data.frame")){
      if(.df == "row").l <- .l[1:{nrow(.l)-n},] else .l <- .l[,1:{ncol(.l)-n}]
    }
    else{
      .l <- .l[-{length(.l):{length(.l) - {n-1}}}]
    }
    .l
  }

  # significant figures
  sgif <- function(digits = 0) {
    ioo <- as.character(i)
    if (lock){
      eval(parse(text = as.character(.r232)))
    }
    bv <- signif(get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18), digits)
    assign(ioo, bv, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    if (lock) lockBinding(ioo, env = .pos80cbca8022ece6174797e10bb8aebf18)
  }

  # contains
  cntsa <- function(pattern, ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE, invert = FALSE, df.col) {
    if (classi[1] %in% c("matrix", "array", "data.frame")) {
      z <- data.frame(get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18))
      res <- z[grep(
        pattern = pattern, x = z[, df.col], ignore.case = ignore.case, perl = perl, value = FALSE,
        fixed = fixed, useBytes = useBytes, invert = invert
      ), ]
    } else {
      res <- grep(
        pattern = pattern, x = get(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18), ignore.case = ignore.case, perl = perl, value = FALSE,
        fixed = fixed, useBytes = useBytes, invert = invert
      )
    }

    as.logical(length(unlist(res)))
  }
  # editable
  editn1 <- 1
  if(length(editn)){
    if (editn == 0) editn1 <- editn
  }

  # assign to variable
  for (i in .v) {
    if (exists(as.character(i), envir = .pos80cbca8022ece6174797e10bb8aebf18)) {
      stop("The variable '", i, "' already exists as a superVar. In order to redeclare it,
           you need to remove the existing object by using '", i, ".rm()'")
    }
    assign(as.character(i), value, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    if (lock) lockBinding(i, env = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[7]), classi, envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[2]), .join(rmv), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[4]), .join(cntsa), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[5]), .join(rldd), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[6]), .join(sgif), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[10]), .join(pushl), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    assign(paste0(i, els[11]), .join(popl), envir = .pos80cbca8022ece6174797e10bb8aebf18)

    if(inherits(value,"data.frame")){
      #if data frame, add the head and tail functions
      assign(paste0(i, els[8]), .join(headd), envir = .pos80cbca8022ece6174797e10bb8aebf18)
      assign(paste0(i, els[9]), .join(taill), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    }else{
      els <- els[els %nin% c(".head",".tail")]
    }

    if(editn1 != 0)
      assign(paste0(i, els[3]), .join(ifelse(lock, setv, setl)), envir = .pos80cbca8022ece6174797e10bb8aebf18)
    else els <- els[els!=".set"]
    for(k3i in els[-1])
    lockBinding(paste0(i, k3i), env = .pos80cbca8022ece6174797e10bb8aebf18)
  }
  rm(rmv, setv, setl, rldd, cntsa, sgif, .r231)
}


# will be exported in future release
# `%-%` <- function(item1,item2,return=FALSE){
#   if(!all(class(item1) == class(item2))) stop("Both variables must have the same class")
#   res <- "The variables have to be either two lists or two character vectors"
#   if(inherits(item1,"character"))
#   res <- item1[item1%nin%item2]
#   if(inherits(item1,"list"))
#   res <- mapply('-', item1, item2, SIMPLIFY = FALSE)
#   res
# }

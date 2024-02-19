#' Create and Use a super variable
#'
#' Create a variable that supercedes other variables and has various functionalities
#'
#' @param ... variable name super variable
#' @param value value of the variable
#' @param lock lock variable to change
#'
#' @return no visible return, but variable is created and stored
#'
#' @examples
#' # create a variable to store dataset that should not be altered
#' newSuperVar(mtdf, value = mtcars)
#'
#'
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

  #

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

# lockBinding("yooo", env = env1)


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
# type1(number(10))
# type3(number(12))

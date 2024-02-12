#to-do
#function to track function usage
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

#' @export

newSuperVar <- function(..., value = 1, class = character, lock = TRUE){
  message("This function is still being developed")
  .v <- as.list(substitute(args(...))[-1L])
  .spkg = new.env()
  if(!is.attached(super.)){
    attach(list(), name = super.)
  }
  super.env <- as.environment(super.)

  #i1 <- paste0(i,".",c("rm","set","reclass","sort","subset"))

  for(i in .v){
    assign(paste0(i),value, envir = super.env)
    lockBinding(paste0(i), env = super.env)
    assign(paste0(i,".rm"),eval(parse(text=paste0("function() rm(list = '",i,"', envir = super.env)"))) , envir = super.env)
    assign(paste0(i,".set"),eval(parse(text=paste0("function(b) assign('",i,"', b, envir = super.env)"))) , envir = super.env)
    assign(paste0(i,".contains"),eval(parse(text=paste0("function(b) assign('",i,"', b, envir = super.env)"))) , envir = super.env)
    assign(paste0(i,".reclass"),eval(expression({
      function(b) assign(get(i), b + 1, envir = super.env)
      })), envir = super.env)

    #attach(.spkg, name = super.)

  }

  lockEnvironment(super.)

}

#lockBinding("yooo", env = env1)

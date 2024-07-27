# to-do v1.0
# function to track function usage
type3 <-function(x)type1(x)
type1 <- function(x){
  mean(x)
  sd(x)
  tracker()
}

tracker <- function(apiId){
  getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
  getFuncName <- strsplit(getCall,"\\(")[[1]][1]
  print(getFuncName)
  ls("package:quickcode")
}



extract_comments = function (filename) {

  is_assign = function (expr)

    as.character(expr) %in% c('<-', '<<-', '=', 'assign')



  is_function = function (expr)

    is.call(expr) && is_assign(expr[[1]]) && is.call(expr[[3]]) && expr[[3]][[1]] == quote(`function`)



  source = parse(filename, keep.source = TRUE)

  functions = Filter(is_function, source)

  fun_names = as.character(lapply(functions, `[[`, 2))

  setNames(lapply(attr(functions, 'srcref'), grep,

                  pattern = '^\\s*#', value = TRUE), fun_names)

}

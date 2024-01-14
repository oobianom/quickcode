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




#' Remove elements from a data matching filter
#'
#' Shorthand to remove elements from a data frame based on filter and save as the same name
#'
#' @param . data object
#' @param remove expression for filter
#' @return data filtered out based on the expression
#' @examples
#' # this function removes rows matching the filter expression
#' data.01 <- mtcars
#' data.02 <- airquality
#'
#' #task: remove all mpg > 20
#' data.01 #data.01 data before pop
#' data_pop_filter(data.01,mpg > 15) #computes and resaves to variable
#' #note: this is different from subset(data.01,data.01$mpg > 15)
#' data.01 #modified data after pop based on filter
#'
#' #task: remove all multiple. remove all elements where Month   == 5 or Solar.R > 50
#' data.02 #data.02 data before pop
#' data_pop_filter(data.02,Month   == 5 | Solar.R > 50) #computes and resaves to variable
#' data.02 #modified data after pop based on filter
#'
#' @export
#'

data_pop_filter <- function(.,remove){
  .. <- substitute(.)
  .... <- substitute(remove)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  data <- as.data.frame(get(as.character(..), envir = parent.frame()))
  filt <- as.character(list(....))
  eval(parse(text = paste0("data = subset(data,!(",filt,"))")))
  assign(as.character(..), data, envir = parent.frame())
}


# .. <- substitute(.)
# .... <- substitute(remove)
# if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
# data <- as.data.frame(get(as.character(..), envir = parent.frame()))
# filt <- as.character(list(....))
# for(x in names(data))
#   filt <- gsub(x,paste0("data$",x),filt)
# eval(parse(text = paste0("data = data[!(",filt,"),]")))
# #alternatively: eval(parse(text = paste0("data = subset(data,!(",filt,"))")))
# assign(as.character(..), data, envir = parent.frame())

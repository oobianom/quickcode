#' Convert boolean values between formats
#'
#' Convert Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @param ds item to convert
#' @param type format to convert to, choices 1, 2 or 3

#' @details
#' Output various format of booleans into a specified format. Below are the options for the type argument.
#'
#' \strong{type}: options are as follows - \cr\cr
#'      1 - Yes/No \cr
#'      2 - TRUE/FALSE\cr
#'      3 - 1/0\cr
#'
#' @return output adhering to the format of the type provided

#' @examples
#'
#' # convert "yes" or "no" to format of TRUE or FALSE
#' as.boolean("yes",2)
#' as.boolean("no",2)
#' as.boolean("YES",2)
#' as.boolean("NO",2)
#'
#' # convert "yes" or "no" to format of 1 or 0
#' as.boolean("yes",3)
#' as.boolean("no",3)
#' as.boolean("YES",3)
#' as.boolean("NO",3)
#'
#'
#' # convert 1 to format of Yes or No
#' as.boolean(1,1)
#'
#' # convert "T" to format of Yes or No
#' as.boolean("T",1)
#'
#'
#' # convert "f" to format of TRUE or FALSE
#' as.boolean("f",2)
#'
#'
#' # convert 1 to format of TRUE or FALSE
#' as.boolean(1,2)
#'
#'
#' # convert "Y" or "y" to format of Yes or No
#' as.boolean("Y",1) #uppercase Y
#' as.boolean("y",1) #lowercase y
#'
#'
#' # convert TRUE/FALSE to format of 1 or 0
#' as.boolean(TRUE,3)
#' as.boolean(FALSE,3)
#'
#'
#' # convert TRUE/FALSE to format of Yes or No
#' as.boolean(TRUE,1)
#' as.boolean(FALSE,1)
#'
#'
#'
#' # in case of error in argument
#' # as.boolean("tr",3) #NA
#' # as.boolean("ye",3) #NA
#'
#' # vector of mixed boolean to TRUE/FALSE or 1/0
#' multv <- c(TRUE,"y","n","YES","yes",FALSE,"f","F","T","t")
#' as.boolean(multv,1) # return vector as Yes/No
#' as.boolean(multv,2) # return vector as TRUE/FALSE
#' as.boolean(multv,3) # return vector as 1/0
#'
#' @export
#'
as.boolean <- function(ds, type = 3) {
  # boolean combos
  # lowercase of entry
  .d <- tolower(ds)
  # convert to 1/0
  .d[.d %in% c("t","true","yes","y")] = 1L
  .d[.d %in% c("f","false","no","n")] = 0L
  # if type is different
  if(type == 1) gsub("^0$","No",gsub("^1$","Yes",.d)) -> .d
  else as.numeric(.d) -> .d
  if(type == 2) as.logical(.d) -> .d
  # return transformed
  .d
}






#' Convert Yes/No to Binary or Logical
#'
#' Seemlessly convert a yes or no to either a binary or logical output
#'
#' @param table data frame
#' @param fldname field name in the data frame
#' @param out output form, choices - change, append, vector
#' @param type output type, choices - bin, log
#'
#' @details
#' type - bin for binary, and log for logical
#'
#' @examples
#' usedata <- mtcars
#' usedata
#' usedata$yess = "yes"
#' usedata
#'
#'
#' yesNoBool(usedata,yess)
#' #or
#' yesNoBool(usedata,"yess")
#'
#'
#' yesNoBool(usedata,yess,"append")
#' #or
#' yesNoBool(usedata,"yess","append")
#'
#' @export
#'
yesNoBool <- function(table,fldname, out = c("change","append","vector"), type = c("bin","log")){
  if(typeof(table) != "list") stop("A data frame must be used.")
  .tt <- switch (match.arg(type), "bin" = 3, "log" = 2  )
  .wt <- table[,as.character(substitute(fldname))]
  .dt <- within(table,{
    #new__col = as.boolean(get(fldname),.tt)
    new__col = as.boolean(.wt,.tt)
  })
  switch (match.arg(out),
    "change" = {
      .dt[fldname] <- .dt$new__col
      .dt$new__col <- NULL
    },
    "vector" = {
      .dt <- .dt$new__col
    }
  )
  .dt
}

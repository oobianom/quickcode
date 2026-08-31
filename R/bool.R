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
#' # Task: convert "yes" or "no" to format of TRUE or FALSE
#' as.boolean("yes",2)
#' as.boolean("no",2)
#' as.boolean("YES",2)
#' as.boolean("NO",2)
#'
#' # Task: convert "yes" or "no" to format of 1 or 0
#' as.boolean("yes",3)
#' as.boolean("no",3)
#' as.boolean("YES",3)
#' as.boolean("NO",3)
#'
#'
#' # Task: convert 1 to format of Yes or No
#' as.boolean(1,1)
#'
#' # Task: convert "T" to format of Yes or No
#' as.boolean("T",1)
#'
#'
#' # Task: convert "f" to format of TRUE or FALSE
#' as.boolean("f",2)
#'
#'
#' # Task: convert 1 to format of TRUE or FALSE
#' as.boolean(1,2)
#'
#'
#' # Task: convert "Y" or "y" to format of Yes or No
#' as.boolean("Y",1) #uppercase Y
#' as.boolean("y",1) #lowercase y
#'
#'
#' # Task: convert TRUE/FALSE to format of 1 or 0
#' as.boolean(TRUE,3)
#' as.boolean(FALSE,3)
#'
#'
#' # Task: convert TRUE/FALSE to format of Yes or No
#' as.boolean(TRUE,1)
#' as.boolean(FALSE,1)
#'
#'
#'
#' # In case of error in argument
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
  # lowercase of entry
  .d <- tolower(ds)
  # convert to 1/0
  .d[.d %in% c("t", "true", "yes", "y")] <- 1L
  .d[.d %in% c("f", "false", "no", "n")] <- 0L
  NULL # if type is not 1:3
  switch(type,
    "1" = as.factor(gsub("^0$", "No", gsub("^1$", "Yes", .d))),
    "2" = as.logical(as.numeric(.d)),
    "3" = as.factor(as.numeric(.d))
  )
}






#' Convert Yes/No to Binary or Logical
#'
#' Seamlessly convert a yes or no to either a binary or logical output
#'
#' @param table data frame
#' @param fldname field name in the data frame
#' @param out output form, choices - change, append, vector
#' @param type output type, choices - bin, log
#' @return converted Yes/No entries into 1/0 or TRUE/FALSE
#' @details
#' type - "bin" for binary, and "log" for logical
#'
#' @examples
#' \donttest{
#' # Declare data for example
#' usedata <- data.frame(ID = 1:32)
#' usedata #view the dataset
#'
#' usedata$yess = rep(c("yes","n","no","YES","No","NO","yES","Y"),4) #create a new column
#' usedata #view the modified dataset
#'
#' # Set all yess field as standardize boolean
#' # Task: convert the "yess" column content to 1/0 or TRUE/FALSE
#' # Notice that you have add the column name with or without quotes
#' yesNoBool(usedata,yess, type="bin") #set all as binary 1/0
#' yesNoBool(usedata,"yess", type="log") #set all as logical TRUE/FALSE
#'
#'
#' # Task: By default, the 'out' argument is set to "change"
#' # means that the original data field will be
#' # replaced with the results as above
#'
#' # In this example, set the out variable to
#' # append data frame with a new column name containing the result
#'
#' yesNoBool(usedata,yess,"append")
#' #or yesNoBool(usedata,"yess","append")
#'
#' # In this example, return as vector
#' yesNoBool(usedata,yess,"vector")
#' #or yesNoBool(usedata,"yess","vector")
#'
#' # Task: Return result as logical
#' yesNoBool(usedata,"yess",type = "log")
#' }
#' @export
#'
yesNoBool <- function(table,fldname, out = c("change","append","vector"), type = c("bin","log")){
  if(typeof(table) != "list") stop("A data frame must be used.")
  .tt <- switch (match.arg(type), "bin" = 3, "log" = 2  )
  .cook <- as.character(substitute(fldname))
  switch (match.arg(out),
    "change" = {table[,.cook] <- as.boolean(table[,.cook],.tt)},
    "append" = {table$new__col <- as.boolean(table[,.cook],.tt)},
    "vector" = {table <- as.boolean(table[,.cook],.tt)}
  )
  table
}

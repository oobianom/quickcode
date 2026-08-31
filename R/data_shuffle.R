#' Shuffle a data frame just like shuffle in PHP
#'
#' Shorthand to shuffle a data frame and save
#'
#' @param . data to shuffle as data frame
#' @param which what to shuffle, rows or columns
#' @param seed apply seed if indicated for reproducibility
#' @return shuffled data frame of items store to the data frame name
#'
#' @examples
#'
#' #basic example
#' data.frame(ID=46:55,PK=c(rep("Treatment",5),rep("Placebo",5))) #before
#' data_shuffle(
#'   data.frame(ID=46:55,PK=c(rep("Treatment",5),rep("Placebo",5)))
#' ) #after shuffle row
#' data_shuffle(
#'   data.frame(ID=46:55,PK=c(rep("Treatment",5),rep("Placebo",5))),
#'   which = "cols"
#' ) #after shuffle column
#'
#'
#' # examples using object
#' df1<-data.frame(ID=46:55,PK=c(rep("Treatment",5),rep("Placebo",5)))
#'
#' #illustrate basic functionality
#' data_shuffle(df1)
#' df1 #shuffle and resaved to variable
#'
#' data.f2<-df1
#' data_shuffle(data.f2)
#' data.f2 #first output
#'
#' data.f2<-df1
#' data_shuffle(data.f2)
#' data.f2 # different output from first output top
#'
#' data.f2<-df1
#' data_shuffle(data.f2,seed = 344L)
#' data.f2 #second output
#'
#' data.f2<-df1
#' data_shuffle(data.f2,seed = 344L)
#' data.f2 #the same output as second output top
#'
#' @export
#'

data_shuffle <- function(., which = c("rows", "cols"), seed = NULL) {
  which <- match.arg(which)

  .. <- substitute(.)

  if(not.null(seed))set.seed(seed)

  if (typeof(..) == "language"){
    switch(which,
           "rows" = {
             d05 <- .[sample(nrow(.)), ]
           },
           "cols" = {
             d05 <- .[, sample(ncol(.))]
           }
    )
    return(d05)
  }

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(as.character(..), envir = parent.frame()))


  switch(which,
         "rows" = {
           data <- data[sample(nrow(data)), ]
         },
         "cols" = {
           data <- data[, sample(ncol(data))]
         }
  )
  assign(as.character(..), data, envir = parent.frame())
}

#' Combine vector to create Date, or split Date into vector
#'
#' Combine or split Date into a specified format
#'
#' @rdname date_tweaks
#'
#' @references
#' Adapted from Ecfun R package
#' @examples
#' # EXAMPLES FOR date3to1
#'
#' data0 <- data.frame(y=c(NA, -1, 2001:2009),
#' m=c(1:2, -1, NA, 13, 2, 12, 6:9),
#' d=c(0, 0:6, NA, -1, 32) )
#' head(data0)
#'
#' # combine and convert to date
#' # return as data frame
#' date3to1(data0)
#'
#' # combine and convert to date
#' # return as vector
#' date3to1(data0, as.vector = TRUE) #eg. 2004-02-04
#'
#'
#' # combine and convert to date in the format DD_MM_YYYY
#' date3to1(data0, out.format = "%d_%m_%Y") #eg. 04_02_1974
#'
#'
#' # combine and convert to date in the format MM_DD_YY
#' date3to1(data0, out.format = "%m_%d_%y") #eg. 02_04_74
#'
#' # combine and convert to date in the various date formats
#' date3to1(data0, out.format = "%B %d, %y") #eg. February 04, 74
#' date3to1(data0, out.format = "%a, %b %d, %Y") #eg. Mon, Feb 04, 1974
#' date3to1(data0, out.format = "%A, %B %d, %Y") #eg. Monday, February 04, 1974
#' date3to1(data0, out.format = "Day %j in Year %Y") #eg. Day 035 in Year 1974
#' date3to1(data0, out.format = "Week %U in %Y") #eg. Week 05 in 1974
#' date3to1(data0, out.format = "Numeric month %m in Year %Y") #eg. Numeric month 02 in Year 1974
#'
#' @param data data frame object
#' @param out.format date output format
#' @param col.YMD columns to combine for Year, Month and Day
#' @param as.vector return output as vector, or leave as data frame
#' @return date derived from combining values from three columns of a data frame
#'
#' @section Note:
#' \strong{DATE FORMATS IN R}\cr
#' \tabular{rrrrr}{
#' \strong{Date Specification}   \tab \tab \strong{Description}          \tab \tab  \strong{Example} \cr
#' \%a  \tab \tab Abbreviated weekday             \tab \tab Sun, Thu \cr
#' \%A  \tab \tab Full weekday                    \tab \tab Sunday \cr
#' \%b  \tab \tab Abbreviated month               \tab \tab May, Jul \cr
#' \%B  \tab \tab Full month                      \tab \tab March, July \cr
#' \%d  \tab \tab Day of the month                \tab \tab 27, 07 \cr
#' \%j  \tab \tab Day of the year                 \tab \tab 148, 188 \cr
#' \%m  \tab \tab Month                           \tab \tab 05, 07 \cr
#' \%U  \tab \tab Week, with Sunday as first day  \tab \tab 22, 27 \cr
#' \%w  \tab \tab Weekday, Sunday is 0            \tab \tab 0, 4 \cr
#' \%W  \tab \tab Week, with Monday as first day  \tab \tab 21, 27 \cr
#' \%x  \tab \tab Date, locale-specific           \tab \tab \cr
#' \%y  \tab \tab Year without century            \tab \tab 84, 05 \cr
#' \%Y  \tab \tab Year with century               \tab \tab 1984, 2005 \cr
#' \%C  \tab \tab Century                         \tab \tab 19, 20 \cr
#' \%D  \tab \tab Date formatted \%m/\%d/\%y      \tab \tab 07/17/23 \cr
#' \%u  \tab \tab Weekday, Monday is 1            \tab \tab 7, 4 \cr
#' }
#'
#' @details
#' \strong{NOTE for date3to1}\cr
#'
#' The three input columns corresponding to "Year Month Day" must be numeric values. \cr\cr
#' For example, Do not provide the month variable as non-numeric such as "Mar", "Jul", or "Jan". \cr\cr
#' If the values of the columns are non-numeric, the results will return an "NA" in the output.date column.
#'
#' @export

date3to1 <-
  function(data,
           out.format = "%Y-%m-%d",
           col.YMD = 1:3,
           as.vector = FALSE
  ){

    stopifnot(inherits(data,"data.frame")) # data must be a data frame
    if(has.error(data[,col.YMD]))
      stop("The columns for Year Month Day (col.YMD) does not exist in the dataset")
    or.names<- names(data)
    names(data)[col.YMD] = c("..yyyy..","..mm..","..dd..")

    b.out <- within(data,{
      output.date = as.POSIXct(paste0(..yyyy..,"-",..mm..,"-",..dd..),format="%Y-%m-%d")
      if(out.format!="%Y-%m-%d")
        output.date = format(output.date, out.format)
    })

    if(as.vector){
      as.character(b.out$output.date)
    }else{
      names(b.out) <- c(or.names,'output.date')
      b.out
    }
  }





#'
#' @examples
#'
#'
#'
#'
#' # EXAMPLES FOR date1to3
#'
#' data1 <- data.frame(Full.Dates =
#'                       c("2023-02-14",NA,NA,
#'                         "2002-12-04","1974-08-04",
#'                         "2008-11-10"))
#' head(data1)
#'
#' # split date with default settings
#' # return as data frame with columns
#' # for day(d), month(m) and year(Y)
#' date1to3(data1)
#'
#'
#' # split date in the format and only return year in YYYY
#' date1to3(data1, out.cols = "%Y") #eg. 2002, 2023
#'
#'
#' # split date in the format and only return month in m
#' date1to3(data1, out.cols = "%m") #eg. 02, 12, 08
#'
#' # split date in the format and return multiple date formats colums
#' date1to3(data1, out.cols = c("%B","%d") )
#' date1to3(data1, out.cols = c("%a","%b","%y") )
#' date1to3(data1, out.cols = c("%A","%B","%Y","%y") )
#' date1to3(data1, out.cols = c("%j","%Y","%y","%m") )
#' date1to3(data1, out.cols = c("%U","%Y","%y","%x") )
#' date1to3(data1, out.cols = c("%m","%Y","%y","%C") )
#'
#' @param data data frame object
#' @param in.format date input format
#' @param date.col numeric value of column within the dataset that contains the dates
#' @param out.cols cols to of date items to split. Make sure to conform to date formats. See "NOTE" section
#' for date formats
#' @rdname date_tweaks
#' @export
date1to3 <-
  function(data,
           in.format = "%Y-%m-%d",
           date.col = 1,
           out.cols = c("%Y", "%m", "%d")
  ){

    stopifnot(inherits(data,"data.frame"), length(out.cols) > 0) # data must be a data frame
    if(has.error(data[,date.col]))
      stop("The columns for Year Month Day (col.YMD) does not exist in the dataset")

    .prevn = names(data)[date.col]
    names(data)[date.col] = "...dMywhole_"

    b.out <- within(data,{
      output.date = as.POSIXct(...dMywhole_,format=in.format)
      for(iui in rev(out.cols))
        assign(paste0(".date_",iui),format(output.date,iui))
      rm(iui,output.date)
    })

    names(b.out)[date.col] = .prevn
    b.out
  }













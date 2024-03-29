#' Combine vectors to create Date, or split Date into vectors
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
#' @note
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
#' @export

date3to1 <-
  function(data,
           out.format = "%Y-%m-%d",
           col.YMD = 1:3,
           as.vector = FALSE
  ){

  stopifnot("data.frame" %in% class(data)) # data must be a data frame
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




#' Split date into columns of strings
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

  stopifnot("data.frame" %in% class(data), length(out.cols) > 0) # data must be a data frame
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


#' Switch the index of two rows in a data set
#'
#' Allows the user to choose precisely which two rows they want to swap places,
#' while optionally preventing some columns from being altered in the process.
#' Excluded columns within the rows act as anchors that are immune from the switching operation
#' on the selected rows.
#'
#' @param data dataset object
#' @param row1 numeric. the first row number
#' @param row2 numeric. the second row number
#' @param keep.cols numeric or character. column number or name to keep
#' @examples
#'
#' # Example using mtcars
#' data100 <- mtcars
#'
#' head(data100) # preview overall data
#'
#' # task 1: basic result of switching rows 5 and 6
#' head(switch_rows(data100, 5, 6))
#'
#' # task 2: switch rows, but retain some columns
#' data100[5:6,7:10] # preview the portion that is to be changed
#'
#' # lets switch 2 rows, but keep content of columns 7, 8, 9 10 within the changed rows
#' res1 <- switch_rows(data100, row1 = 5, row2 = 6, keep.cols = 7:10) # use column numbers
#' res1[5:6,] # check result, pay attention to columns 7, 8,9 and 10 as well
#' res2 <- switch_rows(data100, row1 = 5, row2 = 6, keep.cols = c("qsec","vs","am","gear")) # use column names
#' res2[5:6,] # check result, pay attention to columns "qsec",vs","am","gear" as well
#'
#' @export
switch_rows <- function(data, row1, row2, keep.cols = NULL) {
  # check
  stopifnot(row1 != row2)

  # update row names
  rownames(data)[c(row1, row2)] <- rownames(data)[c(row2, row1)]
  .x2 <- data[row2, ]

  # account for keep.cols
  if (not.null(keep.cols)) {
    if (class(keep.cols) == "character")
      keep.cols <- which(names(data) %in% keep.cols)
    .x2[, keep.cols] <- data[row1, keep.cols]
    data[row1, keep.cols] <- data[row2, keep.cols]
  }
  # switch rows
  data[row2, ] <- data[row1, ]
  data[row1, ] <- .x2

  # return output
  data
}


#' Switch the index of two columns in a data set
#'
#' Allows the user to choose precisely which two columns they want to swap places,
#' while optionally preventing some rows within the columns from being altered in the process.
#' Excluded rows within the columns act as anchors that are immune from the switching operation
#' on the selected columns.
#'
#' @param data dataset object
#' @param col1 numeric or character the first column name or number
#' @param col2 numeric or character the second column name or number
#' @param keep.rows numeric. row number to keep
#' @examples
#'
#' # Example using mtcars
#' data101 <- mtcars
#'
#' head(data101) # preview overall data
#'
#' # task 1: basic result of switching columns 5 and 6
#' head(switch_cols(data101, 5, 6))
#'
#' # task 1: basic result of switching columns number 5 and name "gear"
#' head(switch_cols(data101, 5, "gear"))
#'
#' # task 1: basic result of switching columns "qsec" and "carb"
#' head(switch_cols(data101, "qsec", "carb"))
#'
#'
#' # task 2: switch columns, but retain some rows with the switched columns
#'
#'
#' # lets exchange some columns, but keep content of row 4, 5 intact
#' data101[1:6,5:6] # preview the portion that is to be changed
#' res1 <- switch_cols(data101, col1 = 5, col2 = 6, keep.rows = 4:5) # use column numbers
#' res1[1:6,5:6] # check result, pay attention to rows 4, 5 of columns 5, 6 as well
#'
#' data101[1:6,6:11] # preview the portion that is to be changed
#' res2 <- switch_cols(data101, col1 = "qsec", col2 = "carb", keep.rows = c(1,2,3)) # keep 1, 2, 3
#' res2[1:6,6:11] # check result
#'
#' @export
switch_cols <- function(data, col1, col2, keep.rows = NULL) {
  # check
  stopifnot(col1 != col2, class(keep.rows) != "character")

  # update column names
  if (class(col1) == "character") col1 <- which(names(data) %in% col1)
  if (class(col2) == "character") col2 <- which(names(data) %in% col2)

  colnames(data)[c(col1, col2)] <- colnames(data)[c(col2, col1)]

  .x2 <- data[,col2, drop=FALSE]

  # account for keep.rows
  if (not.null(keep.rows)) {
    .x2[keep.rows, ] <- data[keep.rows,col1]
    data[keep.rows,col1] <- data[keep.rows,col2]
  }
  # switch columns
  data[, col2] <- data[, col1, drop=FALSE]
  data[, col1] <- .x2

  # return output
  data
}






#' Check if a call or expression produces errors
#'
#' Whether a function or series of calls results in error
#'
#' @param ... the expression or function calls
#' @note
#' For more information, check: https://rpkg.net/package/quickcode
#'
#' @examples
#' # this should not produce error
#' # so the function result should be FALSE
#' has.error({
#'   x = 8
#'   y = number(10)
#'   res = x + y
#' })
#'
#' # this should produce the following error
#' # Error in x + y : non-numeric argument to binary operator
#' # so the function result should be TRUE
#' has.error({
#'   x = 8
#'   y = "random"
#'   res = x + y
#' })
#'
#' # this should result in error because
#' # the dataset does not contain a "rpkg.net" column
#' # the result should be TRUE
#' df1 = mtcars
#' has.error(df1[,"rpkg.net"])
#'
#' @return boolean value to indicate if the expression produces errors
#' @export

has.error <- function(...) {
  .error <- FALSE
  tryCatch(...,
    error = function(e) {
      .error <<- TRUE
    }
  )
  .error
}






# print all the environment object and sizes and connections to each other
#' Get all the environment objects and their sizes
#'
#' Retrieve the size contribution of all the available objects in the environment
#'
#' @param envir the environment to retrieve objects from
#' @return a dataframe of all the variables within the environment
#' @examples
#' # Get a data frame of all environment objects and their size
#' summarize.envobj()
#' @export
summarize.envobj <- function(envir = parent.frame()){
# version 2
  # within(data.frame(size.bytes = unlist(
  #   sapply(ls(all.names = TRUE, envir = envir), function(n) {
  #     object.size(get(n))
  #   }, simplify = FALSE)
  # )),{
  #   size.kbytes = size.bytes/1000
  # })
 envs = ls(all.names = TRUE, envir = envir)
 envs.size = c()
  for(b in  envs) vector_push(envs.size, eval(parse(text = paste0("object.size(",b,")"))))
 within(data.frame(objects = c(envs,"TOTAL"), size.bytes = c(envs.size,sum(envs.size))),{
   size.kbytes = size.bytes/1000
   size.mbytes = size.kbytes/1000
 })
}









#https://www.stat.umn.edu/geyer/old/5101/rlook.html

# Distribution	Functions
# Beta	pbeta	qbeta	dbeta	rbeta
# Binomial	pbinom	qbinom	dbinom	rbinom
# Cauchy	pcauchy	qcauchy	dcauchy	rcauchy
# Chi-Square	pchisq	qchisq	dchisq	rchisq
# Exponential	pexp	qexp	dexp	rexp
# F	pf	qf	df	rf
# Gamma	pgamma	qgamma	dgamma	rgamma
# Geometric	pgeom	qgeom	dgeom	rgeom
# Hypergeometric	phyper	qhyper	dhyper	rhyper
# Logistic	plogis	qlogis	dlogis	rlogis
# Log Normal	plnorm	qlnorm	dlnorm	rlnorm
# Negative Binomial	pnbinom	qnbinom	dnbinom	rnbinom
# Normal	pnorm	qnorm	dnorm	rnorm
# Poisson	ppois	qpois	dpois	rpois
# Student t	pt	qt	dt	rt
# Studentized Range	ptukey	qtukey	dtukey	rtukey
# Uniform	punif	qunif	dunif	runif
# Weibull	pweibull	qweibull	dweibull	rweibull
# Wilcoxon Rank Sum Statistic	pwilcox	qwilcox	dwilcox	rwilcox
# Wilcoxon Signed Rank Statistic	psignrank	qsignrank	dsignrank	rsignrank


#check if data is log normal
#' Check if a data fits a Normal or LogNormal or Uniform or Poisson or Gamma or Logistic distribution
#'
#' @description
#' Check whether a vector of data contains values that fit a distribution
#' @details
#' This function takes a numeric vector as its input. This vector contains the dataset that will be analyzed.
#' \cr\cr
#' \strong{For Normal and LogNormal:}\cr
#' This function first performs a Shapiro-Wilk test on the data to check if it comes from a normal distribution. The Shapiro-Wilk test checks if sample data came from a normal distribution. It returns a p-value, with a higher p-value indicating stronger evidence that the data is normally distributed.
#' If the p-value from the Shapiro-Wilk test is above a predefined threshold (such as 0.05), the data is considered normally distributed. In this case, the function returns "normal".
#' If the data fails the Shapiro-Wilk normality test, the function then checks if the data fits a lognormal distribution. It transforms the data by taking the natural logarithm of each value. It then runs the Shapiro-Wilk test on the transformed data.
#' If the p-value from this test on the logged data is above the threshold, the function returns "lognormal", indicating the untransformed data fits a lognormal distribution after logging.
#' If the data fails both the normal and logged Shapiro-Wilk tests, the function returns "neither", indicating the data does not appear to come from a normal or lognormal distribution based on the tests.
#' @note
#' is.normal and is.lognormal uses the "Shapiro-Wilk test" from the utils package
#' @rdname distribution_check
#' @param values vector of values
#' @param sig significance level to test p-value against
#' @return boolean value if lognormal distributed
#' @examples
#' # EXAMPLE FOR is.lognormal
#'
#' Set the seed for reproducibility
#' set.seed(1989)
#'
#' # Generate 1000 data points from a lognormal distribution with mean 0 and standard deviation 1
#' lognormal_data <- rlnorm(n = 1000, meanlog = 0, sdlog = 1)
#' lognormal_data1 <- rlnorm(n = 100, meanlog = 0, sdlog = 1)
#' lognormal_data2 <- rlnorm(n = 10, meanlog = 0, sdlog = 1)
#' lognormal_data3 <- rlnorm(n = 5, meanlog = 0, sdlog = 1)
#'
#' # Plot the distribution of the data using a histogram
#' hist(lognormal_data, main = "Lognormal Distribution with Mean 0 and Standard Deviation 1", xlab = "Data Values", col = "lightblue")
#'
#' # Add a density curve to the histogram
#' lines(density(lognormal_data), col = "red")
#'
#' # Test if the data is lognormal
#' is.lognormal(lognormal_data)
#' is.lognormal(lognormal_data1)
#' is.lognormal(lognormal_data2)
#' is.lognormal(lognormal_data3)
#'
#' @export
is.lognormal <- function(values,sig = 0.5){
  (stats::shapiro.test(log(values)))$p.value >= 0.05
}

#' @rdname distribution_check
#' @param values vector of values
#' @param sig significance level to test p-value against
#' @return boolean value if normal distributed
#'
#' @examples
#' # EXAMPLE FOR is.normal
#'
#' # Set the seed for reproducibility
#' set.seed(1989)
#'
#' # Generate 1000 data points from a normal distribution with mean 0 and standard deviation 1
#' normal_data <- rnorm(n = 1000, mean = 0, sd = 1)
#' normal_data1 <- rnorm(n = 500, mean = 0, sd = 1)
#' normal_data2 <- rnorm(n = 100, mean = 0, sd = 1)
#' normal_data3 <- rnorm(n = 10, mean = 0, sd = 1)
#'
#' # Plot the distribution of the data using a histogram
#' hist(normal_data, main = "Normal Distribution with Mean 0 and Standard Deviation 1", xlab = "Data Values", col = "lightblue")
#'
#' # Add a density curve to the histogram
#' lines(density(normal_data), col = "red")
#'
#' # Test if the data is normal
#' is.normal(normal_data)
#' is.normal(normal_data1)
#' is.normal(normal_data2)
#' is.normal(normal_data3)
#'
#' @export
is.normal <- function(values,sig = 0.5){
  {stats::shapiro.test(values)}$p.value >= sig
}


#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.uniform uses the "Kolmogorov-Smirnov test"
#'
#' @param sig significance level to test p-value against
#' @return boolean value if uniform distributed
#' @export
is.uniform <- function(values,sig = 0.5){
  {stats::ks.test(values)}$p.value >= sig
}


#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.poisson uses the "pchisq"
#'
#' @param sig significance level to test p-value against
#' @return boolean value if poisson distributed
#' @export
is.poisson <-function(values,sig= 0.5){
  # Perform chi-squared test to check Poisson distribution
  obs <- table(values)
  exp <- length(values)*mean(values)
  chisq <- sum((obs-exp)^2/exp)
  # Compare test statistic to chi-squared distribution
  {1-stats::pchisq(chisq, length(obs)-1)}  >= sig
}



#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.gamma uses the "Anderson-Darling test"
#'
#' @param sig significance level to test p-value against
#' @return boolean value if gamma distributed
#'
#' @examples
#'
#' EXAMPLE for is.gamma
#'
#' Example gamma data
#' set.seed(5434)
#' n = 1000
#' x <- stats::rgamma(n,5, 2)
#' is.gamma(x) # check if it is gamma distribution
#' @export
is.gamma <- function(values,sig = 0.5){
  warning("Function under development")
  breaks <- seq(min(values), max(values), length.out = 20)
  {stats::chisq.test(table(cut(value, breaks = breaks)))}$p.value >= sig
}

#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.logistic use the "Kolmogorov-Smirnov test"
#'
#' @param sig significance level to test p-value against
#' @return boolean value if logistic distributed
#' @examples
#' set.seed(231)
#' n <- 1000
#' location <- 0
#' scale <- 1
#' xlogi <- sim.logistic(n, location, scale)# Simulate logistic values
#' hist(xlogi, prob=TRUE, main="Plot of simulated logistic")# Plot histogram
#'
#' is.logistic(xlogi) # test if it is logistic distribution
#' @export
is.logistic <- function(values,sig= 0.5) {
  warning("Function under development")
  {ks.test(values, "plogis")}$p.value >= sig
}



#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.weibull use the "Kolmogorov-Smirnov test"
#' @examples
#' x <- rweibull(1000, shape = 2, scale = 1)
#' is.weibull(x)
#' @param sig significance level to test p-value against
#' @return boolean value if logistic distributed
#' @export
is.weibull <- function(values,sig= 0.5, shape = 2, scale = 1) {
  {stats::ks.test(values,"pweibull",shape,scale)}$p.value >= sig
}


#' Generate logistic random values
#'
#' generates random values from a logistic distribution. It takes in the number of values to generate (n) and optional location and scale parameters.
#'
#' @param n integer number of random values to generate
#' @param min minimum value
#' @param max maximum value
#' @param location numeric location parameter of the logistic distribution (default is 0)
#' @param scale numeric scale parameter of the logistic distribution (default is 1)
#'
#' @details
#'
#' Details:\cr\cr
#'
#' - The function first generates uniform [0,1] random values using runif(n) \cr
#' - It then transforms these values using the quantile function qlogis() to map them to the logistic CDF\cr
#' - stats::qlogis() takes the uniform values and the location and scale parameters\cr
#' @return random values that follow a logistic distribution with the given parameters\cr
#'
#' @export
sim.logistic <- function(n, location = 0, scale = 1, min = 0, max = 1) {
  stats::qlogis(runif(n,min,max), location, scale)
}




# easyrleft <-
# function (string, char)
#   substr(string, 1, char)
#
# easyrmid <-
# function (string, start, nchars)
#   substr(string, start, start + nchars - 1)
#
# easyrright<-
# function (string, char)
#   substr(string, nchar(string) - (char - 1), nchar(string))

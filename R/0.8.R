#' @export
date1to3 <- function(data, in.format = "%Y-%m-%d"){
  if (class(data) != "Date") {
    stop("class(data) is not an object of class Date")
  }
  str = as.character(data)
  yr1 = easyrleft(str, 4)
  mth1 = easyrmid(str, 6, 2)
  day1 = easyrright(str, 2)
  data.frame(yr1, mth1, day1)
}

#' Combine vectors to form date
#'
#' Adapted from Ecfun R package
#' @examples
#'
#' df1 <- data.frame(y=c(NA, -1, 1971:1979),
#' m=c(1:2, -1, NA, 13, 2, 12, 6:9),
#' d=c(0, 0:6, NA, -1, 32) )
#' head(df)
#'
#' # combine and convert to date
#' # return as data frame
#' date3to1(df1)
#'
#' # combine and convert to date
#' # return as vector
#' date3to1(data, as.vector = TRUE)
#'
#'
#' # combine and convert to date in the format DD_MM_YYYY
#' date3to1(data, out.format = "%d_%m_%Y") #eg. 04_02_1974
#'
#'
#' # combine and convert to date in the format MM_DD_YY
#' date3to1(data, out.format = "%m_%d_%y") #eg. 02_04_74
#'
#' # combine and convert to date in the various date formats
#' date3to1(data, out.format = "%B %d, %y") #eg. February 04, 74
#' date3to1(data, out.format = "%a, %b %d, %Y") #eg. Mon, Feb 04, 1974
#' date3to1(data, out.format = "%A, %B %d, %Y") #eg. Monday, February 04, 1974
#' date3to1(data, out.format = "Day %j in Year %Y") #eg. Day 035 in Year 1974
#' date3to1(data, out.format = "Week %U in %Y") #eg. Week 05 in 1974
#' date3to1(data, out.format = "Numeric month %m in Year %Y") #eg. Numeric month 02 in Year 1974
#'
#' @param data data frame object
#' @param out.format date output format
#' @param col.YMD columns to combine for Year, Month and Day
#' @param as.vector return output as vector, or leave as data frame
#' @return date derived from combining values from three columns of a data frame
#'
#' @export

date3to1 <- function(data, out.format = "%Y-%m-%d", col.YMD = 1:3, as.vector = FALSE){
  stopifnot("data.frame" %in% class(data)) # data must be a data frame
  if(has.error(you[,col.YMD]))
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

#' @export
switch_rows <- function(data,row1,row2,keep){
  warning("Function under development")
  .x2 <- data[row2,]
  data[row2,] <- data[row1,]
  data[row1,] <- .x2
  data
}


easyrleft <-
function (string, char)
  substr(string, 1, char)

easyrmid <-
function (string, start, nchars)
  substr(string, start, start + nchars - 1)

easyrright<-
function (string, char)
  substr(string, nchar(string) - (char - 1), nchar(string))


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
summarize.envObj <- function(){

}

#function execution time

fun.time <- function(...){
  .m <- Sys.time()
  local(...)
  Sys.time() - .m
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
#' Check if a data fits a Normal or LogNormal or Uniform or Poisson or Gamma distribution
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
#' is.normal and is.lognormal uses the "Shapiro-Wilk test"
#' @rdname distribution_check
#' @param values vector of values
#' @param sig significance level to test p-value against
#' @return boolean value if lognormal distributed
#' @export
is.lognormal <- function(values,sig = 0.5){
  (stats::shapiro.test(log(values)))$p.value >= 0.05
}

#' @rdname distribution_check
#' @param values vector of values
#' @param sig significance level to test p-value against
#' @return boolean value if normal distributed
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
#' is.gamma should use the "Anderson-Darling test"
#'
#' @param sig significance level to test p-value against
#' @return boolean value if gamma distributed
#'
#' @examples
#'
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





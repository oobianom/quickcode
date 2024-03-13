#' @export
date1to3 <- function(data){
  if (class(data) != "Date") { stop("class(data) is not an object of class Date")
  }
  str = as.character(data)
  yr1 = easyrleft(str, 4)
  mth1 = easyrmid(str, 6, 2)
  day1 = easyrright(str, 2)
  data.frame(yr1, mth1, day1)
}
#' @export
date3to1 <- function(dat,sep="-"){
  paste(dat[,1],dat[,2],dat[,3],collapse = sep)
}

#' @export
switch_rows <- function(data,row1,row2,keep){
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
#' Check if a vector of values are Normal or Lognormal distributed
#'
#' @description
#' Check whether a vector of data contains values that fit a lognormal or normal distribution
#' @details
#' This function takes a numeric vector as its input. This vector contains the dataset that will be analyzed.
#' \cr
#' This function first performs a Shapiro-Wilk test on the data to check if it comes from a normal distribution. The Shapiro-Wilk test checks if sample data came from a normal distribution. It returns a p-value, with a higher p-value indicating stronger evidence that the data is normally distributed.
#' \cr
#' If the p-value from the Shapiro-Wilk test is above a predefined threshold (such as 0.05), the data is considered normally distributed. In this case, the function returns "normal".
#' \cr
#' If the data fails the Shapiro-Wilk normality test, the function then checks if the data fits a lognormal distribution. It transforms the data by taking the natural logarithm of each value. It then runs the Shapiro-Wilk test on the transformed data.
#' \cr
#' If the p-value from this test on the logged data is above the threshold, the function returns "lognormal", indicating the untransformed data fits a lognormal distribution after logging.
#' \cr
#' If the data fails both the normal and logged Shapiro-Wilk tests, the function returns "neither", indicating the data does not appear to come from a normal or lognormal distribution based on the tests.
#'
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
  (stats::shapiro.test(values))$p.value >= sig
}

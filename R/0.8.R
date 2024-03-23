#' @export
date1to3 <- function(data){
  if (class(data) != "Date") {
    stop("class(data) is not an object of class Date")
  }
  str = as.character(data)
  yr1 = easyrleft(str, 4)
  mth1 = easyrmid(str, 6, 2)
  day1 = easyrright(str, 2)
  data.frame(yr1, mth1, day1)
}
#'
#' Adapted from Ecfun R package
#'
#' @export
date3to1 <- function(data, default='Start'){
  nc <- ncol(data)
  if(is.null(nc)){
    stop('data is not a data.frame')
  }
  if(nc != 3){
    stop('ncol(data) = ', nc, ' != 3')
  }
  nchd <- nchar(default)
  if(nchd<1){
    stop('nchar(default) < 1:  erroneous call')
  }
  def1 <- toupper(substring(default, 1, 1))
  defStart <- (def1 == 'S')
  defSt1 <- (1+defStart)
  Dt <- as.list(data)
  YrNA <- (is.na(Dt[[1]]) | (Dt[[1]]<1))
  #  3.2.  Month <1 or >12
  MoNA <- which(is.na(Dt[[2]]) |
                  (Dt[[2]]<1) | (Dt[[2]]>12))
  Dt[[2]][MoNA] <- c(12, 1)[defSt1]
  Mo1 <- Dt[[2]]+1
  Mo1[Mo1>12] <- 1
  YM1ch <- paste(Dt[[1]], Mo1, "01", sep='-')
  YM1ch[YrNA] <- NA
  YMend <- (as.Date(YM1ch)-1)
  daysofmonth <- as.numeric(substring(YMend, 9, 10))
  dayout <- which(is.na(Dt[[3]]) |
                    (Dt[[3]]<1) | (daysofmonth < Dt[[3]]))
  if(defStart){
    Dt[[3]][MoNA] <- 1
    Dt[[3]][dayout] <- 1
  } else {
    Dt[[3]][MoNA] <- daysofmonth[MoNA]
    Dt[[3]][dayout] <- daysofmonth[dayout]
  }
  Dt$sep <- "-"
  Dte <- do.call(paste, Dt)
  Dte[YrNA] <- NA
  msng <- YrNA
  msng[MoNA] <- TRUE
  msng[dayout] <- TRUE
  DTE <- as.Date(Dte)
  if(any(msng)){
    attr(DTE, 'missing') <- which(msng)
  }
  DTE
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
#' @export
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





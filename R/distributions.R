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


# check if data is log normal
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
#' @param alpha significance level to test p-value against
#' @return boolean value if lognormal distributed
#' @examples
#' # EXAMPLE FOR is.lognormal
#'
#' # Set the seed for reproducibility
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
is.lognormal <- function(values,alpha = 0.5){
  (stats::shapiro.test(log(values)))$p.value >= alpha
}

#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
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
is.normal <- function(values,alpha = 0.5){
  {stats::shapiro.test(values)}$p.value >= alpha
}


#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.uniform uses the "Kolmogorov-Smirnov test"
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if uniform distributed
#'
#' @examples
#' # EXAMPLES for is.uniform
#'
#' set.seed(323)
#' unifdata <- runif(10000,min=0,max=3)
#'
#' is.uniform(unifdata)
#'
#' @export
is.uniform <- function(values,alpha = 0.5){
  {stats::ks.test(values, "punif", min(values), max(values))}$p.value >= alpha
}


#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.poisson uses the "Chi-squared test"
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if poisson distributed
#' @examples
#'
#' # EXAMPLE for is.poisson
#' set.seed(1989)
#' data.poisson <- rpois(1000, lambda = 5) #poisson data
#' data.normal <- runif(1000, min = 5, max = 10) #uniform data
#' is.poisson(data.poisson) # should be TRUE
#' is.poisson(data.normal) # should be FALSE
#'
#' @export
is.poisson <-function(values,alpha = 0.5){
  {suppressWarnings(stats::chisq.test(table(values)))}$p.value < alpha
}



#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.gamma uses the "Anderson-Darling test" or "Kolmogorov-Smirnov test"
#' @param alpha significance level to test p-value against
#' @return boolean value if gamma distributed
#'
#' @examples
#'
#' # EXAMPLE for is.gamma
#'
#' set.seed(5434)
#' data.gamma <- stats::rgamma(1000,shape = 5, rate = 2) # gamma data
#' data.norm = runif(1000,min = 0, max = 1) # not gamma data
#'
#' is.gamma(data.gamma) # check if it is gamma distribution
#' is.gamma(data.norm) # check if it is gamma distribution
#'
#' @export
is.gamma <- function(values,alpha = 0.5){
  .sr <- fitdistrplus::fitdist(values, "gamma")
  shape <- .sr$estimate['shape']
  rate <- .sr$estimate['rate']
  {stats::ks.test(values, "pgamma", shape = shape, rate = rate)}$p.value >= alpha
}

#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.logistic use the "Kolmogorov-Smirnov test"
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @examples
#'
#' # EXAMPLE for is.logistic
#'
#' set.seed(231)
#' n <- 1000
#' location <- 4
#' scale <- 2
#' xlogi <- sim.logistic(n, location, scale)# Simulate logistic values
#' hist(xlogi, prob=TRUE, main="Plot of simulated logistic")# Plot histogram
#'
#' data.normal <- runif(1000) #uniform ditribution for negative test
#'
#' is.logistic(xlogi) # test if it is logistic distribution
#' is.logistic(data.normal)
#'
#' @export
is.logistic <- function(values,alpha = 0.5) {
  .sr <- fitdistrplus::fitdist(values, "logis")
  location <- .sr$estimate['location']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values, "plogis",
                  location = location,
                  scale = scale)}$p.value >= alpha
}



#' @rdname distribution_check
#' @param values vector of values
#' @note
#' is.weibull use the "Kolmogorov-Smirnov test"
#' @examples
#' data.weibull <- rweibull(1000, shape = 4, scale = 2) # weibull data
#' data.norm = runif(1000, min = 0, max = 1) # not weibull data
#'
#' is.weibull(data.weibull) #should return TRUE
#' is.weibull(data.norm) #should return FALSE
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @export
is.weibull <- function(values,alpha = 0.5) {
  .sr <- fitdistrplus::fitdist(values, "weibull")
  shape <- .sr$estimate['shape']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values,"pweibull",shape = shape,scale = scale)}$p.value >= alpha
}












# Future functions
# checkDistribution <- function(data,alpha,...){
#   # data.frame(Distributions = c(
#   #   "Beta",
#   #   "Binomial",
#   #   "Cauchy",
#   #   "Chi-Square",
#   #   "Exponential",
#   #   "F",
#   #   "Gamma",
#   #   "Geometric",
#   #   "Hypergeometric",
#   #   "Logistic",
#   #   "Log Normal",
#   #   "Negative Binomial",
#   #   "Normal",
#   #   "Poisson",
#   #   "Student",
#   #   "Studentized Range",
#   #   "Uniform",
#   #   "Weibull",
#   #   "Wilcoxon Rank Sum Statistic",
#   #   "Wilcoxon Signed Rank Statistic"
#   # ))
#
#
#
#   if (missing(distr))
#     stop("You must provide a distribution name")
#   distr <- match.arg(distr, c("norm", "lnorm", "gamma", "weibull",
#                               "exp", "cauchy", "logis", "rayleigh", "hyper", "beta",
#                               "llogis", "frechet", "pareto", "burr", "chi", "chisq",
#                               "f", "t", "binom", "geom", "pois", "nbinom"))
#   if (distr == "norm")
#     return(fitdist_norm(data, method, start, ...))
#   if (distr == "lnorm")
#     return(fitdist_lnorm(data, method, start, ...))
#   if (distr == "gamma")
#     return(fitdist_gamma(data, method, start, ...))
#   if (distr == "weibull")
#     return(fitdist_weibull(data, method, start, ...))
#   if (distr == "exp")
#     return(fitdist_exp(data, method, start, ...))
#   if (distr == "cauchy")
#     return(fitdist_cauchy(data, method, start, ...))
#   if (distr == "logis")
#     return(fitdist_logis(data, method, start, ...))
#   if (distr == "rayleigh")
#     return(fitdist_rayleigh(data, method, start, ...))
#   if (distr == "hyper")
#     return(fitdist_hyper(data, method, start, ...))
#   if (distr == "beta")
#     return(fitdist_beta(data, method, start, ...))
#   if (distr == "llogis")
#     return(fitdist_llogis(data, method, start, ...))
#   if (distr == "frechet")
#     return(fitdist_frechet(data, method, start, ...))
#   if (distr == "pareto")
#     return(fitdist_pareto(data, method, start, ...))
#   if (distr == "burr")
#     return(fitdist_burr(data, method, start, ...))
#   if (distr == "chi")
#     return(fitdist_chi(data, method, start, ...))
#   if (distr == "chisq")
#     return(fitdist_chisq(data, method, start, ...))
#   if (distr == "f")
#     return(fitdist_f(data, method, start, ...))
#   if (distr == "t")
#     return(fitdist_t(data, method, start, ...))
#   if (distr == "binom")
#     return(fitdist_binom(data, method, start, ...))
#   if (distr == "geom")
#     return(fitdist_geom(data, method, start, ...))
#   if (distr == "pois")
#     return(fitdist_pois(data, method, start, ...))
#   if (distr == "nbinom")
#     return(fitdist_nbinom(data, method, start, ...))
#
# }




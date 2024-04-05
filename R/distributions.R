#' Check if a data fits a Normal or LogNormal or Uniform or Poisson or Gamma or Logistic distribution
#'
#' @description
#' Check whether a vector of data contains values that fit a distribution
#' @details
#' This function takes a numeric vector as its input. This vector contains the dataset that will be analyzed.
#' \cr\cr
#' \strong{For Normal and LogNormal:}\cr\cr
#' - Method 1: we perform the Shapiro-Wilk test on the (log-transformed) data to test for normality.
#' The null hypothesis of the Shapiro-Wilk test is that the data are normally distributed.
#' If the p-value is greater than the chosen significance level (typically 0.05),
#' we fail to reject the null hypothesis, indicating that the data may follow a log-normal distribution.\cr\cr
#' - Method 2: we perform the Kolmogorov-Smirnov test on the log-transformed data, comparing it to a normal distribution with the same mean and standard deviation. Again, if the p-value is greater than the chosen significance level, it suggests that the data may follow a log-normal distribution.
#' These tests provide a statistical assessment of whether your data follows a log-normal distribution.
#'
#'
#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @param method method for calculation, where 1 = Shapiro-Wilk test and 2 = Kolmogorov-Smirnov test
#' @return boolean value if lognormal distributed
#' @examples
#'
#' # Prepare all data to test
#' # Set the seed for reproducibility
#' set.seed(13200323)
#' lognormal_data <- stats::rlnorm(n = 1000, meanlog = 1, sdlog = 1) #lognormal data
#' normal_data <- stats::rnorm(n = 1000, mean = 10, sd = 3) #normal data
#' uniform_data <- stats::runif(10000,min=0,max=10) #uniform data
#' poisson_data <- stats::rpois(1000, lambda = 5) #poisson data
#' gamma_data <- stats::rgamma(1000,shape = 5, rate = 2) #gamma data
#' logis_data <- stats::rlogis(1000, location = 4, scale = 2)#logistic values
#' weibull_data <- stats::rweibull(1000, shape = 4, scale = 2) #weibull data
#' cauchy_data <- stats::rcauchy(1000, location = 4, scale = 2) #cauchy data
#'
#' # EXAMPLE FOR is.lognormal
#'
#' # Test if the data is lognormal
#' is.lognormal(lognormal_data)
#' is.lognormal(normal_data)
#' is.lognormal(uniform_data)
#' is.lognormal(poisson_data)
#' is.lognormal(gamma_data)
#' is.lognormal(logis_data)
#' is.lognormal(logis_data)
#'
#' @export
is.lognormal <- function(values, alpha = 0.05, method = 1) {
  # error check
  stopifnot(method %in% 1:2)

  if (method == 2) {
    # Test for log-normal distribution using Kolmogorov-Smirnov test
    message("Kolmogorov-Smirnov test for log-normal distribution")
    {
      stats::ks.test(log(values), "pnorm", mean = mean(log(values)), sd = sd(log(values)))
    }$p.value >= alpha
  } else {
    # Test for log-normal distribution using Shapiro-Wilk test
    if (method == 1) {
      message("Shapiro-Wilk test for log-normal distribution")
      {
        stats::shapiro.test(log(values))
      }$p.value >= alpha
    } else {
      NULL
    }
  }
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

#' normal_data1 <- rnorm(n = 500, mean = 0, sd = 1)
#' normal_data2 <- rnorm(n = 100, mean = 0, sd = 1)
#' normal_data3 <- rnorm(n = 10, mean = 0, sd = 1)
#'
#' # Test if the data is normal
#' is.normal(normal_data)
#' is.normal(normal_data1)
#' is.normal(normal_data2)
#' is.normal(normal_data3)
#'
#' @export
is.normal <- function(values, alpha = 0.05, method = 1) {
  # error check
  stopifnot(method %in% 1:2)

  if (method == 2) {
    # Test for normal distribution using Kolmogorov-Smirnov test
    message("Kolmogorov-Smirnov test for normal distribution")
    {
      stats::ks.test((values), "pnorm", mean = mean((values)), sd = sd((values)))
    }$p.value >= alpha
  } else {
    # Test for normal distribution using Shapiro-Wilk test
    if (method == 1) {
      message("Shapiro-Wilk test for normal distribution")
      {
        stats::shapiro.test((values))
      }$p.value >= alpha
    } else {
      NULL
    }
  }
}


#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if uniform distributed
#'
#' @examples
#' # EXAMPLES for is.uniform
#'
#' is.uniform(uniform_data)
#'
#' @export
is.uniform <- function(values,alpha = 0.05){
  {stats::ks.test(values, "punif", min(values), max(values))}$p.value >= alpha
}


#' @rdname distribution_check
#' @param values vector of values
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if poisson distributed
#' @examples
#' \dontrun{
#' # EXAMPLE for is.poisson
#' is.poisson(poisson_data) # should be TRUE
#' is.poisson(normal_data) # should be FALSE
#' }
#' @export
is.poisson <-function(values,alpha = 0.05){
  {suppressWarnings(stats::chisq.test(table(values)))}$p.value < alpha
}



#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if gamma distributed
#'
#' @examples
#' \dontrun{
#' # EXAMPLE for is.gamma
#'
#' is.gamma(gamma_data) # check if it is gamma distribution
#' is.gamma(data.norm) # check if it is gamma distribution
#' }
#' @export
is.gamma <- function(values,alpha = 0.05){
  .sr <- fitdistrplus::fitdist(values, "gamma")
  shape <- .sr$estimate['shape']
  rate <- .sr$estimate['rate']
  {stats::ks.test(values, "pgamma", shape = shape, rate = rate)}$p.value >= alpha
}

#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @examples
#' \dontrun{
#' # EXAMPLE for is.logistic
#'
#' is.logistic(logis_data) # test if it is logistic distribution
#' is.logistic(normal_data)
#' }
#' @export
is.logistic <- function(values,alpha = 0.05) {
  .sr <- fitdistrplus::fitdist(values, "logis")
  location <- .sr$estimate['location']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values, "plogis",
                  location = location,
                  scale = scale)}$p.value >= alpha
}



#' @rdname distribution_check
#' @param values vector of values
#' @examples
#'
#' is.weibull(weibull_data) #should return TRUE
#' is.weibull(normal_data) #should return FALSE
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @export
is.weibull <- function(values,alpha = 0.05) {
  .sr <- fitdistrplus::fitdist(values, "weibull")
  shape <- .sr$estimate['shape']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values,"pweibull",shape = shape,scale = scale)}$p.value >= alpha
}






#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if cauchy distributed
#'
#' @examples
#' # EXAMPLES for is.is.cauchy
#'
#' is.cauchy(uniform_data)
#'
#' @export
is.cauchy <- function(values,alpha = 0.05){
  .sr <- fitdistrplus::fitdist(values, "cauchy")
  location <- .sr$estimate['location']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values,"pcauchy",location = location,scale = scale)}$p.value >= alpha
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
# https://www.stat.umn.edu/geyer/old/5101/rlook.html

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




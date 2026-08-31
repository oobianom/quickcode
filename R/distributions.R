#' Check if a data fits the distribution
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
#' # Set global alpha for testing significance
#' setDisAlpha(alpha = 0.05)
#'
#' # Prepare all data to test
#' # Set the seed for reproducibility
#' set.seed(13200323)
#' lognormal_data <- stats::rlnorm(n = 4000, meanlog = 1, sdlog = 1) #lognormal data
#' normal_data <- stats::rnorm(n = 4000, mean = 10, sd = 3) #normal data
#' uniform_data <- stats::runif(4000,min=0,max=10) #uniform data
#' poisson_data <- stats::rpois(4000, lambda = 5) #poisson data
#' gamma_data <- stats::rgamma(4000,shape = 5, rate = 2) #gamma data
#' logis_data <- stats::rlogis(4000, location = 4, scale = 2)#logistic values
#' weibull_data <- stats::rweibull(4000, shape = 4, scale = 2) #weibull data
#' cauchy_data <- stats::rcauchy(4000, location = 8, scale = 5) #cauchy data
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
#' is.lognormal(weibull_data)
#' is.lognormal(cauchy_data)
#' is.lognormal(1:4000)
#'
#' @export
is.lognormal <- function(values, alpha = 0.05, method = 1)!{
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level

  #test for lognormal
  values <- values[values>0]
  # error check
  stopifnot(length(values) > 3, method %in% 1:2)

  if (method == 2) {
    # Test for log-normal distribution using Kolmogorov-Smirnov test
    # message("Kolmogorov-Smirnov test for log-normal distribution")
    {
      stats::ks.test(log(values), "pnorm", mean = mean(log(values)), sd = sd(log(values)))
    }$p.value < alpha
  } else {
    # Test for log-normal distribution using Shapiro-Wilk test
    if (method == 1) {
      # message("Shapiro-Wilk test for log-normal distribution")
      {
        stats::shapiro.test(log(values))
      }$p.value < alpha
    } else {
      TRUE
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
#' # Test if the data fits a normal distribution
#' is.normal(lognormal_data)
#' is.normal(normal_data)
#' is.normal(uniform_data)
#' is.normal(poisson_data)
#' is.normal(gamma_data)
#' is.normal(logis_data)
#' is.normal(weibull_data)
#' is.normal(cauchy_data)
#' is.normal(1:4000)
#'
#' @export
is.normal <- function(values, alpha = 0.05, method = 1)!{
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level
  #test for normality
  # error check
  stopifnot(length(values) > 3, method %in% 1:2)

  if (method == 2) {
    # Test for normal distribution using Kolmogorov-Smirnov test
    # message("Kolmogorov-Smirnov test for normal distribution")
    {
      stats::ks.test(values, "pnorm", mean = mean(values), sd = sd(values))
    }$p.value < alpha
  } else {
    # Test for normal distribution using Shapiro-Wilk test
    if (method == 1) {
      # message("Shapiro-Wilk test for normal distribution")
      {
        stats::shapiro.test(values)
      }$p.value < alpha
    } else {
      TRUE
    }
  }
}


#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if uniform distributed
#'
#' @examples
#' \dontrun{
#' # EXAMPLES for is.uniform
#'
#' # Test if the data fits a uniform distribution
#' is.uniform(lognormal_data)
#' is.uniform(normal_data)
#' is.uniform(uniform_data)
#' is.uniform(poisson_data)
#' is.uniform(gamma_data)
#' is.uniform(logis_data)
#' is.uniform(weibull_data)
#' is.uniform(cauchy_data)
#' is.uniform(1:4000)
#' }
#' @export
is.uniform <- function(values,alpha = 0.05)!{
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level
  stopifnot(length(values) > 3)
  #test for uniform
  {stats::ks.test(values, "punif", min(values), max(values))}$p.value < alpha
}


#' @rdname distribution_check
#' @param values vector of values
#'
#' @param alpha significance level to test p-value against
#' @return boolean value if poisson distributed
#' @examples
#' \dontrun{
#' # EXAMPLE for is.poisson
#'
#' # Test if the data fits a poisson distribution
#' is.poisson(lognormal_data)
#' is.poisson(normal_data)
#' is.poisson(uniform_data)
#' is.poisson(poisson_data)
#' is.poisson(gamma_data)
#' is.poisson(logis_data)
#' is.poisson(weibull_data)
#' is.poisson(cauchy_data)
#' is.poisson(1:4000)
#' }
#' @export
is.poisson <-function(values,alpha = 0.05){
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level
  stopifnot(length(values) > 3)
  #test for poisson
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
#' # Test if the data fits a gamma distribution
#' is.gamma(lognormal_data)
#' is.gamma(normal_data)
#' is.gamma(uniform_data)
#' is.gamma(poisson_data)
#' is.gamma(gamma_data)
#' is.gamma(logis_data)
#' is.gamma(weibull_data)
#' is.gamma(cauchy_data)
#' is.gamma(1:4000)
#' }
#' @export
is.gamma <- function(values, alpha = 0.05) {
  # override alpha if global significance level set
  if (not.null(options()$qc.sig.alpha.level)) {
    alpha <- options()$qc.sig.alpha.level
  }
  stopifnot(length(values) > 3)
  # test for gamma
  tryCatch(
    {
      .sr <- fitdistrplus::fitdist(values, "gamma")
      shape <- .sr$estimate["shape"]
      rate <- .sr$estimate["rate"]
      {
        stats::ks.test(values, "pgamma", shape = shape, rate = rate)
      }$p.value >= alpha
    },
    error = function(msg) {
      return(FALSE)
    }
  )
}

#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @examples
#' \dontrun{
#' # EXAMPLE for is.logistic
#'
#' # Test if the data fits a logistic distribution
#' is.logistic(lognormal_data)
#' is.logistic(normal_data)
#' is.logistic(uniform_data)
#' is.logistic(poisson_data)
#' is.logistic(gamma_data)
#' is.logistic(logis_data)
#' is.logistic(weibull_data)
#' is.logistic(cauchy_data)
#' is.logistic(1:4000)
#' }
#' @export
is.logistic <- function(values,alpha = 0.05) {
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level
  #test for logistic
  stopifnot(length(values) > 3)
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
#' \dontrun{
#' # Test if the data fits a weibull distribution
#' is.weibull(lognormal_data)
#' is.weibull(normal_data)
#' is.weibull(uniform_data)
#' is.weibull(poisson_data)
#' is.weibull(gamma_data)
#' is.weibull(logis_data)
#' is.weibull(weibull_data)
#' is.weibull(cauchy_data)
#' is.weibull(1:4000)
#' }
#' @param alpha significance level to test p-value against
#' @return boolean value if logistic distributed
#' @export
is.weibull <- function(values, alpha = 0.05) {
  # override alpha if global significance level set
  if (not.null(options()$qc.sig.alpha.level)) {
    alpha <- options()$qc.sig.alpha.level
  }
  values[values<=0] = 0
  stopifnot(length(values) > 3)
  # test for weibull
  tryCatch(
    {
      .sr <- fitdistrplus::fitdist(values, "weibull")
      shape <- .sr$estimate["shape"]
      scale <- .sr$estimate["scale"]
      {
        stats::ks.test(values, "pweibull", shape = shape, scale = scale)
      }$p.value >= alpha
    },
    error = function(msg) {
      return(FALSE)
    }
  )
}






#' @rdname distribution_check
#' @param values vector of values
#' @param alpha significance level to test p-value against
#' @return boolean value if cauchy distributed
#'
#' @examples
#' \dontrun{
#' # EXAMPLES for is.cauchy
#'
#' # Test if the data fits a cauchy distribution
#' is.cauchy(lognormal_data)
#' is.cauchy(normal_data)
#' is.cauchy(uniform_data)
#' is.cauchy(poisson_data)
#' is.cauchy(gamma_data)
#' is.cauchy(logis_data)
#' is.cauchy(weibull_data)
#' is.cauchy(cauchy_data)
#' is.cauchy(1:4000)
#' }
#' @export
is.cauchy <- function(values,alpha = 0.05){
  #override alpha if global significance level set
  if(not.null(options()$qc.sig.alpha.level))
    alpha = options()$qc.sig.alpha.level
  #test for cachy
  stopifnot(length(values) > 3)
  .sr <- fitdistrplus::fitdist(values, "cauchy")
  location <- .sr$estimate['location']
  scale <- .sr$estimate['scale']
  {stats::ks.test(values,"pcauchy",location = location,scale = scale)}$p.value >= alpha
}

#' @rdname distribution_check
#' @param alpha significance level to test p-value against
#' @return setDisAlpha sets global significance level for testing of distribution
#'
#' @examples
#' \dontrun{
#' # set global distribution alpha
#'
#' # default setting
#' setDisAlpha()
#'
#' # set to 0.001
#' setDisAlpha(alpha = 0.01)
#' }
#' @export
setDisAlpha <- function(alpha = 0.05){
  stopifnot(is.numeric(alpha) == TRUE)
  options(qc.sig.alpha.level = alpha)
}
#' @rdname distribution_check
#' @return unsetDisAlpha removes global significance level for testing of distribution
#'
#' @examples
#' \dontrun{
#' # unset global distribution alpha
#'
#' unsetDisAlpha()
#' }
#' @export
unsetDisAlpha <- function(){
  options(qc.sig.alpha.level = NULL)
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




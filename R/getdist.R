#' @note
#' getDistribution() checks if data fits multiple distributions
#' @rdname distribution_check
#' @param data the data to check against distributions
#' @param alpha p-value alpha level
#' @export
#'

getDistribution <- function(data, alpha = 0.05) {
  setDisAlpha(alpha = alpha)
  dists <- list(
    lognormal = is.lognormal(data, alpha = alpha, method = 1),
    normal = is.normal(data, alpha = alpha, method = 1),
    uniform = is.uniform(data, alpha = alpha),
    poisson = is.poisson(data, alpha = alpha),
    gamma = is.gamma(data, alpha = alpha),
    logistic = is.logistic(data, alpha = alpha),
    weibull = is.weibull(data, alpha = alpha),
    cauchy = is.cauchy(data, alpha = alpha)
  )
  unsetDisAlpha()

  dists
}
